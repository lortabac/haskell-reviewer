// Test-only HTTP proxy in front of a server, so tests can cut the
// connection, refuse it, or point it at a new server. Host is rewritten
// to the backend's, else the server's Host check says 403.
import {
  type ClientRequest,
  createServer,
  request,
  type Server,
} from "node:http";
import type { AddressInfo, Socket } from "node:net";

export interface Proxy {
  port: number;
  // Same shape as Server.url, on the proxy's port.
  url: (token: string) => string;
  // Drop every open connection.
  cut: () => void;
  // Stop listening (connections refused) and cut.
  close: () => Promise<void>;
  retarget: (port: number) => void;
}

export async function startProxy(backend: number): Promise<Proxy> {
  let target = backend;
  const sockets = new Set<Socket>();
  const upstreams = new Set<ClientRequest>();

  const server: Server = createServer((req, res) => {
    const up = request(
      {
        host: "127.0.0.1",
        port: target,
        method: req.method,
        path: req.url,
        headers: { ...req.headers, host: `127.0.0.1:${target}` },
      },
      (upRes) => {
        res.writeHead(upRes.statusCode ?? 502, upRes.headers);
        // Flush headers now: SSE sends little.
        res.flushHeaders();
        upRes.on("error", () => res.destroy());
        upRes.pipe(res);
      },
    );
    upstreams.add(up);
    up.on("close", () => upstreams.delete(up));
    // Backend gone: look like a network error, not an HTTP one.
    up.on("error", () => req.socket.destroy());
    res.on("close", () => up.destroy());
    req.pipe(up);
  });
  server.on("connection", (s) => {
    sockets.add(s);
    s.on("close", () => sockets.delete(s));
  });

  const listen = (port: number): Promise<void> =>
    new Promise((resolve, reject) => {
      server.once("error", reject);
      server.listen(port, "127.0.0.1", () => {
        server.off("error", reject);
        resolve();
      });
    });

  await listen(0);
  const port = (server.address() as AddressInfo).port;

  const cut = (): void => {
    for (const u of upstreams) u.destroy();
    for (const s of sockets) s.destroy();
  };

  return {
    port,
    url: (token) => `http://127.0.0.1:${port}/${token}/`,
    cut,
    close: () =>
      new Promise((resolve) => {
        server.close(() => resolve());
        cut();
      }),
    retarget: (p) => {
      target = p;
    },
  };
}
