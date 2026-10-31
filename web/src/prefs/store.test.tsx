import { render } from "solid-js/web";
import { afterEach, describe, expect, it, vi } from "vitest";
import { stubFetch } from "../../test/fixtures";
import { defaultPrefs, type Prefs } from "../api/prefs";
import { type PrefsApi, PrefsProvider, PrefsStatus, usePrefs } from "./store";

let dispose: (() => void) | undefined;
let api: PrefsApi | undefined;

function mount(pending?: Promise<Prefs>): void {
  window.history.replaceState(null, "", "/tok");
  const root = document.createElement("div");
  document.body.append(root);
  dispose = render(
    () => (
      <PrefsProvider initial={defaultPrefs} pending={pending}>
        <Capture />
        <PrefsStatus />
      </PrefsProvider>
    ),
    root,
  );
}

function Capture() {
  api = usePrefs();
  return null;
}

afterEach(() => {
  dispose?.();
  dispose = undefined;
  api = undefined;
  document.body.replaceChildren();
  vi.restoreAllMocks();
});

function current(): PrefsApi {
  if (api === undefined) throw new Error("no PrefsApi");
  return api;
}

// A fetch mock whose calls stay pending until resolved by hand, in any
// order (for the races in C2/C3's tests).
function controlledFetch(): {
  bodies: () => string[];
  respond: (i: number, status: number, body: unknown) => void;
} {
  const resolvers: ((r: Response) => void)[] = [];
  const bodies: string[] = [];
  const fn = vi.fn((_input: RequestInfo | URL, init?: RequestInit) => {
    bodies.push(init?.body as string);
    return new Promise<Response>((resolve) => resolvers.push(resolve));
  });
  vi.stubGlobal("fetch", fn);
  return {
    bodies: () => bodies,
    respond: (i, status, body) => {
      resolvers[i]?.(
        new Response(JSON.stringify(body), {
          status,
          headers: { "Content-Type": "application/json" },
        }),
      );
    },
  };
}

const tick = () => new Promise((r) => setTimeout(r, 0));

describe("PrefsProvider", () => {
  it("starts from the initial value", () => {
    mount();
    expect(current().theme()).toBe("system");
    expect(current().exportedOnly()).toBe(false);
    expect(current().sidebar()).toBe("auto");
  });

  it("sets optimistically, before the POST answers", () => {
    const fetch = stubFetch({});
    mount();
    current().setTheme("dark");
    expect(current().theme()).toBe("dark");
    expect(fetch).toHaveBeenCalled();
    const [url, init] = fetch.mock.calls[0] as unknown as [string, RequestInit];
    expect(url).toBe("/tok/api/prefs");
    expect(init.method).toBe("POST");
    expect(init.headers).toMatchObject({ "Content-Type": "application/json" });
    expect(init.mode).toBeUndefined();
    expect(JSON.parse(init.body as string)).toEqual({ theme: "dark" });
  });

  it("a click that doesn't change the value is a no-op: no POST (round 2)", () => {
    const fetch = stubFetch({});
    mount();
    current().setTheme("system"); // already the initial value
    current().setExportedOnly(false); // already false
    current().setSidebar("auto"); // already auto
    expect(fetch).not.toHaveBeenCalled();
  });

  it("a good POST's answer never changes the chosen value (C2)", async () => {
    // Even if the server's answer disagreed (another process's write
    // raced in), what the user picked here stays: no cross-tab sync
    // (D-197), and no second source of truth to go stale from.
    stubFetch({
      "/tok/api/prefs": [
        200,
        { theme: "light", exportedOnly: true, sidebar: "files" },
      ],
    });
    mount();
    current().setExportedOnly(true);
    await vi.waitFor(() => {
      expect(current().status()).toBeUndefined();
    });
    expect(current().theme()).toBe("system");
    expect(current().sidebar()).toBe("auto");
  });

  it("a stale POST answer doesn't undo a newer pick, nor its status (C2)", async () => {
    const ctl = controlledFetch();
    mount();
    current().setTheme("dark"); // request 0
    current().setTheme("light"); // request 1, the latest for "theme"
    expect(current().theme()).toBe("light");
    // The newer request settles first, successfully.
    ctl.respond(1, 200, {
      theme: "light",
      exportedOnly: false,
      sidebar: "auto",
    });
    await vi.waitFor(() => {
      expect(current().status()).toBeUndefined();
    });
    // The older request's answer arrives late, and fails.
    ctl.respond(0, 500, "too late");
    await tick();
    await tick();
    // Neither the value nor the status moved: request 0 is stale.
    expect(current().theme()).toBe("light");
    expect(current().status()).toBeUndefined();
    expect(ctl.bodies()).toEqual([
      JSON.stringify({ theme: "dark" }),
      JSON.stringify({ theme: "light" }),
    ]);
  });

  it("a failed key's status isn't hidden by a different key's later success (round 2)", async () => {
    const ctl = controlledFetch();
    mount();
    current().setTheme("dark"); // request 0, key "theme"
    current().setExportedOnly(true); // request 1, key "exportedOnly"
    ctl.respond(0, 500, "theme save failed");
    await vi.waitFor(() => {
      expect(current().status()).toBe("Couldn't save this preference.");
    });
    ctl.respond(1, 200, {
      theme: "dark",
      exportedOnly: true,
      sidebar: "auto",
    });
    // exportedOnly's own latest save succeeded, but theme's latest
    // save is still failed: the status must stay (per-key failed
    // flags, not one shared counter).
    await tick();
    await tick();
    await vi.waitFor(() => {
      expect(current().status()).toBe("Couldn't save this preference.");
    });
  });

  it("a late GET fills only the keys the user hasn't touched yet (C3)", async () => {
    stubFetch({});
    let resolvePending: ((p: Prefs) => void) | undefined;
    const pending = new Promise<Prefs>((resolve) => {
      resolvePending = resolve;
    });
    mount(pending);
    // Touched before the late GET arrives: must not be overridden.
    current().setExportedOnly(true);
    resolvePending?.({ theme: "dark", exportedOnly: false, sidebar: "files" });
    await vi.waitFor(() => {
      expect(current().theme()).toBe("dark");
      expect(current().sidebar()).toBe("files");
    });
    expect(current().exportedOnly()).toBe(true);
  });

  it("failure keeps the chosen value and shows a polite status", async () => {
    stubFetch({ "/tok/api/prefs": [500, "cannot save preferences"] });
    mount();
    current().setTheme("dark");
    await vi.waitFor(() => {
      expect(current().status()).toBe("Couldn't save this preference.");
    });
    // Kept: not reverted.
    expect(current().theme()).toBe("dark");
  });

  it("the status live region is mounted from the start, text only changes (C4)", async () => {
    stubFetch({ "/tok/api/prefs": [500, "cannot save preferences"] });
    mount();
    const region = document.querySelector(".prefs-status");
    expect(region).not.toBeNull();
    expect(region?.getAttribute("aria-live")).toBe("polite");
    expect(region?.textContent).toBe("");
    current().setSidebar("files");
    await vi.waitFor(() => {
      expect(region?.textContent).toBe("Couldn't save this preference.");
    });
    // Still the same element: never unmounted to show the message.
    expect(document.querySelector(".prefs-status")).toBe(region);
  });

  it("a repeated failure clears the text, then re-sets it after a real delay (round 2)", async () => {
    stubFetch({ "/tok/api/prefs": [500, "cannot save preferences"] });
    mount();
    const region = document.querySelector(".prefs-status");
    if (region === null) throw new Error("no .prefs-status");
    current().setTheme("dark");
    await vi.waitFor(() => {
      expect(region.textContent).toBe("Couldn't save this preference.");
    });
    const records: string[] = [];
    const obs = new MutationObserver((muts) => {
      for (const m of muts) records.push(m.target.textContent ?? "");
    });
    obs.observe(region, {
      characterData: true,
      childList: true,
      subtree: true,
    });
    current().setTheme("light"); // another failure, the same message
    // Cleared well before the message would reappear: a same-task
    // clear-then-set isn't enough (Chromium doesn't diff it), so the
    // text must sit empty for a real stretch, not an instant.
    await vi.waitFor(() => {
      expect(records).toContain("");
    });
    expect(region.textContent).toBe("");
    await vi.waitFor(() => {
      expect(region.textContent).toBe("Couldn't save this preference.");
    });
    obs.disconnect();
    expect(
      records.filter((r) => r === "Couldn't save this preference.").length,
    ).toBeGreaterThanOrEqual(1);
  });
});
