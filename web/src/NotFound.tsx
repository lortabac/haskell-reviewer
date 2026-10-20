import type { JSX } from "solid-js";

export function NotFound(): JSX.Element {
  return (
    <>
      <h1>Not found in this snapshot</h1>
      <p>This page does not exist, or not anymore.</p>
    </>
  );
}
