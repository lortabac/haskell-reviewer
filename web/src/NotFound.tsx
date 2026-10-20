import type { JSX } from "solid-js";
import { PageTitle } from "./PageTitle";

export function NotFound(): JSX.Element {
  return (
    <>
      <PageTitle text="Not found" />
      <h1 tabindex="-1">Not found in this snapshot</h1>
      <p>This page does not exist, or not anymore.</p>
    </>
  );
}
