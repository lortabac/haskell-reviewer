import type { JSX } from "solid-js";
import { PageTitle } from "./PageTitle";

export function Home(): JSX.Element {
  return (
    <>
      <PageTitle text="Home" />
      <h1 tabindex="-1">Home</h1>
      <p>Pick a module or file in the sidebar.</p>
    </>
  );
}
