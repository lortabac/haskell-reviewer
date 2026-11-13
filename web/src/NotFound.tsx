import { useLocation } from "@solidjs/router";
import { type JSX, Show } from "solid-js";
import { queryFromUrl } from "./docs/urls";
import { PageTitle } from "./PageTitle";
import { SearchBox } from "./search/SearchBox";

// "Not found in this snapshot" (SPEC §4.1), with a search box. query:
// what to put in it; else taken from the URL. note: instead of the
// usual text.
export function NotFound(props: {
  query?: string;
  note?: string;
}): JSX.Element {
  const location = useLocation();
  return (
    <>
      <PageTitle text="Not found" />
      <h1 tabindex="-1">Not found in this snapshot</h1>
      <p>{props.note ?? "This page does not exist, or not anymore."}</p>
      {/* Keyed on a new object per URL (the text can be ""): a new URL
          gives a new box with its own text. */}
      <Show
        when={{ text: props.query ?? queryFromUrl(location.pathname) }}
        keyed
      >
        {(q) => <SearchBox initialQuery={q.text} placement="page" />}
      </Show>
    </>
  );
}
