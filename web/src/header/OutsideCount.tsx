// The header's count of changed files outside ROOT (D-338): a link to
// Home, where they are listed, with the first paths in a tip. A link
// is a Tab stop, so the keyboard gets the tip too (ui/tips.ts). Hidden
// when the count is 0 or not known, and while api/diff-files loads.
// On Home it has aria-current="page" (the router's A, with end).
import { A } from "@solidjs/router";
import { type JSX, Show } from "solid-js";
import { useDiffFiles } from "../diff/store";
import { outsideCount, outsideTip } from "../home/summary";

export function OutsideCount(): JSX.Element {
  const diff = useDiffFiles();
  const outside = () => {
    const o = diff.files()?.outside;
    return o !== undefined && o !== null && o.total > 0 ? o : undefined;
  };
  return (
    <Show when={outside()}>
      {(o) => (
        <A href="/" end class="outside-count" data-tip={outsideTip(o())}>
          {outsideCount(o())}
          <span class="sr-only"> changed files</span> outside ROOT
        </A>
      )}
    </Show>
  );
}
