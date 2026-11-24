// View on base / head in the header (diff mode, D-349). One item, on
// its own, so it is easy to move. Off: aria-disabled but focusable,
// the reason in a tip and said on use.
import { useResolvedPath } from "@solidjs/router";
import { createUniqueId, type JSX, Match, Switch } from "solid-js";
import { withBase } from "../files/paths";
import { useSession } from "../session";
import { type SideTarget, useKeys } from "./KeysProvider";
import { plainClick } from "./plainClick";

type Kind<K> = Extract<SideTarget, { kind: K }>;

export function SideSwitch(): JSX.Element {
  const keys = useKeys();
  const session = useSession();
  const base = useResolvedPath(() => "/");
  const id = createUniqueId();
  const target = () =>
    session.mode === "diff" ? keys?.sideTarget()?.() : undefined;
  const go = (): Kind<"go"> | undefined => {
    const t = target();
    return t?.kind === "go" ? t : undefined;
  };
  const off = (): Kind<"off"> | undefined => {
    const t = target();
    return t?.kind === "off" ? t : undefined;
  };
  const label = (side: string) => `View on ${side}`;
  // Native: before the router's own click handler.
  const onClick = (e: MouseEvent) => {
    if (plainClick(e)) keys?.goSide();
  };
  return (
    <Switch>
      <Match when={go()}>
        {(t) => (
          <a
            class="side-switch"
            href={withBase(base(), t().top)}
            on:click={onClick}
          >
            {label(t().side)}
          </a>
        )}
      </Match>
      <Match when={off()}>
        {(t) => (
          <>
            <button
              type="button"
              class="side-switch side-switch-off"
              aria-disabled="true"
              aria-describedby={id}
              data-tip={t().why}
              onClick={() => keys?.goSide()}
            >
              {label(t().side)}
            </button>
            {/* Hidden: only the description, not read twice. */}
            <span id={id} hidden>
              {t().why}
            </span>
          </>
        )}
      </Match>
    </Switch>
  );
}
