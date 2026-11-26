// After a reload the providers and the header search fetch again, in
// place (D-354).
import { createRoot } from "solid-js";
import { render } from "solid-js/web";
import { afterEach, describe, expect, it, vi } from "vitest";
import { fixture, stubFetch } from "../../test/fixtures";
import { worktreeSession } from "../../test/sessions";
import { searchUrl } from "../api/search";
import type { Session } from "../api/session";
import {
  type DiffFilesApi,
  DiffFilesProvider,
  useDiffFiles,
} from "../diff/store";
import { type Modules, ModulesProvider, useModules } from "../modules/store";
import { createSearch, type SearchState } from "../search/createSearch";
import { SessionContext, SideContext } from "../session";
import {
  createGenerationState,
  GenerationContext,
  type GenerationState,
} from "./generation";

let dispose: (() => void) | undefined;
afterEach(() => {
  dispose?.();
  dispose = undefined;
  document.body.replaceChildren();
  vi.unstubAllGlobals();
});

function mount(session: Session, probe: () => void): GenerationState {
  const gen = createRoot(() => createGenerationState());
  const root = document.createElement("div");
  document.body.append(root);
  dispose = render(
    () => (
      <SessionContext.Provider value={session}>
        <SideContext.Provider value={() => "head"}>
          <GenerationContext.Provider value={gen}>
            <DiffFilesProvider>
              <ModulesProvider>{(probe(), null)}</ModulesProvider>
            </DiffFilesProvider>
          </GenerationContext.Provider>
        </SideContext.Provider>
      </SessionContext.Provider>
    ),
    root,
  );
  return gen;
}

describe("providers on a reload", () => {
  it("diff-files and modules: old data kept, loading, then the new", async () => {
    const calls = stubFetch({
      "/tok/api/diff-files": [200, fixture("diff-files-worktree")],
      "/tok/api/modules/head": [200, fixture("modules-diff")],
      "/tok/api/modules/base": [200, fixture("modules-diff")],
    });
    history.replaceState(null, "", "/tok/");
    let diff!: DiffFilesApi;
    let modules!: Modules;
    const gen = mount(worktreeSession, () => {
      diff = useDiffFiles();
      modules = useModules();
    });
    await vi.waitFor(() => {
      expect(diff.files()).toBeDefined();
      expect(modules.other()).toBeDefined();
    });
    const count = (u: string) =>
      calls.mock.calls.filter(([x]) => x === u).length;
    expect(count("/tok/api/diff-files")).toBe(1);
    expect(count("/tok/api/modules/head")).toBe(1);
    expect(count("/tok/api/modules/base")).toBe(1);
    // The baseline came from the answers.
    expect(gen.current()).toBe(1);
    gen.seen(2);
    expect(diff.loading()).toBe(true);
    expect(diff.files()).toBeDefined();
    expect(modules.loading()).toBe(true);
    expect(modules.ok()).toBeDefined();
    await vi.waitFor(() => {
      expect(diff.loading()).toBe(false);
      expect(modules.loading()).toBe(false);
      expect(count("/tok/api/modules/base")).toBe(2);
    });
    expect(count("/tok/api/diff-files")).toBe(2);
    expect(count("/tok/api/modules/head")).toBe(2);
  });
});

describe("the header search on a reload", () => {
  it("a complete answer of an older generation is asked again", async () => {
    const url = "/tok" + searchUrl("head", "map");
    const calls = stubFetch({ [url]: [200, fixture("search")] });
    history.replaceState(null, "", "/tok/");
    let s!: SearchState;
    const gen = createRoot(() => createGenerationState());
    const root = document.createElement("div");
    document.body.append(root);
    dispose = render(
      () => (
        <SideContext.Provider value={() => "head"}>
          <GenerationContext.Provider value={gen}>
            {(() => {
              s = createSearch(
                () => "map",
                () => true,
              );
              return null;
            })()}
          </GenerationContext.Provider>
        </SideContext.Provider>
      ),
      root,
    );
    await vi.waitFor(() => expect(s.answeredQuery()).toBe("map"));
    expect(s.partial()).toBe(false);
    gen.seen(1);
    expect(calls.mock.calls).toHaveLength(1);
    gen.seen(2);
    await vi.waitFor(() => expect(calls.mock.calls).toHaveLength(2));
  });
});
