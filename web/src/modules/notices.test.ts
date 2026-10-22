import { describe, expect, it } from "vitest";
import type { ModuleEntry, ModuleSource } from "../api/modules";
import {
  bannerText,
  moduleMarker,
  noticeText,
  reasonText,
  sourceMarker,
  warningsTitle,
} from "./notices";

describe("noticeText", () => {
  it("stack", () => {
    expect(noticeText({ kind: "stack" })).toBe(
      "stack.yaml isn't supported: without a cabal.project, the .cabal files in ROOT are used. Commit a cabal.project to choose the packages.",
    );
  });

  it("implicit", () => {
    expect(noticeText({ kind: "implicit" })).toBe(
      "No cabal.project or .cabal file: ROOT is read as one implicit package, with module names from file paths (GHC2021).",
    );
  });

  it("hpack in ROOT: . is ROOT, read as the implicit package", () => {
    expect(noticeText({ kind: "hpack", dirs: ["."] })).toBe(
      "Hpack isn't supported: package.yaml without a .cabal file in ROOT. Run hpack and commit the .cabal file; until then ROOT is read as an implicit package.",
    );
  });

  it("hpack, one dir", () => {
    expect(noticeText({ kind: "hpack", dirs: ["hp"] })).toBe(
      "Hpack isn't supported: package.yaml without a .cabal file in hp. Run hpack and commit the .cabal file; until then it's a plain directory.",
    );
  });

  it("hpack, several dirs", () => {
    expect(noticeText({ kind: "hpack", dirs: ["a", "b/c"] })).toBe(
      "Hpack isn't supported: package.yaml without a .cabal file in a, b/c. Run hpack and commit the .cabal file; until then these are plain directories.",
    );
  });
});

describe("markers", () => {
  it.each([
    ["autogen", "autogen"],
    ["missing", "file missing"],
    ["outside", "outside ROOT"],
    ["symlink", "symlink"],
  ] as const)("reason %s", (reason, text) => {
    expect(reasonText(reason)).toBe(text);
    expect(sourceMarker({ kind: "no-source", reason })).toBe(
      `listed, no source (${text})`,
    );
  });

  it("sources", () => {
    expect(sourceMarker({ kind: "module", path: "A.hs" })).toBeUndefined();
    expect(sourceMarker({ kind: "generated", path: "A.x", tool: "alex" })).toBe(
      "generated (alex)",
    );
    expect(
      sourceMarker({ kind: "preprocessed", path: "A.hs", tool: "hspec" }),
    ).toBe("preprocessed (hspec)");
  });

  const entry = (
    role: ModuleEntry["role"],
    source: ModuleSource,
  ): ModuleEntry => ({ name: "A", id: "p:lib:A", ref: "A", role, source });

  it("module markers", () => {
    expect(
      moduleMarker(entry("exposed", { kind: "module", path: "A.hs" })),
    ).toBeUndefined();
    expect(
      moduleMarker(entry("signature", { kind: "module", path: "A.hsig" })),
    ).toBe("signature");
    expect(
      moduleMarker(
        entry("signature", { kind: "no-source", reason: "missing" }),
      ),
    ).toBe("signature, listed, no source (file missing)");
  });
});

describe("banners", () => {
  it("generated, preprocessed, none", () => {
    expect(bannerText({ kind: "generated", path: "A.y", tool: "happy" })).toBe(
      "Generated from this file by happy: shown as text, not indexed.",
    );
    expect(
      bannerText({
        kind: "preprocessed",
        path: "A.hs",
        tool: "hspec-discover",
      }),
    ).toBe("Preprocessed by hspec-discover: shown as text, not indexed.");
    expect(bannerText({ kind: "module", path: "A.hs" })).toBeUndefined();
  });

  it("warnings title", () => {
    expect(warningsTitle(1)).toBe("1 project warning");
    expect(warningsTitle(3)).toBe("3 project warnings");
  });
});
