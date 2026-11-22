// Keys of the disclosure groups in the module tree.
// JSON, since paths and component names can hold ":".
export const packageKey = (cabalFile: string): string =>
  JSON.stringify(["p", cabalFile]);
export const componentKey = (cabalFile: string, comp: string): string =>
  JSON.stringify(["c", cabalFile, comp]);
export const otherKey = "other";
