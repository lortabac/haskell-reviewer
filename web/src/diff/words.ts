// Words the diff and source views share (D-342, D-348).
export const stepTexts = {
  prevChange: "Previous change",
  nextChange: "Next change",
  noMore: "No more changes.",
};

export const notDiffMode = "Not in diff mode.";

// A file or module on one side only (D-349).
export function onlyOn(side: string): string {
  return `Only on ${side}.`;
}
