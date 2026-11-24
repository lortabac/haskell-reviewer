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

// ] and [ (D-350).
export const fileStepTexts = {
  prevFile: "Previous file",
  nextFile: "Next file",
  noMoreFiles: "No more changed files.",
  noFiles: "No changed files.",
  elsewhere: "Changed files: from the diff, source or docs view.",
};
