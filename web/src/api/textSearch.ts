// GET api/text-search/<side>?q=&limit=: the lines of the text files
// that have the text (D-307). Shape: see encodeTextSearch in
// HaskellReviewer.Server.Wire.
import {
  DecodeError,
  array,
  bool,
  field,
  nat,
  object,
  positive,
  str,
} from "./decode";

// A hit on a line: start and end columns of the whole line (code
// points, from 1, end exclusive).
export type TextSpan = [number, number];

export type TextHit = {
  path: string;
  // From 1.
  line: number;
  // The line, cut to 240 code points around the first hit; lineCol is
  // the column of its first code point.
  lineCol: number;
  text: string;
  // At most 20.
  matches: TextSpan[];
};

export type TextSkipped = {
  binary: number;
  tooLarge: number;
  unreadable: number;
  overBudget: number;
};

export type TextSearch = {
  generation: number;
  // q as sent.
  query: string;
  // Always false for now.
  partial: boolean;
  // The deadline came: some files weren't searched.
  stopped: boolean;
  // Every matching line; results has the first ones (limit, 1000 at
  // most).
  total: number;
  // Files with a hit.
  files: number;
  // Files not searched, by why.
  skipped: TextSkipped;
  results: TextHit[];
};

function decodeSpan(value: unknown, path: string): TextSpan {
  const pair = array(positive)(value, path);
  const [from, to] = pair;
  if (
    pair.length !== 2 ||
    from === undefined ||
    to === undefined ||
    to <= from
  ) {
    throw new DecodeError(path, "[from, to] with from < to");
  }
  return [from, to];
}

function decodeHit(value: unknown, path: string): TextHit {
  const o = object(value, path);
  return {
    path: field(o, "path", path, str),
    line: field(o, "line", path, positive),
    lineCol: field(o, "lineCol", path, positive),
    text: field(o, "text", path, str),
    matches: field(o, "matches", path, array(decodeSpan)),
  };
}

function decodeSkipped(value: unknown, path: string): TextSkipped {
  const o = object(value, path);
  return {
    binary: field(o, "binary", path, nat),
    tooLarge: field(o, "tooLarge", path, nat),
    unreadable: field(o, "unreadable", path, nat),
    overBudget: field(o, "overBudget", path, nat),
  };
}

export function decodeTextSearch(value: unknown, path = "$"): TextSearch {
  const o = object(value, path);
  const answer = {
    generation: field(o, "generation", path, nat),
    query: field(o, "query", path, str),
    partial: field(o, "partial", path, bool),
    stopped: field(o, "stopped", path, bool),
    total: field(o, "total", path, nat),
    files: field(o, "files", path, nat),
    skipped: field(o, "skipped", path, decodeSkipped),
    results: field(o, "results", path, array(decodeHit)),
  };
  if (answer.results.length > answer.total) {
    throw new DecodeError(path, "results.length <= total");
  }
  if (answer.files > answer.total) {
    throw new DecodeError(path, "files <= total");
  }
  return answer;
}

// The route for a query, relative to the token, for getJson. No limit:
// the server's (1000).
export function textSearchUrl(q: string, limit?: number): string {
  const url = `/api/text-search/tree?q=${encodeURIComponent(q)}`;
  return limit === undefined ? url : `${url}&limit=${limit}`;
}
