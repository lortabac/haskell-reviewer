// The app lives under /<token>/. The token is the first path segment.
export function basePath(pathname: string): string {
  return "/" + (pathname.split("/")[1] ?? "");
}
