// f, made once per key object (a WeakMap: dropped with the key).
export function memoBy<K extends object, V>(f: (key: K) => V): (key: K) => V {
  const cache = new WeakMap<K, V>();
  return (key) => {
    if (cache.has(key)) return cache.get(key) as V;
    const v = f(key);
    cache.set(key, v);
    return v;
  };
}
