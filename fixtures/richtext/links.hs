-- | Links and what could be unsafe.
module Links (x) where

-- | Raw HTML is text: <b>bold</b> <script>alert(1)</script>
-- <img src=x onerror=alert(1)> &lt;i&gt;
--
-- Images are links: <<http://img.example/a.png>>, <<https://img.example/b.png a title>>,
-- ![alt text](http://img.example/c.png).
--
-- Unsafe images are text: <<a.png>>, <<data:image/png;base64,AAAA t>>.
--
-- Links keep the full URL: <https://example.com/a/b?c=d&e=f#g a label>,
-- [markdown /label/](http://example.com/x), https://example.com/auto?q=1.
--
-- Only http and https: <javascript:alert(1) click>, [x](javascript:alert(1)),
-- <mailto:a@b.example>, <Other.html>, ftp://example.com/f, <//evil.example/x>.
x :: Int
x = 1
