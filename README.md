# roc-url

A Roc-way of working with URLs. This package is split into two main types: a `Url` which
is great for making web requests, and a `Uri` which is great for losslessly tinkering
with a URL/URI.

- Use the `Url` module for typical web URLs (HTTP/HTTPS). You can parse them strictly
  using `Url.parse` or more like a browser would using `Url.parse_leniently`. The latter
  accepts "example.com" as "https://example.com" while the former would complain
  about a missing scheme, for example.

  `Url` normalizes URLs so that you can compare them. `Url.parse_leniently`
  goes further by, for example, percent-encoding on parse as a browser would.

- Use the `Uri` module for everything else (`mailto:`, `postgres://`, `wss://`
  etc.), relative references, or when you don't want normalization (e.g. for
  URL signing where the exact bytes matter). Generally follows RFC 3986 for a
  URI reference.

Convert back and forth using `Url.from_uri` and `Url.to_uri`.

## Example

```roc
app [main!] {
    pf: platform "https://github.com/niclas-ahden/basic-cli/releases/download/0.22.1/DobkAk7zNyqAgqh2Riaj5c5DtWtKhd5iVYE5RFa6izcd.tar.zst",
    url: "",
}

import pf.Stdout
import url.Url
import url.Uri

main! = |_args| {
    # Url.parse is strict, and Url.parse_leniently parses more like a browser would.
    # Neither of them accepts anything other than HTTP/HTTPS, though. Url.parse_lentiently
    # assumes HTTPS when there's no scheme.

    # Rejected by Url.parse, accepted by Url.parse_leniently:
    for url in ["example.com", "  example.com/spaces-trimmed  ", "localhost:3000/spaces encoded/"] {
        line =
            match Url.parse_leniently(url) {
                Ok(u) => "${url} -> ${Url.to_str(u)}"
                Err(e) => "${url} -> ${Str.inspect(e)}"
            }

        Stdout.line!(line)

        # example.com -> https://example.com/
        #   example.com/spaces-trimmed     -> https://example.com/spaces-trimmed
        # localhost:3000/spaces encoded/ -> https://localhost:3000/spaces%20encoded/
    }

    # Use builders with either Uri or Url. Segments and params are percent-encoded for you.
    endpoint =
        Uri.parse("https://api.example.com")
            .append_path(["v1", "users", "örjan.lax@example.com", "posts"])
            .append_param("sort", "newest first")

    Stdout.line!(Uri.to_str(endpoint))?
    # https://api.example.com/v1/users/%C3%B6rjan.lax%40example.com/posts?sort=newest%20first

    Ok({})
}
```

See more uses in `examples/`.

## `Url`: HTTP/HTTPS

`Url.parse` is strict and `Url.parse_leniently` reads text the way a
browser's address bar would. Use the strict one for strings that are already
supposed to be URLs (config values, API responses, HTML attributes), where a
mistake should surface as an error. Use the lenient one for URL-ish text from
the wild, such as user input.

```roc
import url.Url

# Strict: the RFC reading, whitelisted to the web
Url.parse("https://api.example.com/v1") # Ok(...)
Url.parse("localhost:3000/x")           # Err(NotWebScheme("localhost"))
Url.parse("example.com")                # Err(NoScheme)
Url.parse("mailto:hi@example.com")      # Err(NotWebScheme("mailto"))

# Lenient: what the typist meant
Url.parse_leniently("example.com")         # Ok, https://example.com/
Url.parse_leniently("localhost:3000/x")    # Ok, https://localhost:3000/x
Url.parse_leniently("  sword.shop  ")      # Ok, https://sword.shop/
Url.parse_leniently("example.com/a b")     # Ok, https://example.com/a%20b
Url.parse_leniently("javascript:alert(1)") # still Err(NotWebScheme("javascript"))
```

Lenient also encodes bytes no URI can contain (a typed space becomes
`%20`), the way an address bar does. Strict parse never adds escapes that
weren't written, so a raw space passes through it as-is.

A `Url` is normalized at construction: scheme and host are lowercased, a
default port is dropped, an empty path becomes `/`, dot segments are
resolved, and percent escapes are put in canonical form (uppercase hex,
escapes of unreserved characters decoded). That is RFC 3986
normalization, so equality works like so:

```roc
Url.parse("HTTP://Example.COM:80/a/../b") == Url.parse("http://example.com/b") # True
Url.parse("https://example.com/%7Eann%2Fx") == Url.parse("https://example.com/~ann%2Fx") # True
```

Every `Url` passes some basic checks (but is not fully validated), so the
accessors are plain: `Url.host` is a plain `Str`, `Url.port` is a plain `U16` (the
scheme default when none was written), and `Url.origin` never includes userinfo, so it is
always safe to log.

`Url.join` follows a link the way a browser does:

```roc
base = Url.parse("https://example.com/a/b/c") # Ok(...)

Url.join(base, "../g")     # Ok, https://example.com/a/g
Url.join(base, "//cdn/x")  # Ok, https://cdn/x
Url.join(base, "mailto:x") # Err(NotWebScheme("mailto"))
```

Writers cover every component, and each one keeps the guarantees: a
written `Url` is exactly what parsing its `to_str` would give. Segments
and params are percent-encoded for you. Most writers are total. The two
calls that could break a check return an error instead:

```roc
u = Url.parse("https://api.example.com/v1") # Ok(...)

Url.append_path(u, ["users", "a@b.se"]) # Ok, https://api.example.com/v1/users/a%40b.se
Url.with_param(u, "page", "2")          # https://api.example.com/v1?page=2
Url.with_scheme(u, Http)                # http://api.example.com/v1
Url.append_path(u, [".."])              # Err(DotSegment("..")), an append cannot climb
Url.with_host(u, "")                    # Err(EmptyHost), a web URL needs a host
```

## `Uri`

Most URL libraries hand you one big `Err` when any part of the input is off,
and the good parts vanish with it. `Uri.parse` doesn't fail wholesale. You
always get a `Uri` back, and each component answers for itself when you read
it:

```roc
import url.Uri

u = Uri.parse("https://example.com:banana/products")

Uri.require_host(u) # Ok("example.com")
Uri.path(u)         # "/products"
Uri.port(u)         # Err(PortParseErr("banana"))
Uri.to_str(u)       # "https://example.com:banana/products"
```

The port is nonsense so reading it gives you an error, but it's still kept intact.
`Uri.to_str` prints the URI back exactly as it was written.

When you do want all-or-nothing, use `to_try`:

```roc
u = Uri.parse(input).to_try()? # Err(PortParseErr("banana")) for the URI above
```

Absence is not an error, so a relative reference like `/docs` passes
`to_try`. If you also need a host or a scheme to be present, use
`require_host` and `require_scheme`. For a runnable version of this, see
[`examples/to_try.roc`](examples/to_try.roc).

The same guarantee runs in the other direction. Whatever you build with the
writers (`append_path`, `append_param`, `with_host`, and friends) parses back
to the same components. Segments and params are percent-encoded for you, so
nothing sneaks into the URI's structure, and the corner cases you'd rather
not think about (empty hosts, `//` shenanigans in paths) are kept, labelled,
and round-tripped. `Uri.resolve` does full RFC 3986 reference resolution,
and `Uri.percent_encode` / `Uri.percent_decode` are exported for standalone
use.

Crossing between the layers is two functions: `Url.to_uri` gives you the
lossless machinery for a checked value (an arbitrary scheme, a literal dot
segment, an empty host as a labelled fact), and `Url.from_uri` checks any
`Uri` back into web space.

## No IDNA or validation

I've chosen not to ship these features at the moment since they both require a great deal
of decision-making which may differ for each use-case. You know best what works for your
use-case and therefore `roc-url` shouldn't be overly prescriptive. This decision may change
in the future.
