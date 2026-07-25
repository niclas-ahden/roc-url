## A parsed [URL](https://en.wikipedia.org/wiki/URL) (strictly speaking, a URI
## reference as defined in RFC 3986 §4.1).
##
## Create one with [Uri.parse], which accepts any string and never fails, or
## build one from scratch starting at [Uri.empty]. Ask [Uri.to_try] whether
## every component parsed cleanly. Read components with the accessors
## ([Uri.host], [Uri.port], and friends) or the `require_*` extractors. Modify
## with the writers ([Uri.append_path], [Uri.with_host], and friends) and turn
## the whole thing back into a [Str] with [Uri.to_str].
Uri := [
	Uri(
		{
			scheme : [Scheme(Str), SchemeRelative, NoScheme],
			userinfo : [Userinfo(Str), NoUserinfo],
			host : [Host(Str), EmptyHost, NoHost],
			port : [Port(U16), NoPort, PortParseErr(Str)],
			path : Str,
			query : [Query(Str), EmptyQuery, NoQuery],
			fragment : [Fragment(Str), EmptyFragment, NoFragment],
		},
	),
].{

	## Structural equality, derived: two [Uri]s are equal when every
	## component is, byte for byte. No normalization, so `Uri.parse` of two
	## RFC-equivalent spellings need not be equal (that is [Url]'s job).
	is_eq : _

	## A [Uri] with every component absent and an empty path.
	##
	## The starting point for building a URL from components:
	##
	## ```
	## # Gives Ok("https://api.example.com:8443/v1/users")
	## Try.map_ok(Uri.with_scheme(Uri.empty, "https"), |u|
	##     Uri.to_str(Uri.append_path(Uri.with_port(Uri.with_host(u, "api.example.com"), 8443), ["v1", "users"])))
	## ```
	empty : Uri
	empty = Uri({
		scheme: NoScheme,
		userinfo: NoUserinfo,
		host: NoHost,
		port: NoPort,
		path: "",
		query: NoQuery,
		fragment: NoFragment,
	})

	## Parses a [Str] into a [Uri]. It accepts any string and never fails.
	##
	## Component boundaries follow RFC 3986: the fragment starts at the first `#`,
	## the query at the first `?` (after the fragment is removed), the userinfo
	## ends at the last `@` in the authority. `//` is the only thing that
	## introduces an authority (host, port, userinfo). `scheme:` without `//` is
	## an opaque scheme whose remainder is the path (`mailto:a@b.com`), and
	## scheme-less input without `//` is a relative reference, which is all path.
	##
	## Note the strict RFC reading of scheme-less input: `example.com` is a
	## *path*, not a host, and `localhost:3000/x` is `Scheme("localhost")` with
	## path `3000/x`. Reading those as hosts is a choice your application gets
	## to make. Prepend a scheme (e.g. `https://`) before parsing if your input
	## is user-typed addresses.
	##
	## ```
	## # Gives Host("example.com")
	## Uri.host(Uri.parse("https://example.com/path"))
	##
	## # Gives NoHost since there's no "//" and thus no authority
	## Uri.host(Uri.parse("example.com"))
	## ```
	parse : Str -> Uri
	parse = |input| {
		(parsed_fragment, sans_fragment) = extract_fragment(input)
		(parsed_query, sans_query) = extract_query(sans_fragment)

		if Str.starts_with(sans_query, "//") {
			rest = 
				match Str.split_first(sans_query, "//") {
					Ok({ after, .. }) => after
					Err(NotFound) => sans_query
				}
			auth = parse_authority_and_path(rest)
			Uri({
				scheme: SchemeRelative,
				userinfo: auth.userinfo,
				host: auth.host,
				port: auth.port,
				path: auth.path,
				query: parsed_query,
				fragment: parsed_fragment,
			})
		} else {
			match Str.split_first(sans_query, ":") {
				Ok({ before, after }) if is_scheme_token(before) =>
					if Str.starts_with(after, "//") {
						rest = 
							match Str.split_first(after, "//") {
								Ok({ after: r, .. }) => r
								Err(NotFound) => after
							}
						auth = parse_authority_and_path(rest)
						Uri({
							scheme: Scheme(before),
							userinfo: auth.userinfo,
							host: auth.host,
							port: auth.port,
							path: auth.path,
							query: parsed_query,
							fragment: parsed_fragment,
						})
					} else {
						# Opaque scheme: no authority, remainder is the path.
						Uri({
							scheme: Scheme(before),
							userinfo: NoUserinfo,
							host: NoHost,
							port: NoPort,
							path: after,
							query: parsed_query,
							fragment: parsed_fragment,
						})
					}

				_ =>
				# Relative reference: everything (minus query/fragment) is path.
					Uri({
						scheme: NoScheme,
						userinfo: NoUserinfo,
						host: NoHost,
						port: NoPort,
						path: sans_query,
						query: parsed_query,
						fragment: parsed_fragment,
					})
				}
		}
	}

	## Serializes a [Uri] back to a [Str].
	##
	## Component-exact: every field's raw text is preserved (including
	## empty-but-present queries/fragments/hosts and an unparseable port's raw
	## text), so `Uri.to_str(Uri.parse(s))` reproduces `s` up to one
	## normalization: an empty port (`https://x.com:`) serializes without the
	## dangling `:`.
	to_str : Uri -> Str
	to_str = |Uri(u)| {
		authority_present = 
			match u.host {
				NoHost => False
				_ => True
			}

		prefix = 
			match u.scheme {
				Scheme(s) => if authority_present {
					"${s}://"
				} else {
					"${s}:"
				}
				SchemeRelative => "//"
				NoScheme => if authority_present {
					"//"
				} else {
					""
				}
			}

		userinfo_str = 
			match u.userinfo {
				Userinfo(ui) => "${ui}@"
				NoUserinfo => ""
			}

		host_str = 
			match u.host {
				Host(h) => h
				_ => ""
			}

		port_str = 
			match u.port {
				Port(p) => Str.concat(":", p.to_str())
				NoPort => ""
				PortParseErr(raw) => Str.concat(":", raw)
			}

		query_str = 
			match u.query {
				Query(q) => Str.concat("?", q)
				EmptyQuery => "?"
				NoQuery => ""
			}

		fragment_str = 
			match u.fragment {
				Fragment(f) => Str.concat("#", f)
				EmptyFragment => "#"
				NoFragment => ""
			}

		prefix
			.concat(userinfo_str)
			.concat(host_str)
			.concat(port_str)
			.concat(u.path)
			.concat(query_str)
			.concat(fragment_str)
	}

	# ---------------------------------------------------------------------------
	# Accessors
	# ---------------------------------------------------------------------------

	## The URL's scheme: `Scheme("https")` for `https://...` or opaque `mailto:...`,
	## `SchemeRelative` for a leading `//`, `NoScheme` for relative references.
	scheme : Uri -> [Scheme(Str), SchemeRelative, NoScheme]
	scheme = |Uri(u)| u.scheme

	## The userinfo, which is everything before the last `@` in the authority.
	## `NoUserinfo` when there is no authority or no `@` in it.
	userinfo : Uri -> [Userinfo(Str), NoUserinfo]
	userinfo = |Uri(u)| u.userinfo

	## The host. `Host(s)` means the authority is present with a non-empty host
	## (never validated). `EmptyHost` means the authority is present but empty
	## (`https://:3000/`, `file:///etc`). `NoHost` means there is no authority
	## at all (`mailto:...`, `/path`).
	##
	## The host is lifted exactly as written, without case normalization, so
	## `Host("Example.COM") != Host("example.com")` even though RFC 3986 hosts
	## are case-insensitive. Lowercase before comparing if that's your policy.
	host : Uri -> [Host(Str), EmptyHost, NoHost]
	host = |Uri(u)| u.host

	## The port. `Ok(Port(8080))` means an explicit, parseable port. `Ok(NoPort)`
	## means no port was written (an empty `host:` also counts). A port that was
	## written but isn't a [U16] gives `Err(PortParseErr(raw))`, and the raw
	## text still round-trips through [Uri.to_str].
	port : Uri -> Try([Port(U16), NoPort], [PortParseErr(Str)])
	port = |Uri(u)|
		match u.port {
			Port(p) => Ok(Port(p))
			NoPort => Ok(NoPort)
			PortParseErr(raw) => Err(PortParseErr(raw))
		}

	## The path. Always a [Str], possibly `""` or `"/"`. For opaque schemes
	## (`mailto:a@b.com`) this is everything after the `:`.
	path : Uri -> Str
	path = |Uri(u)| u.path

	## The raw query, the part after `?`, not decoded. `EmptyQuery` (a bare
	## trailing `?`) is distinct from `NoQuery` (no `?` at all). See
	## [Uri.query_params] for decoded key/value pairs.
	query : Uri -> [Query(Str), EmptyQuery, NoQuery]
	query = |Uri(u)| u.query

	## The fragment, the part after the first `#`. `EmptyFragment` (a bare
	## trailing `#`) is distinct from `NoFragment`.
	fragment : Uri -> [Fragment(Str), EmptyFragment, NoFragment]
	fragment = |Uri(u)| u.fragment

	## [Bool.True] iff the URL has an authority (a host position, even an empty
	## one, as in `https://x.com`, `//x.com`, `file:///etc`). Note this is *not*
	## RFC "absoluteness": `mailto:a@b.com` has a scheme but no authority.
	has_authority : Uri -> Bool
	has_authority = |Uri(u)|
		match u.host {
			NoHost => False
			_ => True
		}

	# ---------------------------------------------------------------------------
	# Requirements: extract a component, failing if it isn't present/usable
	# ---------------------------------------------------------------------------

	## The [Uri] as a [Try]: `Ok(url)` unless [Uri.parse] left a component in
	## an error state. Parsing is per-component, so this hoists the fields'
	## outcomes into one `Try` at the top, and `Uri.parse(input).to_try()?` is
	## the all-or-nothing read on an otherwise total parse. The port is the
	## only component with an error state today, and `to_try` is the one place
	## to ask "did the whole thing parse cleanly" without knowing that.
	##
	## Absence is not an error. A relative reference like `/docs` passes
	## since it parsed exactly as written. If you also need a host or a
	## scheme to be present, follow with [Uri.require_host] or
	## [Uri.require_scheme].
	##
	## ```
	## # Gives Err(PortParseErr("banana"))
	## Uri.to_try(Uri.parse("https://example.com:banana/products"))
	## ```
	to_try : Uri -> Try(Uri, [PortParseErr(Str)])
	to_try = |Uri(u)|
		match u.port {
			PortParseErr(raw) => Err(PortParseErr(raw))
			_ => Ok(Uri(u))
		}

	## The host as a [Str], or why not. Never returns `Ok("")` since an
	## authority-present-but-empty host fails with its own `Err(EmptyHost)`.
	require_host : Uri -> Try(Str, [NoHost, EmptyHost])
	require_host = |Uri(u)|
		match u.host {
			Host(h) => Ok(h)
			EmptyHost => Err(EmptyHost)
			NoHost => Err(NoHost)
		}

	## The port as a [U16]. "Require" means *explicitly present*: an absent port
	## is `Err(NoPort)`, not a scheme default. See [Uri.port_or] for defaulting.
	require_port : Uri -> Try(U16, [NoPort, PortParseErr(Str)])
	require_port = |Uri(u)|
		match u.port {
			Port(p) => Ok(p)
			NoPort => Err(NoPort)
			PortParseErr(raw) => Err(PortParseErr(raw))
		}

	## The scheme as a [Str]. A scheme-relative URL (`//host/...`) fails with the
	## distinct `Err(SchemeRelative)`, not `NoScheme`.
	require_scheme : Uri -> Try(Str, [NoScheme, SchemeRelative])
	require_scheme = |Uri(u)|
		match u.scheme {
			Scheme(s) => Ok(s)
			SchemeRelative => Err(SchemeRelative)
			NoScheme => Err(NoScheme)
		}

	## The port, or `fallback` if none was written. Garbage is NOT defaulted:
	## an unparseable port still fails with `Err(PortParseErr(raw))`, keeping
	## "didn't specify" and "specified nonsense" distinct.
	port_or : Uri, U16 -> Try(U16, [PortParseErr(Str)])
	port_or = |Uri(u), fallback|
		match u.port {
			Port(p) => Ok(p)
			NoPort => Ok(fallback)
			PortParseErr(raw) => Err(PortParseErr(raw))
		}

	# ---------------------------------------------------------------------------
	# Query params
	# ---------------------------------------------------------------------------

	## The query as decoded key/value pairs, preserving repeated keys and order
	## (which a `Dict` would collapse). Keys and values are percent-decoded
	## leniently: a malformed escape falls back to the raw text rather than
	## failing. A bare flag (`?foo`) and an empty value (`?foo=`) both yield
	## `("foo", "")`, while the raw [Uri.query] field preserves the distinction.
	## Empty pairs (`?a=1&&b=2`, a trailing `&`) are skipped, matching the
	## WHATWG `application/x-www-form-urlencoded` parser.
	##
	## ```
	## # Gives [("café", "du Monde")]
	## Uri.query_params(Uri.parse("https://x.com?caf%C3%A9=du%20Monde"))
	## ```
	query_params : Uri -> List((Str, Str))
	query_params = |Uri(u)|
		match u.query {
			Query(q) => parse_query(q)
			EmptyQuery | NoQuery => []
		}

	## A query string on its own, as decoded key/value pairs. Pass the text that
	## follows the `?`, without the `?` itself.
	##
	## The rules are [Uri.query_params]' rules: repeated keys and order kept,
	## keys and values percent-decoded leniently, `foo` and `foo=` both yielding
	## `("foo", "")`, empty pairs skipped.
	##
	## Reach for this when the query text arrives on its own, with no URL around
	## it to parse: an `application/x-www-form-urlencoded` request body, or the
	## options half of a connection string you split yourself.
	##
	## ```
	## # Gives [("mode", "ro"), ("cache", "shared")]
	## Uri.parse_query("mode=ro&cache=shared")
	## ```
	parse_query : Str -> List((Str, Str))
	parse_query = |query_str|
		List.map(
			List.drop_if(Str.split_on(query_str, "&"), |pair| Str.is_empty(pair)),
			|pair|
				match Str.split_first(pair, "=") {
					Ok({ before, after }) => (percent_decode_lenient(before), percent_decode_lenient(after))
					Err(NotFound) => (percent_decode_lenient(pair), "")
				},
		)

	# ---------------------------------------------------------------------------
	# Writers: field updates, never re-parse. Every writer keeps the record
	# reparse-stable: parse(to_str(w(url))) has the same components as w(url).
	# ---------------------------------------------------------------------------

	## Appends path segments. Each element is exactly ONE segment and is fully
	## percent-encoded, including `/`, `?`, `#`, and `&`, which are data here,
	## not structure. There is no way to smuggle a query or an extra segment in
	## through `append_path`. Pass multiple elements for multiple segments. An empty
	## element appends an empty segment: at most a trailing `/`, and nothing at
	## all if the path already ends in one.
	##
	## ```
	## # Gives "https://example.com/some%20stuff"
	## Uri.to_str(Uri.append_path(Uri.parse("https://example.com"), ["some stuff"]))
	##
	## # Gives "https://example.com/users/someone%40example.com/posts"
	## Uri.to_str(Uri.append_path(Uri.parse("https://example.com"), ["users", "someone@example.com", "posts"]))
	## ```
	append_path : Uri, List(Str) -> Uri
	append_path = |url, segments|
		List.fold(
			segments,
			url,
			|Uri(u), segment| {
				encoded = percent_encode(segment)
				new_path = 
					if Str.is_empty(u.path) {
						match u.host {
							NoHost => encoded
							_ => Str.concat("/", encoded)
						}
					} else if Str.ends_with(u.path, "/") {
						Str.concat(u.path, encoded)
					} else {
						Str.concat(u.path, Str.concat("/", encoded))
					}
				Uri({ ..u, path: new_path })
			},
		)

	## Adds a query parameter. Always adds another pair, even when the key is
	## already present (see [Uri.with_param] for add-or-replace). Key and value
	## are fully percent-encoded, so reading them back through
	## [Uri.query_params] round-trips.
	##
	## ```
	## # Gives "https://example.com?email=someone%40example.com"
	## Uri.to_str(Uri.append_param(Uri.parse("https://example.com"), "email", "someone@example.com"))
	## ```
	append_param : Uri, Str, Str -> Uri
	append_param = |Uri(u), key, value| {
		pair = "${percent_encode(key)}=${percent_encode(value)}"
		new_query = 
			match u.query {
				Query(q) => Query("${q}&${pair}")
				EmptyQuery | NoQuery => Query(pair)
			}
		Uri({ ..u, query: new_query })
	}

	## Sets a query parameter, add-or-replace: the first occurrence of `key`
	## gets the new value in place, any other occurrences of `key` are
	## dropped, and a missing `key` is appended. Use [Uri.append_param] when
	## you want another pair regardless.
	##
	## Key and value are fully percent-encoded, and keys are compared decoded,
	## so it replaces exactly what [Uri.query_params] would report for `key`.
	##
	## ```
	## # Gives "https://x.com?item=axe&coupon=yes"
	## Uri.to_str(Uri.with_param(Uri.parse("https://x.com?item=sword&item=shield&coupon=yes"), "item", "axe"))
	## ```
	with_param : Uri, Str, Str -> Uri
	with_param = |Uri(u), key, value| {
		pair = "${percent_encode(key)}=${percent_encode(value)}"
		match u.query {
			Query(q) => {
				folded = 
					List.fold(
						Str.split_on(q, "&"),
						{ kept: [], replaced: False },
						|state, piece| {
							piece_key = 
								match Str.split_first(piece, "=") {
									Ok({ before, .. }) => before
									Err(NotFound) => piece
								}
							if Str.is_empty(piece) {
								# An empty piece ("a=1&&b=2") is kept byte-exact
								# and never matched
								{ ..state, kept: List.append(state.kept, piece) }
							} else if percent_decode_lenient(piece_key) == key {
								if state.replaced {
									state
								} else {
									{ kept: List.append(state.kept, pair), replaced: True }
								}
							} else {
								{ ..state, kept: List.append(state.kept, piece) }
							}
						},
					)
				new_q = 
					if folded.replaced {
						Str.join_with(folded.kept, "&")
					} else {
						Str.join_with(List.append(folded.kept, pair), "&")
					}
				Uri({ ..u, query: Query(new_q) })
			}

			EmptyQuery | NoQuery => Uri({ ..u, query: Query(pair) })
		}
	}

	## Replaces the raw query string. Passing `""` removes the query. Only `#`
	## is percent-encoded (it would shift the fragment boundary), while `=`, `&`, and
	## `?` are legitimate raw query content and pass through.
	with_query : Uri, Str -> Uri
	with_query = |Uri(u), query_str|
		if Str.is_empty(query_str) {
			Uri({ ..u, query: NoQuery })
		} else {
			Uri({ ..u, query: Query(encode_hashes(query_str)) })
		}

	## Replaces the fragment. Passing `""` removes it. Nothing is encoded: the
	## fragment is last, so any content round-trips via the first-`#` parse rule.
	with_fragment : Uri, Str -> Uri
	with_fragment = |Uri(u), fragment_str|
		if Str.is_empty(fragment_str) {
			Uri({ ..u, fragment: NoFragment })
		} else {
			Uri({ ..u, fragment: Fragment(fragment_str) })
		}

	## Sets the userinfo, the part before the `@` in the authority. Passing
	## `""` removes it. `:` passes through (the usual `user:password` shape),
	## and everything else outside RFC 3986 `userinfo` is percent-encoded, so
	## an `@` or `/` in it can't shift the host or path boundary.
	##
	## Userinfo can only serialize inside an authority, so setting it on an
	## authority-less URL creates an empty-host one, exactly as [Uri.with_port]
	## does.
	with_userinfo : Uri, Str -> Uri
	with_userinfo = |Uri(u), userinfo_str|
		if Str.is_empty(userinfo_str) {
			Uri({ ..u, userinfo: NoUserinfo })
		} else {
			encoded = Userinfo(encode_userinfo(userinfo_str))
			match u.host {
				NoHost => {
					new_scheme = 
						match u.scheme {
							NoScheme => SchemeRelative
							other => other
						}
					Uri({
						..u,
						userinfo: encoded,
						host: EmptyHost,
						scheme: new_scheme,
						path: path_reparse_fixup(EmptyHost, u.path),
					})
				}

				_ => Uri({ ..u, userinfo: encoded })
			}
		}

	## Replaces the whole path. `/` is structure and passes through, and every
	## segment between slashes is fully percent-encoded (so a `?` or `#` in a
	## segment cannot shift the query/fragment boundary). When the URL has an
	## authority, a non-empty path is given a leading `/`. When it has none, a
	## path starting with `//` is given a `/.` prefix so it can't reparse as an
	## authority. Both are required by RFC 3986 §3.3.
	with_path : Uri, Str -> Uri
	with_path = |Uri(u), path_str| {
		encoded = Str.join_with(List.map(Str.split_on(path_str, "/"), percent_encode), "/")
		Uri({ ..u, path: path_reparse_fixup(u.host, encoded) })
	}

	## Sets the host. Passing `""` removes the whole authority: host, userinfo,
	## and port (they have nowhere to serialize without one), and a path left
	## starting with `//` gains a `/.` prefix so it can't reparse as an
	## authority. A `[...]`-shaped argument passes through whole as an IP-literal
	## (`[::1]`), and anything else is percent-encoded where it falls outside
	## RFC 3986 `reg-name`.
	##
	## Because an authority requires `//`, setting a host on a scheme-less URL
	## makes it scheme-relative, and a non-empty rootless path gains a leading
	## `/`. Both keep `to_str`'s output reparsing to the same components.
	with_host : Uri, Str -> Uri
	with_host = |Uri(u), host_str|
		if Str.is_empty(host_str) {
			new_scheme = 
				match u.scheme {
					SchemeRelative => NoScheme
					other => other
				}
			Uri({
				..u,
				host: NoHost,
				userinfo: NoUserinfo,
				port: NoPort,
				scheme: new_scheme,
				path: path_reparse_fixup(NoHost, u.path),
			})
		} else {
			encoded = 
				if Str.starts_with(host_str, "[") and Str.ends_with(host_str, "]") {
					host_str
				} else {
					encode_reg_name(host_str)
				}
			new_scheme = 
				match u.scheme {
					NoScheme => SchemeRelative
					other => other
				}
			Uri({
				..u,
				host: Host(encoded),
				scheme: new_scheme,
				path: path_reparse_fixup(Host(encoded), u.path),
			})
		}

	## Sets an explicit port, replacing an absent or unparseable one.
	##
	## A port can only serialize inside an authority (RFC 3986 §3.2), so
	## setting one on an authority-less URL creates an empty-host authority
	## (`with_port(parse("/p"), 80)` gives `//:80/p`). As with [Uri.with_host],
	## a scheme-less URL becomes scheme-relative and a rootless path gains its
	## leading `/`, keeping the output reparse-stable.
	with_port : Uri, U16 -> Uri
	with_port = |Uri(u), p|
		match u.host {
			NoHost => {
				new_scheme = 
					match u.scheme {
						NoScheme => SchemeRelative
						other => other
					}
				Uri({
					..u,
					port: Port(p),
					host: EmptyHost,
					scheme: new_scheme,
					path: path_reparse_fixup(EmptyHost, u.path),
				})
			}

			_ => Uri({ ..u, port: Port(p) })
		}

	## Removes the port (also clearing an unparseable one).
	without_port : Uri -> Uri
	without_port = |Uri(u)| Uri({ ..u, port: NoPort })

	## Sets the scheme. The one fallible writer: a scheme has no percent-encoding
	## escape mechanism, so an argument that isn't a valid RFC 3986 scheme token
	## (`ALPHA (ALPHA/DIGIT/+/-/.)*`) is rejected rather than mangled.
	##
	## Passing `""` removes the scheme. If the URL has an authority it becomes
	## scheme-relative (`//host/...`) so the authority still serializes.
	with_scheme : Uri, Str -> Try(Uri, [InvalidSchemeToken(Str)])
	with_scheme = |Uri(u), scheme_str|
		if Str.is_empty(scheme_str) {
			new_scheme = 
				match u.host {
					NoHost => NoScheme
					_ => SchemeRelative
				}
			Ok(Uri({ ..u, scheme: new_scheme }))
		} else if is_scheme_token(scheme_str) {
			Ok(Uri({ ..u, scheme: Scheme(scheme_str) }))
		} else {
			Err(InvalidSchemeToken(scheme_str))
		}

	# ---------------------------------------------------------------------------
	# Reference resolution
	# ---------------------------------------------------------------------------

	## Resolves a reference against a base [Uri] per RFC 3986 §5. This is what a
	## browser does to follow a link: `resolve(base, href)` turns a relative
	## `href` into the absolute URL it points at.
	##
	## The reference keeps whatever it specifies and inherits the rest from the
	## base: a reference with its own scheme is used as-is, a scheme-relative
	## `//host/...` borrows the base's scheme, a rooted `/path` replaces the base's
	## path, a rootless `path` is merged against the base's directory, and a bare
	## `?query` or `#fragment` keeps the base's path. Dot segments (`.`, `..`) in
	## the result are removed (RFC 3986 §5.2.4).
	##
	## Resolution is strict: a reference's scheme is never ignored even when it
	## equals the base's. Pass an absolute base for RFC-meaningful results. A
	## non-absolute base still resolves, best-effort, by the same algorithm.
	##
	## ```
	## # Gives "https://example.com/a/g"
	## Uri.to_str(Uri.resolve(Uri.parse("https://example.com/a/b/c"), Uri.parse("../g")))
	## ```
	resolve : Uri, Uri -> Uri
	resolve = |Uri(base), Uri(ref)|
		match ref.scheme {
			Scheme(_) =>
				Uri({
					scheme: ref.scheme,
					userinfo: ref.userinfo,
					host: ref.host,
					port: ref.port,
					path: remove_dot_segments(ref.path),
					query: ref.query,
					fragment: ref.fragment,
				})

			_ => {
				ref_has_authority = 
					match ref.host {
						NoHost => False
						_ => True
					}

				if ref_has_authority {
					Uri({
						scheme: base.scheme,
						userinfo: ref.userinfo,
						host: ref.host,
						port: ref.port,
						path: remove_dot_segments(ref.path),
						query: ref.query,
						fragment: ref.fragment,
					})
				} else {
					(result_path, result_query) = 
						if Str.is_empty(ref.path) {
							new_query = 
								match ref.query {
									NoQuery => base.query
									_ => ref.query
								}
							(base.path, new_query)
						} else if Str.starts_with(ref.path, "/") {
							(remove_dot_segments(ref.path), ref.query)
						} else {
							(remove_dot_segments(merge_paths(Uri(base), ref.path)), ref.query)
						}

					Uri({
						scheme: base.scheme,
						userinfo: base.userinfo,
						host: base.host,
						port: base.port,
						path: result_path,
						query: result_query,
						fragment: ref.fragment,
					})
				}
			}
		}

	# ---------------------------------------------------------------------------
	# Percent encoding
	# ---------------------------------------------------------------------------

	## [Percent-encodes](https://en.wikipedia.org/wiki/Percent-encoding) a string
	## per [RFC 3986](https://www.ietf.org/rfc/rfc3986.txt): everything except
	## unreserved characters (A-Z, a-z, 0-9, `-`, `_`, `.`, `~`) is encoded.
	##
	## ```
	## # Gives "Hello%20World"
	## Uri.percent_encode("Hello World")
	## ```
	##
	## Be careful not to double-encode: [Uri.append_path] and [Uri.append_param]
	## already encode their arguments.
	percent_encode : Str -> Str
	percent_encode = |input| {
		# Optimistically assume no encoding is needed. If we're wrong the list
		# will grow.
		initial_output = List.with_capacity(Str.count_utf8_bytes(input))

		answer = 
			List.fold(
				Str.to_utf8(input),
				initial_output,
				|output, byte|
					if is_unreserved(byte) {
						List.append(output, byte)
					} else {
						List.concat(output, encoded_triplet(byte))
					},
			)

		# This should never fail
		Try.ok_or(Str.from_utf8(answer), "")
	}

	## [Percent-decodes](https://en.wikipedia.org/wiki/Percent-encoding) a
	## string, converting sequences like `%20` back to their characters.
	##
	## Returns `Err(InvalidEncoding)` on a malformed escape.
	##
	## ```
	## # Gives Ok("café")
	## Uri.percent_decode("caf%C3%A9")
	## ```
	percent_decode : Str -> Try(Str, [InvalidEncoding])
	percent_decode = |encoded| {
		folded = 
			List.fold(
				Str.to_utf8(encoded),
				Ok({ utf8: [], action: Step }),
				|state, byte|
					match state {
						Err(_) => state
						Ok(s) =>
							match s.action {
								# A "%" only starts an escape from Step. A "%" seen
								# while already mid-escape (TakeFirst/TakeSecond) is
								# not a hex digit, so it flows into hex_pair_to_decimal
								# and fails, rather than silently resetting the decoder
								# and dropping the partial escape (the "%2%20" bug).
								Step =>
									if byte == 37 { # Byte 37 is %
										Ok({ ..s, action: TakeFirst })
									} else {
										Ok({ ..s, utf8: List.append(s.utf8, byte) })
									}

								TakeFirst => Ok({ ..s, action: TakeSecond(byte) })
								TakeSecond(previous_byte) =>
									match hex_pair_to_decimal(previous_byte, byte) {
										Ok(decoded_byte) =>
											Ok({ ..s, utf8: List.append(s.utf8, decoded_byte), action: Step })

										Err(_) => Err(InvalidEncoding)
									}
								}
						},
			)

		r = Try.map_err(folded, |_| InvalidEncoding)?

		match r.action {
			Step => Try.map_err(Str.from_utf8(r.utf8), |_| InvalidEncoding)
			_ => Err(InvalidEncoding)
		}
	}

	# ---------------------------------------------------------------------------
	# Internal helpers
	# ---------------------------------------------------------------------------

	## Internal helper. Splits a [Str] on the last occurrence of `delim`,
	## returning the part before and after it.
	split_last : Str, Str -> Try({ before : Str, after : Str }, [NotFound])
	split_last = |str, delim| {
		parts = Str.split_on(str, delim)
		n = List.len(parts)
		if n > 1 {
			before = Str.join_with(List.take_first(parts, n - 1), delim)
			after = Try.ok_or(List.last(parts), "")
			Ok({ before, after })
		} else {
			Err(NotFound)
		}
	}

	## Internal helper. The fragment starts at the FIRST "#" (RFC 3986), and
	## later "#"s are part of the fragment itself.
	extract_fragment : Str -> ([Fragment(Str), EmptyFragment, NoFragment], Str)
	extract_fragment = |input|
		match Str.split_first(input, "#") {
			Ok({ before: rest, after: f }) =>
				if Str.is_empty(f) {
					(EmptyFragment, rest)
				} else {
					(Fragment(f), rest)
				}

			Err(NotFound) => (NoFragment, input)
		}

	## Internal helper. The query starts at the FIRST "?" (the fragment has
	## already been removed), and later "?"s are part of the query itself.
	extract_query : Str -> ([Query(Str), EmptyQuery, NoQuery], Str)
	extract_query = |input|
		match Str.split_first(input, "?") {
			Ok({ before: rest, after: q }) =>
				if Str.is_empty(q) {
					(EmptyQuery, rest)
				} else {
					(Query(q), rest)
				}

			Err(NotFound) => (NoQuery, input)
		}

	## Internal helper. Splits the text after "//" into authority components and
	## path. The authority ends at the first "/" (query and fragment are
	## already stripped), and the userinfo ends at the LAST "@" in the
	## authority.
	parse_authority_and_path : Str -> {
		userinfo : [Userinfo(Str), NoUserinfo],
		host : [Host(Str), EmptyHost, NoHost],
		port : [Port(U16), NoPort, PortParseErr(Str)],
		path : Str,
	}
	parse_authority_and_path = |rest| {
		split_path = 
			match Str.split_first(rest, "/") {
				Ok({ before, after }) => { authority: before, path: Str.concat("/", after) }
				Err(NotFound) => { authority: rest, path: "" }
			}

		split_userinfo = 
			match split_last(split_path.authority, "@") {
				Ok({ before, after }) => { userinfo: Userinfo(before), host_port: after }
				Err(NotFound) => { userinfo: NoUserinfo, host_port: split_path.authority }
			}

		hp = split_host_port(split_userinfo.host_port)

		{
			userinfo: split_userinfo.userinfo,
			host: hp.host,
			port: hp.port,
			path: split_path.path,
		}
	}

	## Internal helper. Splits "host:port" bracket-awarely: a leading "[" host
	## extends to the matching "]" (an unmatched "[" or junk after "]" folds
	## into the host so it still round-trips). An empty port ("host:") is
	## RFC-valid and means no port.
	split_host_port : Str -> {
		host : [Host(Str), EmptyHost, NoHost],
		port : [Port(U16), NoPort, PortParseErr(Str)],
	}
	split_host_port = |text|
		if Str.starts_with(text, "[") {
			match Str.split_first(text, "]") {
				Ok({ before, after }) => {
					bracketed = Str.concat(before, "]")
					if Str.is_empty(after) {
						{ host: Host(bracketed), port: NoPort }
					} else if Str.starts_with(after, ":") {
						port_text = 
							match Str.split_first(after, ":") {
								Ok({ after: pt, .. }) => pt
								Err(NotFound) => ""
							}
						{ host: Host(bracketed), port: parse_port(port_text) }
					} else {
						# Junk after "]" that isn't a port folds into the host.
						{ host: Host(text), port: NoPort }
					}
				}

				# Unmatched "[": the whole remainder is the host.
				Err(NotFound) => { host: Host(text), port: NoPort }
			}
		} else {
			match Str.split_first(text, ":") {
				Ok({ before, after }) => { host: host_tag(before), port: parse_port(after) }
				Err(NotFound) => { host: host_tag(text), port: NoPort }
			}
		}

	## Internal helper
	host_tag : Str -> [Host(Str), EmptyHost, NoHost]
	host_tag = |text|
		if Str.is_empty(text) {
			EmptyHost
		} else {
			Host(text)
		}

	## Internal helper
	parse_port : Str -> [Port(U16), NoPort, PortParseErr(Str)]
	parse_port = |text|
		if Str.is_empty(text) {
			# "host:" with an empty port is RFC-valid, and to_str normalizes the ":" away.
			NoPort
		} else {
			match U16.from_str(text) {
				Ok(p) => Port(p)
				Err(_) => PortParseErr(text)
			}
		}

	## Internal helper. A valid RFC 3986 §3.1 scheme token: first char ALPHA,
	## rest ALPHA / DIGIT / "+" / "-" / ".".
	is_scheme_token : Str -> Bool
	is_scheme_token = |text| {
		bytes = Str.to_utf8(text)
		match List.first(bytes) {
			Ok(first) => is_alpha(first) and List.all(bytes, is_scheme_byte)
			Err(_) => False
		}
	}

	## Internal helper
	is_alpha : U8 -> Bool
	is_alpha = |byte|
		(byte >= 'a' and byte <= 'z')
			or (byte >= 'A' and byte <= 'Z')

	## Internal helper
	is_scheme_byte : U8 -> Bool
	is_scheme_byte = |byte|
		is_alpha(byte)
			or (byte >= '0' and byte <= '9')
				or byte == '+'
					or byte == '-'
						or byte == '.'

	## Internal helper. RFC 3986 unreserved: A-Z, a-z, 0-9, "-", ".", "_", "~".
	is_unreserved : U8 -> Bool
	is_unreserved = |byte|
		is_alpha(byte)
			or (byte >= '0' and byte <= '9')
				or byte == '-'
					or byte == '.'
						or byte == '_'
							or byte == '~'

	## Internal helper. Bytes allowed to pass through unencoded in a reg-name:
	## unreserved + sub-delims. A literal "%" is NOT included: writer inputs are
	## treated as literal data and fully encoded (matching append_path and
	## append_param), so a "%" becomes "%25" and the host round-trips rather than
	## being read back as a malformed escape.
	is_reg_name_byte : U8 -> Bool
	is_reg_name_byte = |byte|
		is_unreserved(byte)
			or byte == 33 # !
				or byte == 36 # $
					or byte == 38 # &
						or byte == 39 # '
							or byte == 40 # (
								or byte == 41 # )
									or byte == 42 # *
										or byte == 43 # +
											or byte == 44 # ,
												or byte == 59 # ;
													or byte == 61 # =

	## Internal helper. The "%XX" triplet for a byte, as UTF-8 bytes.
	encoded_triplet : U8 -> List(U8)
	encoded_triplet = |byte|
		List.sublist(
			Str.to_utf8(percent_encoded),
			{ start: 3 * byte.to_u64(), len: 3 },
		)

	## Internal helper. Percent-encodes everything outside the reg-name set.
	encode_reg_name : Str -> Str
	encode_reg_name = |input| {
		answer = 
			List.fold(
				Str.to_utf8(input),
				[],
				|output, byte|
					if is_reg_name_byte(byte) {
						List.append(output, byte)
					} else {
						List.concat(output, encoded_triplet(byte))
					},
			)
		Try.ok_or(Str.from_utf8(answer), "")
	}

	## Internal helper. Percent-encodes everything outside the RFC 3986 §3.2.1
	## userinfo set, which is the reg-name set plus ":".
	encode_userinfo : Str -> Str
	encode_userinfo = |input| {
		answer = 
			List.fold(
				Str.to_utf8(input),
				[],
				|output, byte|
					if is_reg_name_byte(byte) or byte == ':' {
						List.append(output, byte)
					} else {
						List.concat(output, encoded_triplet(byte))
					},
			)
		Try.ok_or(Str.from_utf8(answer), "")
	}

	## Internal helper. Encodes only "#", the one character that would shift
	## the fragment boundary out of a raw query string.
	encode_hashes : Str -> Str
	encode_hashes = |input|
		Str.join_with(Str.split_on(input, "#"), "%23")

	## Internal helper. Write-time path guards, one per direction of RFC 3986
	## §3.3. With an authority, a non-empty path must start with "/"
	## (path-abempty) or its first segment would merge into the host on
	## reparse. Without one, a path must NOT start with "//" or it would
	## reparse as an authority, so it gains a "/." prefix (the WHATWG
	## serializer's fix, equivalent after dot-segment normalization).
	path_reparse_fixup : [Host(Str), EmptyHost, NoHost], Str -> Str
	path_reparse_fixup = |host_field, p|
		match host_field {
			NoHost =>
				if Str.starts_with(p, "//") {
					Str.concat("/.", p)
				} else {
					p
				}

			_ =>
				if Str.is_empty(p) or Str.starts_with(p, "/") {
					p
				} else {
					Str.concat("/", p)
				}
			}

	## Internal helper. RFC 3986 §5.2.3: merges a rootless reference path onto the
	## base's path. If the base has an authority and an empty path the reference is
	## rooted at "/". Otherwise it replaces everything after the base path's last
	## "/".
	merge_paths : Uri, Str -> Str
	merge_paths = |Uri(base), ref_path| {
		base_has_authority = 
			match base.host {
				NoHost => False
				_ => True
			}
		if base_has_authority and Str.is_empty(base.path) {
			Str.concat("/", ref_path)
		} else {
			match split_last(base.path, "/") {
				Ok({ before, .. }) => Str.concat(before, Str.concat("/", ref_path))
				Err(NotFound) => ref_path
			}
		}
	}

	## Internal helper. RFC 3986 §5.2.4: removes "." and ".." segments from a path,
	## resolving them the way a filesystem would ("/a/b/../c" -> "/a/c"). Works on
	## the ASCII "/" and "." structure only, so a byte round-trip is always safe.
	remove_dot_segments : Str -> Str
	remove_dot_segments = |raw_path| {
		result = remove_dot_loop(Str.to_utf8(raw_path), [])
		Try.ok_or(Str.from_utf8(result), raw_path)
	}

	## Internal helper for remove_dot_segments: the RFC 3986 §5.2.4 loop, moving
	## segments from the input buffer to the output buffer.
	remove_dot_loop : List(U8), List(U8) -> List(U8)
	remove_dot_loop = |input, output|
		if List.is_empty(input) {
			output
		} else if List.starts_with(input, ['.', '.', '/']) {
			remove_dot_loop(List.drop_first(input, 3), output)
		} else if List.starts_with(input, ['.', '/']) {
			remove_dot_loop(List.drop_first(input, 2), output)
		} else if List.starts_with(input, ['/', '.', '/']) {
			remove_dot_loop(List.concat(['/'], List.drop_first(input, 3)), output)
		} else if input == ['/', '.'] {
			remove_dot_loop(['/'], output)
		} else if List.starts_with(input, ['/', '.', '.', '/']) {
			remove_dot_loop(List.concat(['/'], List.drop_first(input, 4)), pop_last_segment(output))
		} else if input == ['/', '.', '.'] {
			remove_dot_loop(['/'], pop_last_segment(output))
		} else if input == ['.'] or input == ['.', '.'] {
			remove_dot_loop([], output)
		} else {
			moved = take_first_segment(input)
			remove_dot_loop(moved.rest, List.concat(output, moved.seg))
		}

	## Internal helper for remove_dot_segments: RFC 3986 §5.2.4 step E. Moves the
	## first path segment (its leading "/", if any, plus everything up to but not
	## including the next "/") out of the input.
	take_first_segment : List(U8) -> { seg : List(U8), rest : List(U8) }
	take_first_segment = |input|
		match List.first(input) {
			Ok(first) =>
				if first == '/' {
					after = List.drop_first(input, 1)
					idx = index_of_slash(after)
					{ seg: List.concat(['/'], List.take_first(after, idx)), rest: List.drop_first(after, idx) }
				} else {
					idx = index_of_slash(input)
					{ seg: List.take_first(input, idx), rest: List.drop_first(input, idx) }
				}

			Err(_) => { seg: [], rest: [] }
		}

	## Internal helper for remove_dot_segments: RFC 3986 §5.2.4 step C. Removes the
	## last segment and its preceding "/" from the output buffer. No "/" leaves
	## an empty buffer, matching the RFC's "remove up to and including the last /".
	pop_last_segment : List(U8) -> List(U8)
	pop_last_segment = |output|
		match List.split_last(output, '/') {
			Ok({ before, .. }) => before
			Err(NotFound) => []
		}

	## Internal helper. Index of the first "/" in the bytes, or the length if none.
	index_of_slash : List(U8) -> U64
	index_of_slash = |bytes|
		match List.find_first_index(bytes, |b| b == '/') {
			Ok(idx) => idx
			Err(NotFound) => List.len(bytes)
		}

	## Internal helper. Lenient decode for query_params: malformed escapes fall
	## back to the raw text instead of failing.
	percent_decode_lenient : Str -> Str
	percent_decode_lenient = |text|
		match percent_decode(text) {
			Ok(decoded) => decoded
			Err(_) => text
		}

	## Internal helper for percent_decode
	hex_pair_to_decimal : U8, U8 -> Try(U8, [InvalidHex])
	hex_pair_to_decimal = |first, second| {
		first_hex = hex_char_to_decimal(first)?
		second_hex = hex_char_to_decimal(second)?
		Ok(first_hex * 16 + second_hex)
	}

	## Internal helper for percent_decode
	hex_char_to_decimal : U8 -> Try(U8, [InvalidHex])
	hex_char_to_decimal = |char|
		if char >= 48 and char <= 57 {
			Ok(char - 48) # '0' to '9'
		} else if char >= 65 and char <= 70 {
			Ok(char - 55) # 'A' to 'F'
		} else if char >= 97 and char <= 102 {
			Ok(char - 87) # 'a' to 'f'
		} else {
			Err(InvalidHex)
		}

	# Adapted from the percent-encoding crate:
	# https://github.com/servo/rust-url/blob/e12d76a61add5bc09980599c738099feaacd1d0d/percent_encoding/src/lib.rs#L183
	percent_encoded : Str
	percent_encoded = "%00%01%02%03%04%05%06%07%08%09%0A%0B%0C%0D%0E%0F%10%11%12%13%14%15%16%17%18%19%1A%1B%1C%1D%1E%1F%20%21%22%23%24%25%26%27%28%29%2A%2B%2C%2D%2E%2F%30%31%32%33%34%35%36%37%38%39%3A%3B%3C%3D%3E%3F%40%41%42%43%44%45%46%47%48%49%4A%4B%4C%4D%4E%4F%50%51%52%53%54%55%56%57%58%59%5A%5B%5C%5D%5E%5F%60%61%62%63%64%65%66%67%68%69%6A%6B%6C%6D%6E%6F%70%71%72%73%74%75%76%77%78%79%7A%7B%7C%7D%7E%7F%80%81%82%83%84%85%86%87%88%89%8A%8B%8C%8D%8E%8F%90%91%92%93%94%95%96%97%98%99%9A%9B%9C%9D%9E%9F%A0%A1%A2%A3%A4%A5%A6%A7%A8%A9%AA%AB%AC%AD%AE%AF%B0%B1%B2%B3%B4%B5%B6%B7%B8%B9%BA%BB%BC%BD%BE%BF%C0%C1%C2%C3%C4%C5%C6%C7%C8%C9%CA%CB%CC%CD%CE%CF%D0%D1%D2%D3%D4%D5%D6%D7%D8%D9%DA%DB%DC%DD%DE%DF%E0%E1%E2%E3%E4%E5%E6%E7%E8%E9%EA%EB%EC%ED%EE%EF%F0%F1%F2%F3%F4%F5%F6%F7%F8%F9%FA%FB%FC%FD%FE%FF"
}

# =============================================================================
# Tests: classification: full URLs
# =============================================================================

expect {
	u = Uri.parse("https://www.example.com:8080/path?query=val#fragment")
	Uri.scheme(u) == Scheme("https")
		and Uri.userinfo(u) == NoUserinfo
			and Uri.host(u) == Host("www.example.com")
				and Uri.port(u) == Ok(Port(8080))
					and Uri.path(u) == "/path"
						and Uri.query(u) == Query("query=val")
							and Uri.fragment(u) == Fragment("fragment")
}

expect {
	u = Uri.parse("http://example.com/path/to/resource")
	Uri.scheme(u) == Scheme("http")
		and Uri.host(u) == Host("example.com")
			and Uri.port(u) == Ok(NoPort)
				and Uri.path(u) == "/path/to/resource"
}

# No path after the authority: path is "" and the input round-trips exactly
expect {
	u = Uri.parse("ftp://localhost")
	Uri.host(u) == Host("localhost") and Uri.path(u) == "" and Uri.to_str(u) == "ftp://localhost"
}

expect {
	u = Uri.parse("https://user:pass@example.com/path")
	Uri.userinfo(u) == Userinfo("user:pass") and Uri.host(u) == Host("example.com")
}

# =============================================================================
# Tests: classification: opaque schemes
# =============================================================================

expect {
	u = Uri.parse("mailto:a@b.com")
	Uri.scheme(u) == Scheme("mailto") and Uri.host(u) == NoHost and Uri.path(u) == "a@b.com"
}

expect {
	u = Uri.parse("urn:isbn:0")
	Uri.scheme(u) == Scheme("urn") and Uri.host(u) == NoHost and Uri.path(u) == "isbn:0"
}

expect {
	u = Uri.parse("tel:+1-555")
	Uri.scheme(u) == Scheme("tel") and Uri.host(u) == NoHost and Uri.path(u) == "+1-555"
}

expect {
	u = Uri.parse("tel:5551234")
	Uri.scheme(u) == Scheme("tel") and Uri.host(u) == NoHost and Uri.path(u) == "5551234"
}

expect {
	u = Uri.parse("data:text/plain,x")
	Uri.scheme(u) == Scheme("data") and Uri.host(u) == NoHost and Uri.path(u) == "text/plain,x"
}

# The strict RFC reading: scheme-less "host:port" text is a scheme, not a
# host. That surprises people, so this test pins it as a choice rather than
# an oversight.
expect {
	u = Uri.parse("localhost:3000/x")
	Uri.scheme(u) == Scheme("localhost") and Uri.host(u) == NoHost and Uri.path(u) == "3000/x"
}

expect {
	u = Uri.parse("example.com:8080/path")
	Uri.scheme(u) == Scheme("example.com") and Uri.host(u) == NoHost and Uri.path(u) == "8080/path"
}

# An opaque scheme with an empty body round-trips
expect {
	u = Uri.parse("x.com:")
	Uri.scheme(u) == Scheme("x.com") and Uri.path(u) == "" and Uri.to_str(u) == "x.com:"
}

# =============================================================================
# Tests: classification: relative references (all path)
# =============================================================================

expect {
	u = Uri.parse("docs/index.html")
	Uri.scheme(u) == NoScheme and Uri.host(u) == NoHost and Uri.path(u) == "docs/index.html"
}

expect {
	u = Uri.parse("README.md")
	Uri.scheme(u) == NoScheme and Uri.host(u) == NoHost and Uri.path(u) == "README.md"
}

# Bare "example.com" is a path, not a host, on purpose. Prepending a
# scheme is up to the caller.
expect {
	u = Uri.parse("example.com")
	Uri.scheme(u) == NoScheme and Uri.host(u) == NoHost and Uri.path(u) == "example.com"
}

# "1" is not ALPHA, so there's no scheme token, and no "//" means no authority.
expect {
	u = Uri.parse("192.168.1.1:8080/admin")
	Uri.scheme(u) == NoScheme and Uri.host(u) == NoHost and Uri.path(u) == "192.168.1.1:8080/admin"
}

expect {
	u = Uri.parse("/path?query=val#fragment")
	Uri.scheme(u) == NoScheme
		and Uri.host(u) == NoHost
			and Uri.path(u) == "/path"
				and Uri.query(u) == Query("query=val")
					and Uri.fragment(u) == Fragment("fragment")
}

# =============================================================================
# Tests: classification: scheme-relative
# =============================================================================

expect {
	u = Uri.parse("//google.com/x")
	Uri.scheme(u) == SchemeRelative and Uri.host(u) == Host("google.com") and Uri.path(u) == "/x"
}

expect {
	u = Uri.parse("//user@google.com:8080/x?q=1#frag")
	Uri.scheme(u) == SchemeRelative
		and Uri.userinfo(u) == Userinfo("user")
			and Uri.host(u) == Host("google.com")
				and Uri.port(u) == Ok(Port(8080))
					and Uri.path(u) == "/x"
						and Uri.query(u) == Query("q=1")
							and Uri.fragment(u) == Fragment("frag")
}

# =============================================================================
# Tests: totality corners
# =============================================================================

expect {
	u = Uri.parse("")
	Uri.scheme(u) == NoScheme
		and Uri.userinfo(u) == NoUserinfo
			and Uri.host(u) == NoHost
				and Uri.port(u) == Ok(NoPort)
					and Uri.path(u) == ""
						and Uri.query(u) == NoQuery
							and Uri.fragment(u) == NoFragment
								and Uri.to_str(u) == ""
}

expect {
	u = Uri.parse("https://")
	Uri.scheme(u) == Scheme("https") and Uri.host(u) == EmptyHost and Uri.path(u) == "" and Uri.to_str(u) == "https://"
}

expect {
	u = Uri.parse("https://:3000/")
	Uri.host(u) == EmptyHost and Uri.port(u) == Ok(Port(3000)) and Uri.path(u) == "/" and Uri.to_str(u) == "https://:3000/"
}

expect {
	u = Uri.parse("https://user@")
	Uri.userinfo(u) == Userinfo("user") and Uri.host(u) == EmptyHost and Uri.to_str(u) == "https://user@"
}

# file: URLs have an empty authority on purpose: EmptyHost, not an error
expect {
	u = Uri.parse("file:///etc/passwd")
	Uri.scheme(u) == Scheme("file")
		and Uri.host(u) == EmptyHost
			and Uri.path(u) == "/etc/passwd"
				and Uri.to_str(u) == "file:///etc/passwd"
}

# Bracket-aware host/port split
expect {
	u = Uri.parse("http://[::1]:8080/")
	Uri.host(u) == Host("[::1]") and Uri.port(u) == Ok(Port(8080)) and Uri.path(u) == "/"
}

# Unmatched "[": the whole authority remainder is the host, and it still round-trips
expect {
	u = Uri.parse("http://[::1/path")
	Uri.host(u) == Host("[::1") and Uri.port(u) == Ok(NoPort) and Uri.path(u) == "/path" and Uri.to_str(u) == "http://[::1/path"
}

# Junk after "]" folds into the host and still round-trips
expect {
	u = Uri.parse("http://[::1]junk/")
	Uri.host(u) == Host("[::1]junk") and Uri.to_str(u) == "http://[::1]junk/"
}

# Bare bracketed literal without "//" is a relative reference. Brackets only
# mean anything inside an authority.
expect {
	u = Uri.parse("[::1]:8080")
	Uri.scheme(u) == NoScheme and Uri.host(u) == NoHost and Uri.path(u) == "[::1]:8080"
}

# A fragment-only reference is a valid same-document reference
expect {
	u = Uri.parse("#frag")
	Uri.path(u) == "" and Uri.fragment(u) == Fragment("frag") and Uri.to_str(u) == "#frag"
}

# =============================================================================
# Tests: RFC first/last-occurrence rules
# =============================================================================

# The query starts at the first "?". Later "?"s are part of the query itself.
expect {
	u = Uri.parse("https://x.com/p?a=1?b=2")
	Uri.query(u) == Query("a=1?b=2")
}

# The fragment starts at the first "#". Later "#"s are part of the fragment.
expect {
	u = Uri.parse("https://x.com/p#a#b")
	Uri.fragment(u) == Fragment("a#b")
}

# A "?" inside the fragment is not a query
expect {
	u = Uri.parse("https://x.com/page#section?id=123")
	Uri.path(u) == "/page" and Uri.query(u) == NoQuery and Uri.fragment(u) == Fragment("section?id=123")
}

# The userinfo ends at the last "@" in the authority
expect {
	u = Uri.parse("https://u@v@example.com/p")
	Uri.userinfo(u) == Userinfo("u@v") and Uri.host(u) == Host("example.com")
}

# An empty userinfo is lifted as-is and round-trips
expect {
	u = Uri.parse("https://@example.com/p")
	Uri.userinfo(u) == Userinfo("") and Uri.to_str(u) == "https://@example.com/p"
}

# An "@" in the query is not userinfo
expect {
	u = Uri.parse("https://example.com/path?email=foo@bar.com")
	Uri.userinfo(u) == NoUserinfo
}

# An "@" in the path is not userinfo
expect {
	u = Uri.parse("https://example.com/@username/profile")
	Uri.userinfo(u) == NoUserinfo
}

# Empty-but-present query and fragment are distinct from absent, and round-trip
expect {
	u = Uri.parse("https://x.com?")
	Uri.query(u) == EmptyQuery and Uri.to_str(u) == "https://x.com?"
}

expect {
	u = Uri.parse("https://x.com#")
	Uri.fragment(u) == EmptyFragment and Uri.to_str(u) == "https://x.com#"
}

expect {
	u = Uri.parse("/path?#")
	Uri.query(u) == EmptyQuery and Uri.fragment(u) == EmptyFragment and Uri.to_str(u) == "/path?#"
}

# =============================================================================
# Tests: port states
# =============================================================================

# Garbage port: PortParseErr carries the raw text and round-trips
expect {
	u = Uri.parse("https://x.com:junk/")
	Uri.port(u) == Err(PortParseErr("junk")) and Uri.host(u) == Host("x.com") and Uri.to_str(u) == "https://x.com:junk/"
}

# Out-of-range port inside an explicit authority is a PortParseErr
expect {
	u = Uri.parse("//x.com:99999")
	Uri.port(u) == Err(PortParseErr("99999")) and Uri.to_str(u) == "//x.com:99999"
}

# An empty port ("host:") is RFC-valid, means no port, and to_str normalizes
# the dangling ":" away. That's the documented normalization.
expect {
	u = Uri.parse("https://x.com:/p")
	Uri.port(u) == Ok(NoPort) and Uri.to_str(u) == "https://x.com/p"
}

# =============================================================================
# Tests: to_try
# =============================================================================

# The one error state parse can produce is an unparseable port
expect Uri.to_try(Uri.parse("https://example.com:banana/p")) == Err(PortParseErr("banana"))
expect Uri.to_try(Uri.parse("https://example.com:8080/p")) == Ok(Uri.parse("https://example.com:8080/p"))

# Absence is not an error: relative references and empty input pass
expect Uri.to_try(Uri.parse("/docs")) == Ok(Uri.parse("/docs"))
expect Uri.to_try(Uri.parse("")) == Ok(Uri.parse(""))
expect Uri.to_try(Uri.parse("mailto:a@b.com")) == Ok(Uri.parse("mailto:a@b.com"))

# Odd-but-valid shapes pass: empty host, garbage folded into the host
expect Uri.to_try(Uri.parse("file:///etc")) == Ok(Uri.parse("file:///etc"))
expect Uri.to_try(Uri.parse("http://[::1]junk/")) == Ok(Uri.parse("http://[::1]junk/"))

# Writers can't produce an error state, so a write turns a failing to_try
# into a passing one only by replacing the garbage
expect Uri.to_try(Uri.with_port(Uri.parse("https://x.com:banana/"), 80)) == Ok(Uri.with_port(Uri.parse("https://x.com:banana/"), 80))
expect Uri.to_try(Uri.append_path(Uri.parse("https://x.com:banana/"), ["p"])) == Err(PortParseErr("banana"))

# =============================================================================
# Tests: require_* / port_or / has_authority
# =============================================================================

expect Uri.require_host(Uri.parse("https://example.com/x")) == Ok("example.com")
expect Uri.require_host(Uri.parse("https://:3000/")) == Err(EmptyHost)
expect Uri.require_host(Uri.parse("/path")) == Err(NoHost)
expect Uri.require_host(Uri.parse("mailto:a@b.com")) == Err(NoHost)

expect Uri.require_port(Uri.parse("https://x.com:8080/")) == Ok(8080)
expect Uri.require_port(Uri.parse("https://x.com/")) == Err(NoPort)
expect Uri.require_port(Uri.parse("https://x.com:junk/")) == Err(PortParseErr("junk"))

expect Uri.require_scheme(Uri.parse("https://x.com/")) == Ok("https")
expect Uri.require_scheme(Uri.parse("//x.com/")) == Err(SchemeRelative)
expect Uri.require_scheme(Uri.parse("/path")) == Err(NoScheme)

expect Uri.port_or(Uri.parse("https://x.com:8080/"), 443) == Ok(8080)
expect Uri.port_or(Uri.parse("https://x.com/"), 443) == Ok(443)
expect Uri.port_or(Uri.parse("https://x.com:junk/"), 443) == Err(PortParseErr("junk"))

expect Uri.has_authority(Uri.parse("https://x.com/")) == True
expect Uri.has_authority(Uri.parse("//x.com/")) == True
expect Uri.has_authority(Uri.parse("file:///etc")) == True
expect Uri.has_authority(Uri.parse("mailto:a@b.com")) == False
expect Uri.has_authority(Uri.parse("/path")) == False
expect Uri.has_authority(Uri.parse("")) == False

# =============================================================================
# Tests: query_params
# =============================================================================

expect Uri.query_params(Uri.parse("https://x.com?a=1&b=2")) == [("a", "1"), ("b", "2")]

# Repeated keys and order are preserved. A Dict would collapse them.
expect Uri.query_params(Uri.parse("https://x.com?a=1&a=2")) == [("a", "1"), ("a", "2")]

# Keys and values are percent-decoded
expect Uri.query_params(Uri.parse("https://x.com?caf%C3%A9=du%20Monde")) == [("café", "du Monde")]

# A bare flag decodes to ("key", "")
expect Uri.query_params(Uri.parse("https://x.com?foo")) == [("foo", "")]

# Malformed escapes fall back to the raw text instead of failing
expect Uri.query_params(Uri.parse("https://x.com?a=%zz")) == [("a", "%zz")]

expect Uri.query_params(Uri.parse("https://x.com?")) == []
expect Uri.query_params(Uri.parse("https://x.com")) == []

# Empty pairs are skipped (WHATWG behavior). DECIDED, not incidental.
expect Uri.query_params(Uri.parse("https://x.com?a=1&&b=2")) == [("a", "1"), ("b", "2")]
expect Uri.query_params(Uri.parse("https://x.com?a=1&")) == [("a", "1")]
expect Uri.query_params(Uri.parse("https://x.com?&&")) == []

# =============================================================================
# Tests: parse_query
# =============================================================================

expect Uri.parse_query("a=1&b=2") == [("a", "1"), ("b", "2")]
expect Uri.parse_query("") == []

# The same rules query_params follows: repeated keys and order kept, bare flags
# become ("key", ""), keys and values decoded leniently, empty pairs skipped.
expect Uri.parse_query("a=1&a=2") == [("a", "1"), ("a", "2")]
expect Uri.parse_query("foo") == [("foo", "")]
expect Uri.parse_query("foo=") == [("foo", "")]
expect Uri.parse_query("caf%C3%A9=du%20Monde") == [("café", "du Monde")]
expect Uri.parse_query("a=%zz") == [("a", "%zz")]
expect Uri.parse_query("a=1&&b=2") == [("a", "1"), ("b", "2")]
expect Uri.parse_query("&&") == []

# A query lifted out of a Uri parses to the same pairs the Uri reports
expect Uri.parse_query("mode=ro&cache=shared") == Uri.query_params(Uri.parse("sqlite:///db.sqlite?mode=ro&cache=shared"))

# What append_param writes, query_params reads back. The old code broke this
# round-trip.
expect {
	u = Uri.append_param(Uri.append_param(Uri.parse("https://x.com"), "café", "du Monde"), "email", "hi@example.com")
	Uri.query_params(u) == [("café", "du Monde"), ("email", "hi@example.com")]
}

# =============================================================================
# Tests: append_path
# =============================================================================

expect Uri.to_str(Uri.append_path(Uri.parse("https://example.com"), ["some stuff"])) == "https://example.com/some%20stuff"

expect Uri.to_str(Uri.append_path(Uri.parse("https://example.com"), ["users", "posts"])) == "https://example.com/users/posts"

# A "/" inside a segment is data, not structure
expect Uri.to_str(Uri.append_path(Uri.parse("https://example.com"), ["a/b"])) == "https://example.com/a%2Fb"

# The easy mistake: query-shaped text in a segment becomes a visible, inert,
# encoded segment, NOT an injected query parameter
expect {
	u = Uri.append_path(Uri.parse("https://example.com"), ["report?id=1"])
	Uri.path(u) == "/report%3Fid%3D1" and Uri.query(u) == NoQuery
}

# No double slash when the path already ends with "/"
expect Uri.to_str(Uri.append_path(Uri.parse("https://example.com/things/"), ["stuff"])) == "https://example.com/things/stuff"

# Appending goes on the path even with a query and fragment present
expect Uri.to_str(Uri.append_path(Uri.parse("https://example.com?search=blah#frag"), ["stuff"])) == "https://example.com/stuff?search=blah#frag"

# ...even when the fragment contains a "?"
expect Uri.to_str(Uri.append_path(Uri.parse("https://x.com#frag?q"), ["stuff"])) == "https://x.com/stuff#frag?q"

# On a relative reference the first segment doesn't get a leading "/"
expect Uri.to_str(Uri.append_path(Uri.parse(""), ["a", "b"])) == "a/b"

# Empty list is a no-op
expect Uri.to_str(Uri.append_path(Uri.parse("https://example.com/things"), [])) == "https://example.com/things"

# An empty segment appends at most a trailing "/", and nothing at all if the
# path already ends in one
expect Uri.to_str(Uri.append_path(Uri.parse("https://x.com/a"), [""])) == "https://x.com/a/"
expect Uri.to_str(Uri.append_path(Uri.parse("https://x.com/a/"), [""])) == "https://x.com/a/"

# A URL with an unparseable port can still be written to, and the garbage
# port still round-trips
expect Uri.to_str(Uri.append_path(Uri.parse("https://x.com:junk"), ["p"])) == "https://x.com:junk/p"

# =============================================================================
# Tests: append_param / with_query / with_fragment
# =============================================================================

expect Uri.to_str(Uri.append_param(Uri.parse("https://example.com"), "email", "someone@example.com")) == "https://example.com?email=someone%40example.com"

expect {
	u = Uri.append_param(Uri.append_param(Uri.parse("https://example.com"), "café", "du Monde"), "email", "hi@example.com")
	Uri.to_str(u) == "https://example.com?caf%C3%A9=du%20Monde&email=hi%40example.com"
}

# append_param and with_query put the query before the fragment
expect Uri.to_str(Uri.append_param(Uri.parse("https://x.com#frag?q"), "k", "v")) == "https://x.com?k=v#frag?q"

# =============================================================================
# Tests: with_param
# =============================================================================

# Replaces in place, keeping the pair's position
expect Uri.to_str(Uri.with_param(Uri.parse("https://x.com?a=1&b=2"), "a", "9")) == "https://x.com?a=9&b=2"

# Appends when the key is absent
expect Uri.to_str(Uri.with_param(Uri.parse("https://x.com?a=1"), "b", "2")) == "https://x.com?a=1&b=2"

# ...and when there is no query at all
expect Uri.to_str(Uri.with_param(Uri.parse("https://x.com"), "a", "1")) == "https://x.com?a=1"

# Repeated keys: the first occurrence gets the value, the rest are dropped
expect Uri.to_str(Uri.with_param(Uri.parse("https://x.com?item=sword&coupon=yes&item=shield"), "item", "axe")) == "https://x.com?item=axe&coupon=yes"

# append_param always adds another pair, with_param replaces it. That's the
# whole difference.
expect {
	added = Uri.append_param(Uri.parse("https://x.com?a=1"), "a", "2")
	replaced = Uri.with_param(Uri.parse("https://x.com?a=1"), "a", "2")
	Uri.query_params(added) == [("a", "1"), ("a", "2")] and Uri.query_params(replaced) == [("a", "2")]
}

# A bare flag counts as a key
expect Uri.to_str(Uri.with_param(Uri.parse("https://x.com?debug&a=1"), "debug", "on")) == "https://x.com?debug=on&a=1"

# Keys are compared decoded, so what append_param wrote can be replaced
expect {
	u = Uri.append_param(Uri.parse("https://x.com"), "café", "one")
	Uri.query_params(Uri.with_param(u, "café", "two")) == [("café", "two")]
}

# Encoding matches append_param
expect Uri.to_str(Uri.with_param(Uri.parse("https://x.com"), "email", "someone@example.com")) == "https://x.com?email=someone%40example.com"

# Untouched pairs are preserved byte-exactly, including empty ones
expect Uri.to_str(Uri.with_param(Uri.parse("https://x.com?a=1&&b=2"), "c", "3")) == "https://x.com?a=1&&b=2&c=3"

# The query lands before the fragment
expect Uri.to_str(Uri.with_param(Uri.parse("https://x.com#frag"), "k", "v")) == "https://x.com?k=v#frag"

expect Uri.to_str(Uri.with_query(Uri.parse("https://example.com?key1=val1&key2=val2#stuff"), "newQuery=thisRightHere")) == "https://example.com?newQuery=thisRightHere#stuff"

# with_query("") removes the query
expect Uri.to_str(Uri.with_query(Uri.parse("https://example.com?key1=val1#stuff"), "")) == "https://example.com#stuff"

# with_query encodes "#" (it would shift the fragment boundary), while "="
# and "&" pass through as raw query content
expect {
	u = Uri.with_query(Uri.parse("https://x.com"), "a=1&b=c#d")
	Uri.query(u) == Query("a=1&b=c%23d") and Uri.fragment(u) == NoFragment
}

expect Uri.to_str(Uri.with_fragment(Uri.parse("https://example.com#stuff"), "things")) == "https://example.com#things"

expect Uri.to_str(Uri.with_fragment(Uri.parse("https://example.com"), "things")) == "https://example.com#things"

expect Uri.to_str(Uri.with_fragment(Uri.parse("https://example.com#stuff"), "")) == "https://example.com"

# with_fragment replaces the whole fragment, starting at the first "#"
expect Uri.to_str(Uri.with_fragment(Uri.parse("https://x.com/p#a#b"), "c")) == "https://x.com/p#c"

# =============================================================================
# Tests: with_userinfo
# =============================================================================

expect Uri.to_str(Uri.with_userinfo(Uri.parse("https://example.com/p"), "user")) == "https://user@example.com/p"

# ":" passes through for the usual user:password shape
expect Uri.to_str(Uri.with_userinfo(Uri.parse("https://example.com"), "user:pass")) == "https://user:pass@example.com"

# "@" and "/" are encoded so they can't shift the host or path boundary
expect {
	u = Uri.with_userinfo(Uri.parse("https://example.com/p"), "u@v/w")
	Uri.userinfo(u) == Userinfo("u%40v%2Fw") and Uri.parse(Uri.to_str(u)) == u
}

# Replaces an existing userinfo
expect Uri.to_str(Uri.with_userinfo(Uri.parse("https://old@example.com"), "new")) == "https://new@example.com"

# "" removes it
expect Uri.to_str(Uri.with_userinfo(Uri.parse("https://user:pass@example.com/p"), "")) == "https://example.com/p"

# Userinfo needs an authority, so an authority-less URL gains an empty-host one
expect {
	u = Uri.with_userinfo(Uri.parse("/path"), "user")
	Uri.host(u) == EmptyHost and Uri.to_str(u) == "//user@/path" and Uri.parse(Uri.to_str(u)) == u
}

# Building "https://user@host/" from scratch is possible now that the writer
# set covers every component
expect {
	result = Try.map_ok(
		Uri.with_scheme(Uri.empty, "https"),
		|u| Uri.to_str(Uri.with_userinfo(Uri.with_host(u, "example.com"), "user")),
	)
	result == Ok("https://user@example.com")
}

# =============================================================================
# Tests: with_path / with_host / with_port / with_scheme / empty
# =============================================================================

# with_path: "/" is structure, segments are encoded, leading/trailing "/" kept
expect {
	u = Uri.with_path(Uri.parse("https://example.com/old"), "/a b/c/")
	Uri.path(u) == "/a%20b/c/" and Uri.to_str(u) == "https://example.com/a%20b/c/"
}

# with_path on an authority URL: a rootless path gains a leading "/"
expect Uri.path(Uri.with_path(Uri.parse("https://example.com/old"), "x")) == "/x"

# with_host on a relative reference makes it scheme-relative (an authority
# requires "//") and the path gains its leading "/"
expect {
	u = Uri.with_host(Uri.parse("some/path"), "example.com")
	Uri.scheme(u) == SchemeRelative
		and Uri.host(u) == Host("example.com")
			and Uri.to_str(u) == "//example.com/some/path"
}

# with_host("") drops the whole authority: host, userinfo, and port
expect {
	u = Uri.with_host(Uri.parse("https://user@example.com:8080/p"), "")
	Uri.host(u) == NoHost and Uri.userinfo(u) == NoUserinfo and Uri.port(u) == Ok(NoPort) and Uri.to_str(u) == "https:/p"
}

# with_host on an opaque URL: the rootless path gains a "/" so the host and
# path can't merge on reparse
expect {
	u = Uri.with_host(Uri.parse("mailto:box"), "example.com")
	Uri.to_str(u) == "mailto://example.com/box"
}

# A bracketed IP-literal passes through with_host whole, without ":" encoding
expect {
	u = Uri.with_host(Uri.parse("https://example.com/p"), "[::1]")
	Uri.host(u) == Host("[::1]") and Uri.to_str(u) == "https://[::1]/p"
}

# with_host encodes boundary-shifting characters in a reg-name
expect Uri.host(Uri.with_host(Uri.parse("https://x.com/"), "a/b")) == Host("a%2Fb")

expect Uri.to_str(Uri.with_port(Uri.parse("https://example.com/p"), 8443)) == "https://example.com:8443/p"

# with_port replaces an unparseable port
expect Uri.port(Uri.with_port(Uri.parse("https://x.com:junk/"), 80)) == Ok(Port(80))

# with_port on an authority-less URL creates an empty-host authority since a
# port has nowhere else to serialize (RFC 3986 §3.2)
expect {
	u = Uri.with_port(Uri.parse("/path"), 80)
	Uri.host(u) == EmptyHost and Uri.to_str(u) == "//:80/path" and Uri.parse(Uri.to_str(u)) == u
}

expect {
	u = Uri.with_port(Uri.parse("mailto:a@b.com"), 80)
	Uri.host(u) == EmptyHost and Uri.to_str(u) == "mailto://:80/a@b.com" and Uri.parse(Uri.to_str(u)) == u
}

# =============================================================================
# Tests: RFC 3986 §3.3: no authority means the path cannot begin with "//"
# =============================================================================

# Dropping the host must not let a "//..." path masquerade as an authority: the
# path gains a "/." prefix at write time (the WHATWG serializer's fix,
# equivalent after dot-segment normalization)
expect {
	u = Uri.with_host(Uri.parse("https://h//p"), "")
	Uri.path(u) == "/.//p" and Uri.to_str(u) == "https:/.//p" and Uri.parse(Uri.to_str(u)) == u
}

expect {
	u = Uri.with_path(Uri.parse("mailto:x"), "//p")
	Uri.path(u) == "/.//p" and Uri.to_str(u) == "mailto:/.//p" and Uri.parse(Uri.to_str(u)) == u
}

# ...also on a scheme-less receiver
expect {
	u = Uri.with_path(Uri.parse(""), "//p")
	Uri.to_str(u) == "/.//p" and Uri.parse(Uri.to_str(u)) == u
}

# With an authority present a "//..." path is unambiguous and passes through
expect {
	u = Uri.with_path(Uri.parse("https://x.com"), "//p")
	Uri.path(u) == "//p" and Uri.to_str(u) == "https://x.com//p" and Uri.parse(Uri.to_str(u)) == u
}

expect Uri.to_str(Uri.without_port(Uri.parse("https://example.com:8080/p"))) == "https://example.com/p"

expect {
	result = Try.map_ok(Uri.with_scheme(Uri.parse("http://example.com/p"), "https"), Uri.to_str)
	result == Ok("https://example.com/p")
}

expect Uri.with_scheme(Uri.parse("https://x.com"), "ht tp") == Err(InvalidSchemeToken("ht tp"))

expect Uri.with_scheme(Uri.parse("https://x.com"), "1http") == Err(InvalidSchemeToken("1http"))

# Removing the scheme from an authority URL leaves it scheme-relative so the
# authority still serializes
expect {
	result = Try.map_ok(Uri.with_scheme(Uri.parse("https://x.com/p"), ""), Uri.to_str)
	result == Ok("//x.com/p")
}

# Removing the scheme from an opaque URL leaves a plain relative reference
expect {
	result = Try.map_ok(Uri.with_scheme(Uri.parse("mailto:a@b.com"), ""), |u| Uri.scheme(u))
	result == Ok(NoScheme)
}

# Building from scratch: empty + writers
expect {
	result = Try.map_ok(
		Uri.with_scheme(Uri.empty, "https"),
		|u| Uri.to_str(Uri.append_path(Uri.with_port(Uri.with_host(u, "api.example.com"), 8443), ["v1", "users"])),
	)
	result == Ok("https://api.example.com:8443/v1/users")
}

# =============================================================================
# Tests: round-trip property (component-exact)
# =============================================================================

expect Uri.to_str(Uri.parse("https://user:pass@example.com:8080/path?q=1#frag")) == "https://user:pass@example.com:8080/path?q=1#frag"
expect Uri.to_str(Uri.parse("//user@google.com:8080/x?q=1#frag")) == "//user@google.com:8080/x?q=1#frag"
expect Uri.to_str(Uri.parse("mailto:a@b.com")) == "mailto:a@b.com"
expect Uri.to_str(Uri.parse("urn:isbn:0")) == "urn:isbn:0"
expect Uri.to_str(Uri.parse("localhost:3000/x")) == "localhost:3000/x"
expect Uri.to_str(Uri.parse("docs/index.html")) == "docs/index.html"
expect Uri.to_str(Uri.parse("/path?#")) == "/path?#"
expect Uri.to_str(Uri.parse("http://[::1]:8080/")) == "http://[::1]:8080/"
expect Uri.to_str(Uri.parse("https://x.com/p?a=1?b=2#a#b")) == "https://x.com/p?a=1?b=2#a#b"

# =============================================================================
# Tests: writer reparse-stability: parse(to_str(w(u))) == w(u)
# =============================================================================

expect {
	u = Uri.append_path(Uri.parse("https://example.com/a"), ["b c", "d/e"])
	Uri.parse(Uri.to_str(u)) == u
}

expect {
	u = Uri.append_param(Uri.parse("https://x.com#frag?q"), "k&k", "v=v")
	Uri.parse(Uri.to_str(u)) == u
}

expect {
	u = Uri.with_query(Uri.parse("https://x.com/p#f"), "a=1&b=c#d")
	Uri.parse(Uri.to_str(u)) == u
}

expect {
	u = Uri.with_fragment(Uri.parse("https://x.com/p?q=1"), "sec?tion#2")
	Uri.parse(Uri.to_str(u)) == u
}

expect {
	u = Uri.with_path(Uri.parse("https://x.com/old?q#f"), "a b/c?d")
	Uri.parse(Uri.to_str(u)) == u
}

expect {
	u = Uri.with_host(Uri.parse("some/path"), "example.com")
	Uri.parse(Uri.to_str(u)) == u
}

expect {
	u = Uri.with_host(Uri.parse("mailto:box"), "h@x/y:1")
	Uri.parse(Uri.to_str(u)) == u
}

expect {
	u = Uri.with_host(Uri.parse("https://user@example.com:8080/p"), "")
	Uri.parse(Uri.to_str(u)) == u
}

expect {
	u = Uri.with_port(Uri.parse("https://x.com:junk/"), 80)
	Uri.parse(Uri.to_str(u)) == u
}

# =============================================================================
# Tests: percent_encode / percent_decode
# =============================================================================

expect Uri.percent_encode("Hello World") == "Hello%20World"
expect Uri.percent_encode("a+b&c=d") == "a%2Bb%26c%3Dd"
expect Uri.percent_encode("café") == "caf%C3%A9"
expect Uri.percent_encode("ABC123xyz") == "ABC123xyz"
expect Uri.percent_encode(":/@?#") == "%3A%2F%40%3F%23"
expect Uri.percent_encode("") == ""
expect Uri.percent_encode("100%") == "100%25"

# Regression: "-" is unreserved and must pass through (it was
# encoded to %2D by the old byte-150 bug)
expect Uri.percent_encode("a-b_c.d~e") == "a-b_c.d~e"

expect Uri.percent_decode("Hello%20World") == Ok("Hello World")
expect Uri.percent_decode("a%2Bb%26c%3Dd") == Ok("a+b&c=d")
expect Uri.percent_decode("caf%C3%A9") == Ok("café")
expect Uri.percent_decode("ABC123xyz") == Ok("ABC123xyz")
expect Uri.percent_decode("%3A%2F%40%3F%23") == Ok(":/@?#")
expect Uri.percent_decode("") == Ok("")
expect Uri.percent_decode("100%25") == Ok("100%")
expect Uri.percent_decode("%D0%9F%D1%80%D0%B8%D0%B2%D0%B5%D1%82") == Ok("Привет")
expect Uri.percent_decode("%E4%BD%A0%E5%A5%BD") == Ok("你好")
expect Uri.percent_decode("%zz") == Err(InvalidEncoding)

# A "%" that isn't itself the start of a valid escape is a malformed escape, not
# a silent drop. These once slipped through: a "%" encountered mid-escape reset
# the decoder to "start of escape" and dropped the partial escape's bytes, so a
# truncated escape followed by another "%" decoded successfully instead of failing.
expect Uri.percent_decode("%%20") == Err(InvalidEncoding)
expect Uri.percent_decode("%2%20") == Err(InvalidEncoding)
expect Uri.percent_decode("%2%") == Err(InvalidEncoding)
expect Uri.percent_decode("50%%") == Err(InvalidEncoding)

# A "%" that IS a valid escape is still fine (regression guard for the fix)
expect Uri.percent_decode("100%25") == Ok("100%")
expect Uri.percent_decode("%2525") == Ok("%25")

# encode |> decode is the identity
expect Uri.percent_decode(Uri.percent_encode("a-b c/d?e#f&g=h%i~j")) == Ok("a-b c/d?e#f&g=h%i~j")

# Encode then decode is the identity for every codepoint value 0-255,
# covering every ASCII byte's encode classification plus the multi-byte UTF-8
# path. (A raw 256-BYTE sweep is impossible through the Str API: bytes like
# 0xC0 or 0xFF never occur in valid UTF-8.)
expect {
	codepoints = List.map_with_index(List.repeat(0.U8, 256), |_, i| i)
	List.all(
		codepoints,
		|c| {
			utf8 = 
				if c < 128 {
					[c.to_u8_wrap()]
				} else {
					[(192 + (c // 64)).to_u8_wrap(), (128 + (c % 64)).to_u8_wrap()]
				}
			match Str.from_utf8(utf8) {
				Ok(s) => Uri.percent_decode(Uri.percent_encode(s)) == Ok(s)
				Err(_) => False
			}
		},
	)
}

# =============================================================================
# Tests: writer reparse-stability matrix: parse(to_str(w(u))) == w(u)
# for every writer over a matrix of receiver states
# =============================================================================

# Receiver states: absent/empty/present fields, userinfo, garbage port, opaque
# and scheme-less forms, and a "//"-headed path (which must never be able to
# masquerade as an authority after a write).
reparse_matrix : List(Uri)
reparse_matrix = List.map(
	[
		"https://user:pass@example.com:8080/path?q=1#frag",
		"",
		"mailto:a@b.com",
		"x.com:",
		"localhost:3000/x",
		"some/path",
		"/path",
		"#frag",
		"//host/x",
		"https://",
		"https://user@",
		"https://:3000/",
		"https://x.com:junk/",
		"https://h//p",
	],
	Uri.parse,
)

reparse_stable : Uri -> Bool
reparse_stable = |u| Uri.parse(Uri.to_str(u)) == u

# append_path with boundary-shifting segments: space, "/", "?", "#", "&", empty
expect List.all(reparse_matrix, |u| reparse_stable(Uri.append_path(u, ["b c", "d/e", "?x", "#y", "&z", ""])))

# append_param with "&", "#", "=", "?" in key and value
expect List.all(reparse_matrix, |u| reparse_stable(Uri.append_param(u, "k&k#", "v=v?")))

# with_param with the same adversarial key and value, both replacing and adding
expect {
	keys = ["k&k#", "q", "item"]
	List.all(reparse_matrix, |u| List.all(keys, |k| reparse_stable(Uri.with_param(u, k, "v=v?"))))
}

# with_query: "#" would shift the fragment boundary, and "" removes the query
expect {
	queries = ["a=1&b=c#d", ""]
	List.all(reparse_matrix, |u| List.all(queries, |q| reparse_stable(Uri.with_query(u, q))))
}

# with_fragment: "?" and "#" inside the fragment, and "" removes it
expect {
	fragments = ["sec?tion#2", ""]
	List.all(reparse_matrix, |u| List.all(fragments, |f| reparse_stable(Uri.with_fragment(u, f))))
}

# with_path: rootless, rooted, "//"-headed, and empty paths
expect {
	paths = ["a b/c?d", "x", "/x", "//p", "///", ""]
	List.all(reparse_matrix, |u| List.all(paths, |p| reparse_stable(Uri.with_path(u, p))))
}

# with_userinfo: plain, user:pass, boundary-shifting garbage, and removal
expect {
	infos = ["user", "user:pass", "u@v/w?#", ""]
	List.all(reparse_matrix, |u| List.all(infos, |ui| reparse_stable(Uri.with_userinfo(u, ui))))
}

# with_host: reg-name, IP-literal, garbage full of "@" "/" ":", and removal
expect {
	hosts = ["example.com", "[::1]", "h@x/y:1", ""]
	List.all(reparse_matrix, |u| List.all(hosts, |h| reparse_stable(Uri.with_host(u, h))))
}

expect List.all(reparse_matrix, |u| reparse_stable(Uri.with_port(u, 80)))

expect List.all(reparse_matrix, |u| reparse_stable(Uri.without_port(u)))

# with_scheme: set and remove. A valid token must never be rejected.
expect {
	schemes = ["wss", ""]
	List.all(
		reparse_matrix,
		|u|
			List.all(
				schemes,
				|s|
					match Uri.with_scheme(u, s) {
						Ok(w) => reparse_stable(w)
						Err(_) => False
					},
			),
	)
}

# =============================================================================
# Tests: with_host / with_userinfo full-encode a literal "%"
# =============================================================================

# A literal "%" is fully encoded (no pre-encoded passthrough), so a host with a
# "%" round-trips like every other writer input instead of reparsing as a
# malformed escape
expect Uri.host(Uri.with_host(Uri.parse("https://x.com/"), "100%")) == Host("100%25")
expect Uri.userinfo(Uri.with_userinfo(Uri.parse("https://x.com"), "50%")) == Userinfo("50%25")

expect {
	u = Uri.with_host(Uri.parse("https://x.com/"), "a%b")
	Uri.parse(Uri.to_str(u)) == u
}

expect {
	u = Uri.with_userinfo(Uri.parse("https://x.com/"), "user%name")
	Uri.parse(Uri.to_str(u)) == u
}

# =============================================================================
# Tests: resolve. The RFC 3986 §5.4 reference-resolution vectors, verbatim
# =============================================================================

resolve_base : Uri
resolve_base = Uri.parse("http://a/b/c/d;p?q")

resolves_to : Str, Str -> Bool
resolves_to = |reference, expected|
	Uri.to_str(Uri.resolve(resolve_base, Uri.parse(reference))) == expected

# §5.4.1 Normal examples
expect resolves_to("g:h", "g:h")
expect resolves_to("g", "http://a/b/c/g")
expect resolves_to("./g", "http://a/b/c/g")
expect resolves_to("g/", "http://a/b/c/g/")
expect resolves_to("/g", "http://a/g")
expect resolves_to("//g", "http://g")
expect resolves_to("?y", "http://a/b/c/d;p?y")
expect resolves_to("g?y", "http://a/b/c/g?y")
expect resolves_to("#s", "http://a/b/c/d;p?q#s")
expect resolves_to("g#s", "http://a/b/c/g#s")
expect resolves_to("g?y#s", "http://a/b/c/g?y#s")
expect resolves_to(";x", "http://a/b/c/;x")
expect resolves_to("g;x", "http://a/b/c/g;x")
expect resolves_to("g;x?y#s", "http://a/b/c/g;x?y#s")
expect resolves_to("", "http://a/b/c/d;p?q")
expect resolves_to(".", "http://a/b/c/")
expect resolves_to("./", "http://a/b/c/")
expect resolves_to("..", "http://a/b/")
expect resolves_to("../", "http://a/b/")
expect resolves_to("../g", "http://a/b/g")
expect resolves_to("../..", "http://a/")
expect resolves_to("../../", "http://a/")
expect resolves_to("../../g", "http://a/g")

# §5.4.2 Abnormal examples: ".." that would climb past the root are discarded
expect resolves_to("../../../g", "http://a/g")
expect resolves_to("../../../../g", "http://a/g")

# ".." and "." that are only part of a segment are NOT dot-segments
expect resolves_to("/./g", "http://a/g")
expect resolves_to("/../g", "http://a/g")
expect resolves_to("g.", "http://a/b/c/g.")
expect resolves_to(".g", "http://a/b/c/.g")
expect resolves_to("g..", "http://a/b/c/g..")
expect resolves_to("..g", "http://a/b/c/..g")

# Dot segments in the middle of a path resolve normally
expect resolves_to("./../g", "http://a/b/g")
expect resolves_to("./g/.", "http://a/b/c/g/")
expect resolves_to("g/./h", "http://a/b/c/g/h")
expect resolves_to("g/../h", "http://a/b/c/h")
expect resolves_to("g;x=1/./y", "http://a/b/c/g;x=1/y")
expect resolves_to("g;x=1/../y", "http://a/b/c/y")

# Dot sequences inside the query or fragment are left untouched
expect resolves_to("g?y/./x", "http://a/b/c/g?y/./x")
expect resolves_to("g?y/../x", "http://a/b/c/g?y/../x")
expect resolves_to("g#s/./x", "http://a/b/c/g#s/./x")
expect resolves_to("g#s/../x", "http://a/b/c/g#s/../x")

# A scheme-relative reference borrows the base's scheme
expect Uri.to_str(Uri.resolve(Uri.parse("https://a/b/c"), Uri.parse("//g/x"))) == "https://g/x"

# A reference with its own scheme is used as-is (its dot segments still removed)
expect Uri.to_str(Uri.resolve(resolve_base, Uri.parse("http://z/p/../q"))) == "http://z/q"

# Resolution is strict: a same-scheme reference is still treated as absolute,
# not merged against the base's path
expect Uri.to_str(Uri.resolve(resolve_base, Uri.parse("http:g"))) == "http:g"
