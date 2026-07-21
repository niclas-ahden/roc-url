import Uri

## A web URL, checked and normalized at construction. Every [Url] has an
## http or https scheme, a non-empty host, and a port that is a [U16] or
## absent. Everything else (mailto: links, relative references, unparseable
## ports) is [Uri]'s job, and you can convert back and forth with [Url.to_uri]
## and [Url.from_uri].
##
## Parse with [Url.parse] when the string is supposed to be a URL already,
## with [Url.parse_leniently] when it is URL-ish text from the wild (user
## input, scraped hrefs, config fields), or with [Url.from_uri] when you
## have a [Uri]. [Url.join] follows a reference from a base and checks the
## result, so following links cannot take you off the web.
##
## Every constructor normalizes: the scheme and host are lowercased, a
## default port (80 for http, 443 for https) is dropped, an empty path
## becomes "/", dot segments are resolved, and percent escapes are
## normalized (hex digits uppercased, escapes of unreserved characters
## decoded, so `%2f` becomes `%2F` and `%7E` becomes `~`). That is RFC 3986
## §6.2 normalization, so two spellings the RFC calls equivalent are equal:
## `Url.parse("HTTP://X.com:80/a/../%62")` equals `Url.parse("http://x.com/b")`.
##
## Writers cover every component and renormalize whatever they touch, so a
## written [Url] is exactly the [Url] that parsing its [Url.to_str] would
## give. Most writers are total. The two calls that could break a check,
## [Url.with_host] and [Url.append_path], return a [Try] instead.
##
## Out of scope on purpose: IDNA (`bücher.de` and its punycode form are
## different hosts here) and any validation of host bytes beyond
## non-emptiness. Those are policy, and policy belongs in your application.
## The userinfo is kept and serialized so nothing is dropped silently, but
## there is no accessor for it and [Url.origin] never includes it.
Url := [
	Url(
		{
			scheme : [Http, Https],
			userinfo : [Userinfo(Str), NoUserinfo],
			host : Str,
			port : [Port(U16), NoPort],
			path : Str,
			query : [Query(Str), EmptyQuery, NoQuery],
			fragment : [Fragment(Str), EmptyFragment, NoFragment],
		},
	),
].{

	## Structural equality, derived. Two [Url]s are equal when every
	## normalized component is, which is RFC 3986 §6.2 equivalence because
	## normalization already ran at construction.
	is_eq : _

	## Checks a [Uri] and turns it into a [Url]. Every constructor goes
	## through this function. Checks run in component order and the first
	## failure wins: the scheme must be http or https (in any case), the
	## host must be present and non-empty, and the port must have parsed.
	## `NotWebScheme` and `PortInvalid` carry the raw text so you can say
	## what was wrong.
	##
	## ```
	## # Gives Err(NotWebScheme("mailto"))
	## Url.from_uri(Uri.parse("mailto:a@b.com"))
	## ```
	from_uri : Uri -> Try(Url, [NoScheme, SchemeRelative, NotWebScheme(Str), NoHost, EmptyHost, PortInvalid(Str)])
	from_uri = |uri|
		match Uri.scheme(uri) {
			NoScheme => Err(NoScheme)
			SchemeRelative => Err(SchemeRelative)
			Scheme(raw_scheme) => {
				is_http = Str.caseless_ascii_equals(raw_scheme, "http")
				if is_http or Str.caseless_ascii_equals(raw_scheme, "https") {
					sch = if is_http {
						Http
					} else {
						Https
					}
					match Uri.host(uri) {
						NoHost => Err(NoHost)
						EmptyHost => Err(EmptyHost)
						Host(raw_host) =>
							match Uri.port(uri) {
								Err(PortParseErr(raw)) => Err(PortInvalid(raw))
								Ok(port_field) => {
									port_norm = 
										match port_field {
											NoPort => NoPort
											Port(n) =>
												if n == scheme_default_port(sch) {
													NoPort
												} else {
													Port(n)
												}
											}
									userinfo_norm = 
										match Uri.userinfo(uri) {
											Userinfo(ui) => Userinfo(normalize_escapes(ui))
											NoUserinfo => NoUserinfo
										}
									query_norm = 
										match Uri.query(uri) {
											Query(q) => Query(normalize_escapes(q))
											other => other
										}
									fragment_norm = 
										match Uri.fragment(uri) {
											Fragment(f) => Fragment(normalize_escapes(f))
											other => other
										}
									Ok(
										Url({
											scheme: sch,
											userinfo: userinfo_norm,
											host: lower_host(normalize_escapes(raw_host)),
											port: port_norm,
											path: normalize_path(normalize_escapes(Uri.path(uri))),
											query: query_norm,
											fragment: fragment_norm,
										}),
									)
								}
							}
						}
				} else {
					Err(NotWebScheme(raw_scheme))
				}
			}
		}

	## Parses a [Str] into a [Url]. The input must already be a
	## full web URL: `https://example.com/x` passes, while `example.com`,
	## `//host/x`, and `mailto:...` fail with a tag naming what is missing or
	## wrong. Use this for strings that are supposed to be URLs (config
	## values, API responses, HTML attributes), where a mistake should be
	## an error rather than something to repair. For text that only looks
	## like an address, see [Url.parse_leniently].
	##
	## ```
	## # Gives Err(NotWebScheme("localhost")) since RFC 3986 reads
	## # "localhost" as a scheme here. parse_leniently reads it as a host.
	## Url.parse("localhost:3000/x")
	## ```
	parse : Str -> Try(Url, [NoScheme, SchemeRelative, NotWebScheme(Str), NoHost, EmptyHost, PortInvalid(Str)])
	parse = |input| from_uri(Uri.parse(input))

	## Parses URL-ish text the way a browser's address bar would, then runs
	## the strict checks. Use it for text from the wild: user input, scraped
	## hrefs, CSV columns. If the input was already supposed to be a URL, use
	## [Url.parse] so mistakes surface instead of being repaired.
	##
	## The repair rule, in order: trim whitespace from both ends,
	## percent-encode bytes no URI can contain (interior spaces, controls,
	## and ASCII delimiters like `<`, `|`, and `{`), prepend `https:` to a
	## `//host` input, pass anything with a scheme through untouched (http
	## and https run the checks as-is, another scheme with `//` fails as
	## `NotWebScheme`), read `scheme:digits` as a host and port
	## (`localhost:3000` becomes `https://localhost:3000`), reject
	## `scheme:non-digits` (`mailto:x`, `javascript:...`) as `NotWebScheme`,
	## and prepend `https://` to everything else. The assumed scheme is
	## always https.
	##
	## Every valid URI that [Url.parse] accepts, this accepts with the
	## identical result. The two can only disagree on input that is not a
	## URI at all: surrounding whitespace (strict parse keeps it, this
	## trims it) and forbidden raw bytes (strict parse passes them through
	## as data, this encodes them the way an address bar would). A raw `%`
	## is left alone, so an already-encoded `a%20b` is not double-encoded.
	##
	## ```
	## # Gives Ok of https://example.com/
	## Url.parse_leniently("example.com")
	##
	## # Gives Ok of https://localhost:3000/x
	## Url.parse_leniently("localhost:3000/x")
	##
	## # Gives Ok of https://example.com/a%20b
	## Url.parse_leniently("example.com/a b")
	## ```
	parse_leniently : Str -> Try(Url, [NoScheme, SchemeRelative, NotWebScheme(Str), NoHost, EmptyHost, PortInvalid(Str)])
	parse_leniently = |input| {
		trimmed = Str.trim(input)
		if Str.is_empty(trimmed) {
			Err(NoScheme)
		} else {
			repaired = encode_forbidden(trimmed)
			uri = Uri.parse(repaired)
			match Uri.scheme(uri) {
				SchemeRelative => from_uri(Uri.parse(Str.concat("https:", repaired)))
				NoScheme =>
				# A rooted path has no host to guess, so it stays an error.
					if Str.starts_with(repaired, "/") {
						Err(NoScheme)
					} else {
						from_uri(Uri.parse(Str.concat("https://", repaired)))
					}

				Scheme(raw_scheme) => {
					if Str.caseless_ascii_equals(raw_scheme, "http")
						or Str.caseless_ascii_equals(raw_scheme, "https") {
						# A web scheme goes through the strict checks so
						# "http:x" fails as NoHost, never as NotWebScheme.
						from_uri(uri)
					} else if Uri.has_authority(uri) {
						# A full "scheme://..." said what it meant.
						Err(NotWebScheme(raw_scheme))
					} else if starts_with_digit(Uri.path(uri)) {
						# "localhost:3000/x" and friends: the human meant
						# host:port, so read it that way.
						from_uri(Uri.parse(Str.concat("https://", repaired)))
					} else {
						# "mailto:x", "javascript:...": a scheme with a
						# non-digit body means what it says, and it isn't web.
						Err(NotWebScheme(raw_scheme))
					}
				}
			}
		}
	}

	## Resolves a reference against this base (RFC 3986 §5), then checks
	## the result. Following a link can never hand you a non-web value: a
	## reference that resolves to `mailto:x` or to an empty host fails. The
	## base always has a scheme and a host, so the RFC's undefined
	## relative-base case cannot come up.
	##
	## ```
	## # Gives Ok of https://example.com/a/g
	## Try.map_ok(Url.parse("https://example.com/a/b/c"), |base| Url.join(base, "../g"))
	## ```
	join : Url, Str -> Try(Url, [NoScheme, SchemeRelative, NotWebScheme(Str), NoHost, EmptyHost, PortInvalid(Str)])
	join = |url, reference| from_uri(Uri.resolve(to_uri(url), Uri.parse(reference)))

	## The [Url] as a [Uri], for anything the checked surface does not
	## offer: reading the userinfo, the writers, lossless component work.
	## Validate back with [Url.from_uri] when you are done.
	to_uri : Url -> Uri
	to_uri = |url| Uri.parse(to_str(url))

	## Serializes the [Url]. The output is normalized: lowercase scheme and
	## host, no default port, a rooted path, dot segments resolved, percent
	## escapes in canonical form. Raw bytes are not re-encoded, so a raw
	## space in a parsed path stays a raw space. Userinfo is included when
	## present, so nothing is dropped silently.
	to_str : Url -> Str
	to_str = |Url(u)| {
		scheme_str = scheme_to_str(u.scheme)
		userinfo_str = 
			match u.userinfo {
				Userinfo(ui) => Str.concat(ui, "@")
				NoUserinfo => ""
			}
		port_str = 
			match u.port {
				Port(p) => Str.concat(":", p.to_str())
				NoPort => ""
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
		host_str = u.host
		path_str = u.path
		"${scheme_str}://${userinfo_str}${host_str}${port_str}${path_str}${query_str}${fragment_str}"
	}

	## The scheme. A closed union, so there is no "other" case to handle.
	scheme : Url -> [Http, Https]
	scheme = |Url(u)| u.scheme

	## The host. Guaranteed non-empty, ASCII-lowercased, and
	## escape-normalized, and otherwise whatever bytes the input had: no
	## IDNA, no validation. `[::1]` comes back with its brackets.
	host : Url -> Str
	host = |Url(u)| u.host

	## The port to connect to: the explicit port, or the scheme's default (80
	## for http, 443 for https) when none was written. See [Url.explicit_port]
	## for the distinction.
	port : Url -> U16
	port = |Url(u)|
		match u.port {
			Port(p) => p
			NoPort => scheme_default_port(u.scheme)
		}

	## The port as written: `Port(8080)` only when an explicit, non-default
	## port was given. A default port is normalized away at construction, so
	## `https://x.com:443/` gives `NoPort` here (and 443 from [Url.port]).
	explicit_port : Url -> [Port(U16), NoPort]
	explicit_port = |Url(u)| u.port

	## The path. Guaranteed to start with `/` (an empty path normalizes to
	## `/` at construction), with dot segments already resolved.
	path : Url -> Str
	path = |Url(u)| u.path

	## The raw query, the part after `?`, not decoded. `EmptyQuery` (a bare
	## trailing `?`) is distinct from `NoQuery` (no `?` at all). See
	## [Url.query_params] for decoded key/value pairs.
	query : Url -> [Query(Str), EmptyQuery, NoQuery]
	query = |Url(u)| u.query

	## The query as decoded key/value pairs, preserving repeated keys and
	## order (which a `Dict` would collapse). Same rules as
	## [Uri.query_params]: keys and values are percent-decoded leniently, a
	## bare flag (`?foo`) and an empty value (`?foo=`) both yield
	## `("foo", "")`, and empty pairs (`?a=1&&b=2`) are skipped.
	##
	## ```
	## # Gives [("item", "sword"), ("item", "shield")] inside the Ok
	## Try.map_ok(Url.parse("https://shop.example/cart?item=sword&item=shield"), Url.query_params)
	## ```
	query_params : Url -> List((Str, Str))
	query_params = |url| Uri.query_params(to_uri(url))

	## Sets a query parameter, add-or-replace: the first occurrence of `key`
	## gets the new value in place, other occurrences of `key` are dropped,
	## and a missing `key` is appended. Key and value are percent-encoded
	## for you, same rules as [Uri.with_param]. Only the query changes, so
	## every [Url] guarantee holds as before. See [Url.append_param] when
	## you want another pair regardless.
	##
	## ```
	## # Gives "https://x.example/?item=axe&coupon=yes" inside the Ok
	## Try.map_ok(Url.parse("https://x.example/?item=sword&coupon=yes"), |u| Url.to_str(Url.with_param(u, "item", "axe")))
	## ```
	with_param : Url, Str, Str -> Url
	with_param = |Url(u), key, value| {
		updated = Uri.with_param(to_uri(Url(u)), key, value)
		Url({ ..u, query: Uri.query(updated) })
	}

	## Adds a query parameter, always as another pair, even when the key is
	## already present. Key and value are percent-encoded for you. See
	## [Url.with_param] for add-or-replace.
	append_param : Url, Str, Str -> Url
	append_param = |Url(u), key, value| {
		updated = Uri.append_param(to_uri(Url(u)), key, value)
		Url({ ..u, query: Uri.query(updated) })
	}

	# ---------------------------------------------------------------------------
	# Writers: Uri's writers, mirrored onto the checked layer. Every writer
	# renormalizes what it touches, so a written Url is exactly the Url
	# that parsing its to_str would give. The writers that cannot fail a
	# check are total. with_host and append_path return a Try instead of
	# breaking a guarantee.
	# ---------------------------------------------------------------------------

	## Sets the scheme. The argument is the same closed union [Url.scheme]
	## answers, so unlike [Uri.with_scheme] there is no string to validate
	## and nothing to fail. The port is renormalized against the new
	## scheme's default: switching `http://x.example:443/` to `Https` drops
	## the now-default port, and a port that was already normalized away
	## stays away, so switching `http://x.example/` to `Https` means
	## [Url.port] answers 443, not 80.
	##
	## ```
	## # Gives Ok of https://x.example/cart
	## Try.map_ok(Url.parse("http://x.example:443/cart"), |u| Url.with_scheme(u, Https))
	## ```
	with_scheme : Url, [Http, Https] -> Url
	with_scheme = |Url(u), sch| {
		new_port = 
			match u.port {
				Port(p) =>
					if p == scheme_default_port(sch) {
						NoPort
					} else {
						Port(p)
					}

				NoPort => NoPort
			}
		Url({ ..u, scheme: sch, port: new_port })
	}

	## Sets an explicit port. The scheme's default (80 for http, 443 for
	## https) is normalized away, same as at construction, so
	## `with_port(u, 443)` on an https [Url] leaves [Url.explicit_port]
	## answering `NoPort` (and [Url.port] answering 443).
	with_port : Url, U16 -> Url
	with_port = |Url(u), p|
		if p == scheme_default_port(u.scheme) {
			Url({ ..u, port: NoPort })
		} else {
			Url({ ..u, port: Port(p) })
		}

	## Removes the explicit port, so [Url.port] answers the scheme's
	## default again.
	without_port : Url -> Url
	without_port = |Url(u)| Url({ ..u, port: NoPort })

	## Sets the host. The empty string is the one argument that cannot make
	## a [Url] (a web URL needs a host), so it is refused as `EmptyHost`.
	## Everything else works: a `[...]`-shaped argument passes through whole
	## as an IP-literal, and any other argument is percent-encoded where it
	## falls outside RFC 3986 `reg-name` (same as [Uri.with_host]), so an
	## `@`, `:`, or `/` cannot shift a boundary. The new host is then
	## normalized like a parsed one: escapes canonicalized, then
	## ASCII-lowercased.
	##
	## ```
	## # Gives Ok of https://api.example.com/x inside the Ok
	## Try.map_ok(Url.parse("https://STAGING.example.com/x"), |u| Url.with_host(u, "API.example.com"))
	## ```
	with_host : Url, Str -> Try(Url, [EmptyHost])
	with_host = |Url(u), host_str|
		if Str.is_empty(host_str) {
			Err(EmptyHost)
		} else {
			encoded = 
				if Str.starts_with(host_str, "[") and Str.ends_with(host_str, "]") {
					host_str
				} else {
					Uri.encode_reg_name(host_str)
				}
			Ok(Url({ ..u, host: lower_host(normalize_escapes(encoded)) }))
		}

	## Replaces the whole path, then normalizes it the way a parse would.
	## `/` is structure and passes through, and every segment between
	## slashes is fully percent-encoded (same as [Uri.with_path]), so a `?`
	## or `#` in a segment cannot shift a boundary. A rootless path gains
	## its leading `/`, the empty string becomes `/`, and dot segments are
	## resolved, because `..` is path syntax here: `with_path(u, "/a/../b")`
	## means `/b`, exactly as it does in a parsed string. Use
	## [Url.append_path] to treat segments as opaque data instead.
	with_path : Url, Str -> Url
	with_path = |Url(u), path_str| {
		encoded = Str.join_with(List.map(Str.split_on(path_str, "/"), Uri.percent_encode), "/")
		rooted = 
			if Str.is_empty(encoded) or Str.starts_with(encoded, "/") {
				encoded
			} else {
				Str.concat("/", encoded)
			}
		Url({ ..u, path: normalize_path(rooted) })
	}

	## Appends path segments. Each element is exactly ONE segment of opaque
	## data and is fully percent-encoded, including `/`, `?`, `#`, and `&`
	## (same as [Uri.append_path]), so there is no way to smuggle structure
	## in. A normalized path cannot hold a literal `"."` or `".."` segment,
	## and resolving one away would make an "append" delete data, so those
	## two values are refused as `DotSegment` instead. When you mean
	## navigation, say so with [Url.join].
	##
	## ```
	## # Gives "https://api.example/v1/users/a%40b.se" inside the Oks
	## Try.map_ok(Url.parse("https://api.example/v1"), |u| Try.map_ok(Url.append_path(u, ["users", "a@b.se"]), Url.to_str))
	## ```
	append_path : Url, List(Str) -> Try(Url, [DotSegment(Str)])
	append_path = |url, segments| {
		dots = List.drop_if(segments, |s| s != "." and s != "..")
		match List.first(dots) {
			Ok(bad) => Err(DotSegment(bad))
			Err(_) =>
				Ok(
					List.fold(
						segments,
						url,
						|Url(u), segment| {
							encoded = Uri.percent_encode(segment)
							new_path = 
								if Str.ends_with(u.path, "/") {
									Str.concat(u.path, encoded)
								} else {
									Str.concat(u.path, Str.concat("/", encoded))
								}
							Url({ ..u, path: new_path })
						},
					),
				)
			}
	}

	## Replaces the raw query string. Passing `""` removes the query. Same
	## rules as [Uri.with_query]: only `#` is percent-encoded (it would
	## shift the fragment boundary), while `=`, `&`, and `?` are legitimate
	## raw query content and pass through. Escapes in the argument are
	## normalized, so equality keeps meaning RFC-equivalence.
	with_query : Url, Str -> Url
	with_query = |Url(u), query_str|
		if Str.is_empty(query_str) {
			Url({ ..u, query: NoQuery })
		} else {
			Url({ ..u, query: Query(normalize_escapes(Uri.encode_hashes(query_str))) })
		}

	## Replaces the fragment. Passing `""` removes it. Nothing is encoded
	## (the fragment is last, so any content round-trips, same as
	## [Uri.with_fragment]), but escapes in the argument are normalized so
	## equality keeps meaning RFC-equivalence.
	with_fragment : Url, Str -> Url
	with_fragment = |Url(u), fragment_str|
		if Str.is_empty(fragment_str) {
			Url({ ..u, fragment: NoFragment })
		} else {
			Url({ ..u, fragment: Fragment(normalize_escapes(fragment_str)) })
		}

	## Sets the userinfo, or removes it when passed `""`. Everything
	## outside RFC 3986 `userinfo` except `:` is percent-encoded (same as
	## [Uri.with_userinfo]), so an `@` cannot shift the host boundary.
	## Userinfo stays write-only on [Url]: there is no accessor, and
	## [Url.origin] never includes it.
	with_userinfo : Url, Str -> Url
	with_userinfo = |Url(u), userinfo_str|
		if Str.is_empty(userinfo_str) {
			Url({ ..u, userinfo: NoUserinfo })
		} else {
			Url({ ..u, userinfo: Userinfo(Uri.encode_userinfo(userinfo_str)) })
		}

	## The fragment, the part after the first `#`. `EmptyFragment` (a bare
	## trailing `#`) is distinct from `NoFragment`.
	fragment : Url -> [Fragment(Str), EmptyFragment, NoFragment]
	fragment = |Url(u)| u.fragment

	## The origin: scheme, host, and explicit port, as in
	## `https://example.com:8443`. Never includes userinfo, so an origin is
	## always safe to log or compare.
	origin : Url -> Str
	origin = |Url(u)| {
		scheme_str = scheme_to_str(u.scheme)
		port_str = 
			match u.port {
				Port(p) => Str.concat(":", p.to_str())
				NoPort => ""
			}
		host_str = u.host
		"${scheme_str}://${host_str}${port_str}"
	}

	# ---------------------------------------------------------------------------
	# Internal helpers
	# ---------------------------------------------------------------------------

	## Internal helper
	scheme_to_str : [Http, Https] -> Str
	scheme_to_str = |sch|
		match sch {
			Http => "http"
			Https => "https"
		}

	## Internal helper
	scheme_default_port : [Http, Https] -> U16
	scheme_default_port = |sch|
		match sch {
			Http => 80
			Https => 443
		}

	## Internal helper. An empty path becomes "/" and dot segments are
	## resolved, so a constructed Url is always rooted and canonical.
	normalize_path : Str -> Str
	normalize_path = |p|
		if Str.is_empty(p) {
			"/"
		} else {
			Uri.remove_dot_segments(p)
		}

	## Internal helper. RFC 3986 §6.2.2 percent-encoding normalization: the
	## hex digits of a valid escape are uppercased, and an escape that
	## encodes an unreserved character (A-Z, a-z, 0-9, "-", ".", "_", "~")
	## is decoded. Escapes of reserved characters stay escapes, so encoding
	## still cannot change structure, and malformed escapes ("%zz", a
	## truncated "%2") pass through verbatim. Runs before dot-segment
	## resolution, so a decoded "%2E" takes part in it.
	normalize_escapes : Str -> Str
	normalize_escapes = |s| {
		folded = List.fold(
			Str.to_utf8(s),
			{ out: [], pending: Step },
			|state, byte|
				match state.pending {
					Step => step_plain(state.out, byte)
					TakeFirst =>
						match Uri.hex_char_to_decimal(byte) {
							Ok(first_val) => { out: state.out, pending: TakeSecond({ raw: byte, val: first_val }) }
							Err(_) => step_plain(List.append(state.out, '%'), byte)
						}

					TakeSecond(first) =>
						match Uri.hex_char_to_decimal(byte) {
							Ok(second_val) => {
								decoded = first.val * 16 + second_val
								emitted = 
									if Uri.is_unreserved(decoded) {
										List.append(state.out, decoded)
									} else {
										List.concat(state.out, ['%', upper_hex(first.raw), upper_hex(byte)])
									}
								{ out: emitted, pending: Step }
							}

							Err(_) => step_plain(List.concat(state.out, ['%', first.raw]), byte)
						}
					},
		)
		finished = 
			match folded.pending {
				Step => folded.out
				TakeFirst => List.append(folded.out, '%')
				TakeSecond(first) => List.concat(folded.out, ['%', first.raw])
			}
		Try.ok_or(Str.from_utf8(finished), s)
	}

	## Internal helper for normalize_escapes. Consumes one byte outside an
	## escape: a "%" opens an escape, anything else is emitted as-is.
	step_plain : List(U8), U8 -> { out : List(U8), pending : [Step, TakeFirst, TakeSecond({ raw : U8, val : U8 })] }
	step_plain = |out, byte|
		if byte == '%' {
			{ out: out, pending: TakeFirst }
		} else {
			{ out: List.append(out, byte), pending: Step }
		}

	## Internal helper. Uppercases one ASCII hex digit.
	upper_hex : U8 -> U8
	upper_hex = |byte|
		if byte >= 'a' and byte <= 'f' {
			byte - 32
		} else {
			byte
		}

	## Internal helper. ASCII-lowercases a host without touching the hex
	## digits of valid percent escapes, so the uppercase hex that
	## normalize_escapes produces (RFC 3986 §6.2.2.1) survives host
	## lowercasing (§6.2.2.3). A malformed escape is plain data and is
	## lowercased like any other byte.
	lower_host : Str -> Str
	lower_host = |s| {
		folded = List.fold(
			Str.to_utf8(s),
			{ out: [], pending: Step },
			|state, byte|
				match state.pending {
					Step => step_lower(state.out, byte)
					TakeFirst =>
						match Uri.hex_char_to_decimal(byte) {
							Ok(_) => { out: state.out, pending: TakeSecond(byte) }
							Err(_) => step_lower(List.append(state.out, '%'), byte)
						}

					TakeSecond(first) =>
						match Uri.hex_char_to_decimal(byte) {
							Ok(_) => { out: List.concat(state.out, ['%', upper_hex(first), upper_hex(byte)]), pending: Step }
							Err(_) => step_lower(List.concat(state.out, ['%', lower_byte(first)]), byte)
						}
					},
		)
		finished = 
			match folded.pending {
				Step => folded.out
				TakeFirst => List.append(folded.out, '%')
				TakeSecond(first) => List.concat(folded.out, ['%', lower_byte(first)])
			}
		Try.ok_or(Str.from_utf8(finished), s)
	}

	## Internal helper for lower_host. Consumes one byte outside an escape:
	## a "%" opens an escape, anything else is emitted lowercased.
	step_lower : List(U8), U8 -> { out : List(U8), pending : [Step, TakeFirst, TakeSecond(U8)] }
	step_lower = |out, byte|
		if byte == '%' {
			{ out: out, pending: TakeFirst }
		} else {
			{ out: List.append(out, lower_byte(byte)), pending: Step }
		}

	## Internal helper. Lowercases one ASCII letter.
	lower_byte : U8 -> U8
	lower_byte = |byte|
		if byte >= 'A' and byte <= 'Z' {
			byte + 32
		} else {
			byte
		}

	## Internal helper
	starts_with_digit : Str -> Bool
	starts_with_digit = |s|
		match List.first(Str.to_utf8(s)) {
			Ok(byte) => byte >= '0' and byte <= '9'
			Err(_) => False
		}

	## Internal helper for parse_leniently. Percent-encodes the bytes no
	## URI can contain, and nothing else: escapes, non-ASCII bytes, and
	## every reserved delimiter pass through, so the input's structure is
	## unchanged. A raw "%" also passes through, so already-encoded input
	## is not double-encoded.
	encode_forbidden : Str -> Str
	encode_forbidden = |s| {
		encoded = List.fold(
			Str.to_utf8(s),
			[],
			|out, byte|
				if is_forbidden_byte(byte) {
					List.concat(out, Uri.encoded_triplet(byte))
				} else {
					List.append(out, byte)
				},
		)
		Try.ok_or(Str.from_utf8(encoded), s)
	}

	## Internal helper. True for the ASCII bytes RFC 3986 excludes from
	## every production, so a URI can never contain them: controls, space,
	## DEL, and the unwise delimiters.
	is_forbidden_byte : U8 -> Bool
	is_forbidden_byte = |byte|
		byte <= ' '
			or byte == '"'
				or byte == '<'
					or byte == '>'
						or byte == 92 # backslash
							or byte == '^'
								or byte == 96 # backtick
									or byte == '{'
										or byte == '|'
											or byte == '}'
												or byte == 127 # DEL
}

# =============================================================================
# Test helpers
# =============================================================================

# Strict parse succeeds and serializes to `expected`
parses_to : Str, Str -> Bool
parses_to = |input, expected|
	match Url.parse(input) {
		Ok(u) => Url.to_str(u) == expected
		Err(_) => False
	}

# Lenient parse succeeds and serializes to `expected`
lenient_to : Str, Str -> Bool
lenient_to = |input, expected|
	match Url.parse_leniently(input) {
		Ok(u) => Url.to_str(u) == expected
		Err(_) => False
	}

# Base parses strictly, join succeeds, and the result serializes to `expected`
joins_to : Str, Str, Str -> Bool
joins_to = |base_input, reference, expected|
	match Url.parse(base_input) {
		Ok(base) =>
			match Url.join(base, reference) {
				Ok(u) => Url.to_str(u) == expected
				Err(_) => False
			}

		Err(_) => False
	}

# =============================================================================
# Tests: scheme-less input. parse is strict, parse_leniently
# guesses like a browser. These pin both.
# =============================================================================

expect Url.parse("example.com") == Err(NoScheme)
expect lenient_to("example.com", "https://example.com/")

# "localhost" is a scheme per RFC 3986, so parse rejects it and
# parse_leniently reads it as the host the writer meant.
expect Url.parse("localhost:3000/x") == Err(NotWebScheme("localhost"))
expect lenient_to("localhost:3000/x", "https://localhost:3000/x")

expect Url.parse("example.com:8080/path") == Err(NotWebScheme("example.com"))
expect lenient_to("example.com:8080/path", "https://example.com:8080/path")

expect Url.parse("192.168.1.1:8080/admin") == Err(NoScheme)
expect lenient_to("192.168.1.1:8080/admin", "https://192.168.1.1:8080/admin")

# A rooted path has no host to guess, so it is an error in both.
expect Url.parse("/docs?q=1") == Err(NoScheme)
expect Url.parse_leniently("/docs?q=1") == Err(NoScheme)

# Scheme-relative input gets its own precise error strictly, and https leniently
expect Url.parse("//cdn.example/x") == Err(SchemeRelative)
expect lenient_to("//cdn.example/x", "https://cdn.example/x")

# "http:foo" has a web scheme but no authority. The error is NoHost, never
# a misleading NotWebScheme("http").
expect Url.parse("http:foo") == Err(NoHost)
expect Url.parse_leniently("http:foo") == Err(NoHost)

expect Url.parse("") == Err(NoScheme)
expect Url.parse_leniently("") == Err(NoScheme)

expect Url.parse("#frag") == Err(NoScheme)
expect Url.parse_leniently("#frag") == Err(EmptyHost)

# =============================================================================
# Tests: normalization
# =============================================================================

# Scheme and host lowercase, default port dropped, dot segments resolved:
# equality is normalized equality
expect Url.parse("HTTP://Example.COM:80/a/../b") == Url.parse("http://example.com/b")

expect parses_to("HTTP://EXAMPLE.com/A/b", "http://example.com/A/b")

# Empty path becomes "/"
expect parses_to("https://example.com", "https://example.com/")
expect {
	match Url.parse("https://example.com") {
		Ok(u) => Url.path(u) == "/"
		Err(_) => False
	}
}

# Default port is elided but still answered by port()
expect {
	match Url.parse("https://example.com:443/") {
		Ok(u) => Url.explicit_port(u) == NoPort and Url.port(u) == 443 and Url.to_str(u) == "https://example.com/"
		Err(_) => False
	}
}

expect {
	match Url.parse("http://example.com") {
		Ok(u) => Url.port(u) == 80
		Err(_) => False
	}
}

# A non-default port is kept and reported by both accessors
expect {
	match Url.parse("https://example.com:8443/") {
		Ok(u) => Url.explicit_port(u) == Port(8443) and Url.port(u) == 8443
		Err(_) => False
	}
}

# Dot segments are resolved at construction
expect {
	match Url.parse("http://e.com/a/../b") {
		Ok(u) => Url.path(u) == "/b"
		Err(_) => False
	}
}

# Path case is data and stays untouched
expect {
	match Url.parse("https://example.com/A/b") {
		Ok(u) => Url.path(u) == "/A/b"
		Err(_) => False
	}
}

# =============================================================================
# Tests: userinfo. Kept, serialized, never in origin.
# =============================================================================

expect parses_to("https://user:pass@e.com/", "https://user:pass@e.com/")
expect {
	match Url.parse("https://user:pass@e.com/") {
		Ok(u) => Url.origin(u) == "https://e.com"
		Err(_) => False
	}
}

# Userinfo ends at the LAST "@" (same reading as Uri and WHATWG)
expect {
	match Url.parse("http://a@b@c.com/") {
		Ok(u) => Url.host(u) == "c.com" and Url.origin(u) == "http://c.com"
		Err(_) => False
	}
}

# =============================================================================
# Tests: empty host
# =============================================================================

expect Url.parse("https://") == Err(EmptyHost)
expect Url.parse("https://:3000/") == Err(EmptyHost)

# =============================================================================
# Tests: ports
# =============================================================================

expect Url.parse("https://e.com:abc/") == Err(PortInvalid("abc"))
expect Url.parse("https://e.com:99999/") == Err(PortInvalid("99999"))

# Leading zeros parse to the same number, so the default is still elided
expect Url.parse("http://e.com:0080/") == Url.parse("http://e.com/")

# A trailing ":" is an absent port (RFC-valid), and port() answers the default
expect {
	match Url.parse("https://e.com:") {
		Ok(u) => Url.explicit_port(u) == NoPort and Url.port(u) == 443 and Url.to_str(u) == "https://e.com/"
		Err(_) => False
	}
}

# =============================================================================
# Tests: non-web schemes. A Url can never hold a mailto:,
# file:, data:, or javascript: value, so code that takes a Url needs no
# blacklist.
# =============================================================================

expect Url.parse("mailto:a@b.com") == Err(NotWebScheme("mailto"))
expect Url.parse("ftp://files.example/x") == Err(NotWebScheme("ftp"))
expect Url.parse("file:///etc/hosts") == Err(NotWebScheme("file"))
expect Url.parse("javascript:alert(1)") == Err(NotWebScheme("javascript"))
expect Url.parse("data:text/plain,x") == Err(NotWebScheme("data"))

# parse_leniently rejects them too: never a garbled host out of a scheme
expect Url.parse_leniently("mailto:a@b.com") == Err(NotWebScheme("mailto"))
expect Url.parse_leniently("javascript:alert(1)") == Err(NotWebScheme("javascript"))
expect Url.parse_leniently("ftp://files.example/x") == Err(NotWebScheme("ftp"))
expect Url.parse_leniently("file:///etc/hosts") == Err(NotWebScheme("file"))

# The raw scheme keeps its case in the error
expect Url.parse("MAILTO:x") == Err(NotWebScheme("MAILTO"))

# =============================================================================
# Tests: bytes are policy. No IDNA and no re-encoding of raw bytes. Valid
# escapes are the one exception: RFC 3986 §6.2.2 normalization runs at
# construction (uppercase hex, unreserved characters decoded).
# =============================================================================

expect {
	match Url.parse("http://bücher.de/") {
		Ok(u) => Url.host(u) == "bücher.de"
		Err(_) => False
	}
}

expect parses_to("http://e.com/a%2Fb", "http://e.com/a%2Fb")

# A malformed escape passes through the path verbatim
expect parses_to("http://e.com/%zz", "http://e.com/%zz")

# A truncated escape passes through verbatim too
expect parses_to("http://e.com/abc%2", "http://e.com/abc%2")

# A raw space passes through verbatim, on purpose. WHATWG would encode it.
expect parses_to("http://e.com/a b", "http://e.com/a b")

# Escape hex is uppercased (RFC 3986 §6.2.2.1), so %2f and %2F are one Url
expect Url.parse("http://e.com/%2f") == Url.parse("http://e.com/%2F")
expect parses_to("http://e.com/a%2fb", "http://e.com/a%2Fb")

# An escape of an unreserved character is decoded (RFC 3986 §6.2.2.2)
expect Url.parse("http://e.com/%61") == Url.parse("http://e.com/a")
expect parses_to("http://e.com/%41b", "http://e.com/Ab")
expect parses_to("http://e.com/%7Euser", "http://e.com/~user")

# A reserved character stays escaped, so encoding cannot change structure:
# %2F does not become a path separator (pinned above), %26 stays "&" data
expect parses_to("https://x.example/?a=b%26c=d", "https://x.example/?a=b%26c=d")

# A decoded %2E takes part in dot-segment resolution
expect parses_to("http://e.com/a/%2E%2E/b", "http://e.com/b")

# Escape normalization runs in every component: userinfo, host, path,
# query, and fragment. A decoded host byte is then lowercased.
expect parses_to("https://%75ser@e%2Dcom.example/%7E?q=%2f#%2f", "https://user@e-com.example/~?q=%2F#%2F")
expect parses_to("http://%41.com/", "http://a.com/")

# Host lowercasing skips escape hex, so the uppercase hex of §6.2.2.1
# survives §6.2.2.3: letters go down, escapes stay canonical, and the two
# spellings are one Url
expect parses_to("http://B%c3%bcCHER.de/", "http://b%C3%BCcher.de/")
expect Url.parse("http://e%C3.com/") == Url.parse("http://E%c3.COM/")

# Backslashes are not slashes, so "http:\\e.com\x" has no authority. WHATWG
# would read host e.com, we follow the RFC.
expect Url.parse("http:\\\\e.com\\x") == Err(NoHost)

# =============================================================================
# Tests: join. Composition of Uri.resolve plus revalidation.
# =============================================================================

expect joins_to("http://a/b/c/d;p?q", "g", "http://a/b/c/g")
expect joins_to("http://a/b/c/d;p?q", "./g", "http://a/b/c/g")
expect joins_to("http://a/b/c/d;p?q", "/g", "http://a/g")
expect joins_to("http://a/b/c/d;p?q", "../g", "http://a/b/g")
expect joins_to("http://a/b/c/d;p?q", "../../g", "http://a/g")

# Abnormal vectors: ".." past the root is dropped
expect joins_to("http://a/b/c/d;p?q", "../../../g", "http://a/g")
expect joins_to("http://a/b/c/d;p?q", "../../../../g", "http://a/g")

# An empty reference keeps the query and drops nothing else
expect joins_to("http://a/b/c/d;p?q", "", "http://a/b/c/d;p?q")

# Fragment-only and query-only references replace just that component
expect joins_to("http://a/b/c/d;p?q", "#s", "http://a/b/c/d;p?q#s")
expect joins_to("http://a/b/c/d;p?q", "?y", "http://a/b/c/d;p?y")

# A scheme-relative reference takes a new authority, same scheme
expect joins_to("https://example.com/a", "//other.example/p", "https://other.example/p")

# Following a link stays on the web: a reference that resolves to mailto:
# fails instead of producing a non-web value.
expect {
	match Url.parse("https://example.com/a") {
		Ok(base) => Url.join(base, "mailto:x") == Err(NotWebScheme("mailto"))
		Err(_) => False
	}
}

expect {
	match Url.parse("https://example.com/a") {
		Ok(base) => Url.join(base, "//") == Err(EmptyHost)
		Err(_) => False
	}
}

# The joined result is normalized like any other construction
expect joins_to("https://example.com/a/", "b/../c", "https://example.com/a/c")

# =============================================================================
# Tests: delimiter precedence
# =============================================================================

expect {
	match Url.parse("http://e.com/p#f?x") {
		Ok(u) => Url.query(u) == NoQuery and Url.fragment(u) == Fragment("f?x")
		Err(_) => False
	}
}

expect {
	match Url.parse("http://e.com/?a=1?b=2") {
		Ok(u) => Url.query(u) == Query("a=1?b=2")
		Err(_) => False
	}
}

expect {
	match Url.parse("http://e.com/#a#b") {
		Ok(u) => Url.fragment(u) == Fragment("a#b")
		Err(_) => False
	}
}

# Empty-but-present query and fragment survive normalization
expect parses_to("https://x.com/?", "https://x.com/?")
expect parses_to("https://x.com/#", "https://x.com/#")

# =============================================================================
# Tests: query_params. Same decoding rules as Uri.query_params.
# =============================================================================

expect {
	match Url.parse("https://x.example/?a=1&b=2") {
		Ok(u) => Url.query_params(u) == [("a", "1"), ("b", "2")]
		Err(_) => False
	}
}

# Repeated keys and order are preserved
expect {
	match Url.parse("https://shop.example/cart?item=sword&item=shield") {
		Ok(u) => Url.query_params(u) == [("item", "sword"), ("item", "shield")]
		Err(_) => False
	}
}

# Keys and values are percent-decoded
expect {
	match Url.parse("https://x.example/?caf%C3%A9=du%20Monde") {
		Ok(u) => Url.query_params(u) == [("café", "du Monde")]
		Err(_) => False
	}
}

# An empty or absent query gives an empty list
expect {
	match Url.parse("https://x.example/?") {
		Ok(u) => Url.query_params(u) == []
		Err(_) => False
	}
}
expect {
	match Url.parse("https://x.example/") {
		Ok(u) => Url.query_params(u) == []
		Err(_) => False
	}
}

# =============================================================================
# Tests: with_param / append_param. Query writers cannot touch the scheme,
# host, or port, so the Url guarantees survive any argument.
# =============================================================================

# with_param replaces in place, keeping the pair's position
expect {
	match Url.parse("https://x.example/?a=1&b=2") {
		Ok(u) => Url.to_str(Url.with_param(u, "a", "9")) == "https://x.example/?a=9&b=2"
		Err(_) => False
	}
}

# ...appends when the key is absent, and when there is no query at all
expect {
	match Url.parse("https://x.example/?a=1") {
		Ok(u) => Url.to_str(Url.with_param(u, "b", "2")) == "https://x.example/?a=1&b=2"
		Err(_) => False
	}
}
expect {
	match Url.parse("https://x.example/") {
		Ok(u) => Url.to_str(Url.with_param(u, "a", "1")) == "https://x.example/?a=1"
		Err(_) => False
	}
}

# append_param always adds another pair, with_param replaces it
expect {
	match Url.parse("https://x.example/?a=1") {
		Ok(u) =>
			Url.query_params(Url.append_param(u, "a", "2")) == [("a", "1"), ("a", "2")]
				and Url.query_params(Url.with_param(u, "a", "2")) == [("a", "2")]

		Err(_) => False
	}
}

# Key and value are encoded, and query_params reads them back decoded
expect {
	match Url.parse("https://x.example/") {
		Ok(u) => {
			written = Url.append_param(u, "email", "hi@example.com")
			Url.to_str(written) == "https://x.example/?email=hi%40example.com"
				and Url.query_params(written) == [("email", "hi@example.com")]
		}

		Err(_) => False
	}
}

# A written Url still parses back to itself, and the rest is untouched
expect {
	match Url.parse("https://user@x.example:8443/p#f") {
		Ok(u) => {
			written = Url.with_param(u, "k&k", "v=v?")
			Url.parse(Url.to_str(written)) == Ok(written)
				and Url.origin(written) == Url.origin(u)
					and Url.path(written) == Url.path(u)
						and Url.fragment(written) == Url.fragment(u)
		}

		Err(_) => False
	}
}

# =============================================================================
# Tests: the component writers. Each one renormalizes what it touches, so
# a written Url is what parsing its to_str would give. with_host and
# append_path return a Try. Everything else is total.
# =============================================================================

# with_scheme renormalizes the port against the new scheme: 443 is default
# under https, so it is dropped, and the result equals the parsed spelling
expect {
	match Url.parse("http://e.com:443/x") {
		Ok(u) => Ok(Url.with_scheme(u, Https)) == Url.parse("https://e.com/x")
		Err(_) => False
	}
}

# ...while a non-default port survives the switch
expect {
	match Url.parse("https://e.com:8443/x") {
		Ok(u) => Ok(Url.with_scheme(u, Http)) == Url.parse("http://e.com:8443/x")
		Err(_) => False
	}
}

# An absent port stays absent, so the new scheme's default applies
expect {
	match Url.parse("http://e.com/") {
		Ok(u) => Url.port(Url.with_scheme(u, Https)) == 443
		Err(_) => False
	}
}

# with_port sets an explicit port, and the scheme's default normalizes away
expect {
	match Url.parse("https://e.com/") {
		Ok(u) =>
			Url.to_str(Url.with_port(u, 8443)) == "https://e.com:8443/"
				and Url.explicit_port(Url.with_port(u, 443)) == NoPort
					and Url.port(Url.with_port(u, 443)) == 443

		Err(_) => False
	}
}

# without_port drops the explicit port, and port() answers the default again
expect {
	match Url.parse("https://e.com:8443/") {
		Ok(u) => Url.to_str(Url.without_port(u)) == "https://e.com/" and Url.port(Url.without_port(u)) == 443
		Err(_) => False
	}
}

# with_host refuses the one value that cannot be a web host
expect {
	match Url.parse("https://e.com/") {
		Ok(u) => Url.with_host(u, "") == Err(EmptyHost)
		Err(_) => False
	}
}

# ...and normalizes everything else like a parse: lowercased, and an "@"
# is encoded so it cannot smuggle a userinfo boundary in
expect {
	match Url.parse("https://e.com/x") {
		Ok(u) =>
			Try.map_ok(Url.with_host(u, "API.Example.COM"), Url.host) == Ok("api.example.com")
				and Try.map_ok(Url.with_host(u, "a@b"), Url.to_str) == Ok("https://a%40b/x")

		Err(_) => False
	}
}

# A bracket-shaped argument passes through whole as an IP-literal
expect {
	match Url.parse("https://e.com/") {
		Ok(u) => Try.map_ok(Url.with_host(u, "[::1]"), Url.host) == Ok("[::1]")
		Err(_) => False
	}
}

# with_path encodes each segment (a "?" cannot shift the query boundary),
# roots a rootless path, and turns "" into "/"
expect {
	match Url.parse("https://e.com/old?q=1") {
		Ok(u) =>
			Url.to_str(Url.with_path(u, "docs/getting started")) == "https://e.com/docs/getting%20started?q=1"
				and Url.path(Url.with_path(u, "/a?b")) == "/a%3Fb"
					and Url.path(Url.with_path(u, "")) == "/"

		Err(_) => False
	}
}

# In with_path, ".." is path syntax and resolves, exactly as in a parse
expect {
	match Url.parse("https://e.com/") {
		Ok(u) => Url.path(Url.with_path(u, "/a/../b")) == "/b"
		Err(_) => False
	}
}

# append_path treats each element as one opaque segment: the "@" is
# encoded, and a trailing "/" does not double
expect {
	match Url.parse("https://api.example.com/v1") {
		Ok(u) =>
			Try.map_ok(Url.append_path(u, ["users", "a@b.se", "posts"]), Url.to_str)
				== Ok("https://api.example.com/v1/users/a%40b.se/posts")

		Err(_) => False
	}
}

expect {
	match Url.parse("https://e.com/a/") {
		Ok(u) => Try.map_ok(Url.append_path(u, ["b"]), Url.path) == Ok("/a/b")
		Err(_) => False
	}
}

# A literal dot segment cannot exist in a normalized path, and resolving it
# would make an "append" delete data, so it is refused with the segment named
expect {
	match Url.parse("https://e.com/a/b") {
		Ok(u) =>
			Url.append_path(u, ["..", "x"]) == Err(DotSegment(".."))
				and Url.append_path(u, ["."]) == Err(DotSegment("."))

		Err(_) => False
	}
}

# An escaped dot-dot is data, not structure: the "%" is encoded, so nothing
# resolves and the result round-trips
expect {
	match Url.parse("https://e.com/a") {
		Ok(u) =>
			match Url.append_path(u, ["%2e%2e"]) {
				Ok(w) => Url.path(w) == "/a/%252e%252e" and Url.parse(Url.to_str(w)) == Ok(w)
				Err(_) => False
			}

		Err(_) => False
	}
}

# with_query sets the raw query ("?" and "=" pass through, "#" is encoded),
# and "" removes it
expect {
	match Url.parse("https://e.com/p#f") {
		Ok(u) =>
			Url.query(Url.with_query(u, "a=1&b=2")) == Query("a=1&b=2")
				and Url.query_params(Url.with_query(u, "a=1&b=2")) == [("a", "1"), ("b", "2")]
					and Url.query(Url.with_query(u, "a#b")) == Query("a%23b")
						and Url.fragment(Url.with_query(u, "a#b")) == Fragment("f")
							and Url.query(Url.with_query(u, "")) == NoQuery

		Err(_) => False
	}
}

# with_fragment sets, normalizes escapes (equality keeps working), and ""
# removes
expect {
	match Url.parse("https://e.com/") {
		Ok(u) =>
			Url.fragment(Url.with_fragment(u, "%7esec")) == Fragment("~sec")
				and Url.with_fragment(u, "%2f") == Url.with_fragment(u, "%2F")
					and Url.fragment(Url.with_fragment(u, "")) == NoFragment

		Err(_) => False
	}
}

# with_userinfo encodes (the "@" cannot shift the host), serializes, stays
# out of origin, and "" removes it
expect {
	match Url.parse("https://e.com/") {
		Ok(u) => {
			written = Url.with_userinfo(u, "a@b:pa ss")
			Url.to_str(written) == "https://a%40b:pa%20ss@e.com/"
				and Url.origin(written) == "https://e.com"
					and Url.to_str(Url.with_userinfo(written, "")) == "https://e.com/"
		}

		Err(_) => False
	}
}

# Every total writer's output reparses to itself: writing and parsing
# agree on normalization
expect {
	match Url.parse("https://user@x.example:8443/a/b?q=1#f") {
		Ok(u) => {
			written = [
				Url.with_scheme(u, Http),
				Url.with_port(u, 9000),
				Url.without_port(u),
				Url.with_path(u, "a b/../ü"),
				Url.with_query(u, "raw?=&#x"),
				Url.with_fragment(u, "%2fsec tion"),
				Url.with_userinfo(u, "a@b:pw"),
			]
			List.all(written, |w| Url.parse(Url.to_str(w)) == Ok(w))
		}

		Err(_) => False
	}
}

# ...and so do the Try writers' outputs
expect {
	match Url.parse("https://x.example/a") {
		Ok(u) =>
			match Url.with_host(u, "bücher.DE") {
				Ok(w) => Url.host(w) == "b%C3%BCcher.de" and Url.parse(Url.to_str(w)) == Ok(w)
				Err(_) => False
			}

		Err(_) => False
	}
}

expect {
	match Url.parse("https://x.example/a") {
		Ok(u) =>
			match Url.append_path(u, ["b c", "ü"]) {
				Ok(w) => Url.path(w) == "/a/b%20c/%C3%BC" and Url.parse(Url.to_str(w)) == Ok(w)
				Err(_) => False
			}

		Err(_) => False
	}
}

# =============================================================================
# Tests: IP literals and brackets
# =============================================================================

expect {
	match Url.parse("http://[::1]:8080/") {
		Ok(u) => Url.host(u) == "[::1]" and Url.explicit_port(u) == Port(8080)
		Err(_) => False
	}
}

# No IPv6 canonicalization: the uncompressed form is a different Url
expect Url.parse("http://[0:0:0:0:0:0:0:1]/") != Url.parse("http://[::1]/")

# Bracket contents are bytes, their validity is policy: an unmatched "[" or
# junk after "]" still construct
expect {
	match Url.parse("http://[::1/path") {
		Ok(u) => Url.host(u) == "[::1" and Url.path(u) == "/path"
		Err(_) => False
	}
}

expect {
	match Url.parse("http://[::1]junk/") {
		Ok(u) => Url.host(u) == "[::1]junk"
		Err(_) => False
	}
}

expect Url.parse("[::1]:8080") == Err(NoScheme)

# =============================================================================
# Tests: parse_leniently corners, each pinned on purpose. They follow the
# repair rule in the docstring.
# =============================================================================

expect lenient_to("  example.com  ", "https://example.com/")
expect lenient_to("user@host", "https://user@host/")
expect {
	match Url.parse_leniently("user@host") {
		Ok(u) => Url.origin(u) == "https://host"
		Err(_) => False
	}
}
expect lenient_to("[::1]:8080", "https://[::1]:8080/")

# "tel:80" has digits after the colon, so the repair rule reads it as host
# "tel" port 80. Nobody types it, and special-casing scheme names would be a
# blacklist in disguise, so the oddity is pinned here as a known tradeoff.
expect lenient_to("tel:80", "https://tel:80/")

# The same digit rule, but the port overflows U16
expect Url.parse_leniently("tel:5551234") == Err(PortInvalid("5551234"))

expect Url.parse_leniently("http:") == Err(NoHost)
expect Url.parse_leniently("http://") == Err(EmptyHost)
expect Url.parse_leniently(":3000/x") == Err(EmptyHost)
expect Url.parse_leniently("?q=1") == Err(EmptyHost)
expect Url.parse_leniently("#frag") == Err(EmptyHost)

# No IDNA in the lenient path either: the bytes pass through
expect lenient_to("bücher.de", "https://bücher.de/")

# Forbidden bytes are encoded the way an address bar would: a typed space
# becomes %20, a delimiter no URI can contain becomes its escape
expect lenient_to("example.com/a b", "https://example.com/a%20b")
expect lenient_to("example.com/{x}", "https://example.com/%7Bx%7D")

# A raw "%" passes through, so already-encoded input is not double-encoded
expect lenient_to("example.com/a%20b", "https://example.com/a%20b")

# Non-ASCII bytes are not forbidden here, they stay raw (no IDNA, and a
# browser would encode the path bytes)
expect lenient_to("bücher.de/ü", "https://bücher.de/ü")

# The one disagreement with strict parse, pinned: a raw space in otherwise
# valid input is data to parse (bytes are policy) and a typo to repair here
expect parses_to("http://e.com/a b", "http://e.com/a b")
expect lenient_to("http://e.com/a b", "http://e.com/a%20b")

# Uppercase web schemes go through strict validation
expect lenient_to("HTTP://EXAMPLE.com", "http://example.com/")

# =============================================================================
# Tests: properties
# =============================================================================

# Every valid URI parse accepts, parse_leniently accepts with the
# identical result. Only non-URI input (surrounding whitespace, forbidden
# raw bytes like a space) can make them disagree.
expect {
	strict_corpus = [
		"https://example.com",
		"HTTP://EXAMPLE.com:80/A/../b",
		"https://user:pass@e.com:8443/p?q=1#f",
		"http://[::1]:8080/",
		"http://bücher.de/",
		"http://e.com/%zz",
		"https://x.com/?",
		"http://e.com/p#f?x",
	]
	List.all(strict_corpus, |input| Url.parse(input) == Url.parse_leniently(input))
}

# Parsing to_str's output gives the same Url back, for every constructed
# Url. Normalization only happens once.
expect {
	corpus = [
		"https://example.com",
		"HTTP://Example.COM:80/a/../b",
		"https://user:pass@e.com:8443/p?q=1#f",
		"http://a@b@c.com/",
		"http://[::1]:8080/",
		"http://[::1/path",
		"http://bücher.de/",
		"http://e.com/%zz",
		"http://e.com/abc%2",
		"http://e.com/a%2fb",
		"http://e.com/%61%2E%C3%A9",
		"http://e.com/a b",
		"https://x.com/?",
		"https://x.com/#",
		"http://e.com/?a=1?b=2",
		"http://e.com/p#f?x",
		"https://e.com:",
	]
	List.all(
		corpus,
		|input|
			match Url.parse(input) {
				Ok(u) => Url.parse(Url.to_str(u)) == Ok(u)
				Err(_) => False
			},
	)
}

# to_uri round-trips: from_uri(to_uri(url)) == Ok(url)
expect {
	corpus = [
		"https://example.com",
		"https://user:pass@e.com:8443/p?q=1#f",
		"http://[::1]:8080/",
		"http://e.com/a b",
	]
	List.all(
		corpus,
		|input|
			match Url.parse(input) {
				Ok(u) => Url.from_uri(Url.to_uri(u)) == Ok(u)
				Err(_) => False
			},
	)
}

# Origins never contain userinfo
expect {
	corpus = ["https://user:pass@e.com/", "http://a@b@c.com/", "https://u@v@w@x.com:99/"]
	List.all(
		corpus,
		|input|
			match Url.parse(input) {
				Ok(u) => Str.split_on(Url.origin(u), "@") == [Url.origin(u)]
				Err(_) => False
			},
	)
}
