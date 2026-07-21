app [main!] {
	pf: platform "https://github.com/niclas-ahden/basic-cli/releases/download/0.22.0/7FuVkuWGpyRSLu5w8vfBx82bhqpiVRv1gqQZcrA2H9m9.tar.zst",
	url: "../package/main.roc",
}

# An overview of roc-url. Run it with: roc examples/main.roc

import pf.Stdout
import url.Url
import url.Uri

main! = |_| {
	# Let's parse some URLs!
	#
	# Url.parse is strict, and Url.parse_leniently parses more like a browser would.
	# Neither of them accepts anything other than HTTP/HTTPS, though. Url.parse_lentiently
	# assumes HTTPS when there's no scheme.

	# Rejected by Url.parse, accepted by Url.parse_leniently:
	Stdout.line!("# Url.parse_leniently")?
	for url in ["example.com", "  example.com/spaces-trimmed  ", "localhost:3000/spaces encoded/"] {
		line = 
			match Url.parse_leniently(url) {
				Ok(u) => "${url} -> ${Url.to_str(u)}"
				Err(e) => "${url} -> ${Str.inspect(e)}"
			}

		Stdout.line!(line)

		# example.com -> https://example.com/
		# example.com/spaces-trimmed   -> https://example.com/spaces-trimmed
		# localhost:3000/spaces encoded/ -> https://localhost:3000/spaces%20encoded/
	}

	Stdout.line!("")?

	# The same inputs through the strict Url.parse give what the RFC says, not
	# what the typist meant. Use Url.parse for config values and API responses,
	# where a mistake should be an error rather than something to repair.
	Stdout.line!("# Url.parse is strict")?
	for url in ["example.com", "  example.com/spaces-not-trimmed  ", "localhost:3000/spaces wont be encoded/", "https://example.com"] {
		line = 
			match Url.parse(url) {
				Ok(u) => "${url} -> ${Url.to_str(u)}"
				Err(e) => "${url} -> ${Str.inspect(e)}"
			}

		Stdout.line!(line)?

		# example.com -> NoScheme
		#   example.com/spaces-not-trimmed   -> NoScheme
		# localhost:3000/spaces wont be encoded/ -> NotWebScheme("localhost")
		# https://example.com -> https://example.com/
	}

	Stdout.line!("")?

	# A Url is normalized at construction (scheme and host lowercased, the
	# default port dropped, dot segments resolved), so two spellings of the
	# same URL are equal.
	Stdout.line!("# Url normalization")?
	normalized = Url.parse("HTTP://Example.COM:80/a/../b") == Url.parse("http://example.com/b")
	Stdout.line!("HTTP://Example.COM:80/a/../b == http://example.com/b -> ${bool_str(normalized)}")?

	Stdout.line!("")?

	# Every Url passed its checks, so the accessors are plain: host is a Str,
	# port is a U16 (the scheme's default when none was written), and origin
	# never includes userinfo, so it is safe to log.
	Stdout.line!("# Accessors and join")?
	for shop_input in ["HTTPS://Sword-Shop.example/Cart?item=sword&item=shield"] {
		match Url.parse(shop_input) {
			Ok(shop) => {
				Stdout.line!("host:   ${Url.host(shop)}")?
				Stdout.line!("port:   ${Url.port(shop).to_str()}")?
				Stdout.line!("origin: ${Url.origin(shop)}")?

				# join follows a link like a browser, and it stays on the web:
				# a reference that resolves to mailto: fails instead.
				checkout = 
					match Url.join(shop, "../checkout?step=1") {
						Ok(next) => Url.to_str(next)
						Err(e) => Str.inspect(e)
					}
				Stdout.line!("join:   ${checkout}")?

				# query_params decodes the query into pairs, keeping repeated
				# keys and their order.
				for pair in Url.query_params(shop) {
					(key, value) = pair
					Stdout.line!("query:  ${key} = ${value}")?
				}

				# with_param is add-or-replace: both item pairs collapse into
				# one axe. append_param always adds another pair. Keys and
				# values are percent-encoded for you.
				updated = shop.with_param("item", "axe").append_param("coupon", "SAVE 10%")
				Stdout.line!("params: ${Url.to_str(updated)}")?

				# append_path encodes every segment, and refuses
				# a literal ".." (use Url.join to traverse up).
				gift = 
					match Url.append_path(shop, ["gift wrap", ".."]) {
						Ok(g) => Url.to_str(g)
						Err(e) => Str.inspect(e)
					}
				Stdout.line!("append: ${gift}")?

				# See the docs for more builders, such as Url.{join, with_path}.
			}

			Err(_) => Stdout.line!("not a web URL")?
		}
	}

	# Unlike Url, which normalizes input, Uri does not and is lossless.
	# Uri.parse never fails, doesn't encode or modify the input, so
	# you can round-trip it exactly. Builders like Uri.append_path do
	# percent-encode for you.
	Stdout.line!("")?
	Stdout.line!("# Uri builders")?
	endpoint = 
		Uri.parse("https://api.example.com")
			.append_path(["v1", "users", "örjan.lax@example.com", "posts"])
			.append_param("sort", "newest first")
	Stdout.line!("${Uri.to_str(endpoint)}")?
	# https://api.example.com/v1/users/%C3%B6rjan.lax%40example.com/posts?sort=newest%20first

	# Nonsense is kept, labelled, and it still prints back exactly as written.
	Stdout.line!("")?
	Stdout.line!("# Uri keeps nonsense, labelled")?
	janky = Uri.parse("https://x.example:banana/path")
	port_line = 
		match Uri.port(janky) {
			Ok(_) => "a parseable port"
			Err(PortParseErr(raw)) => "\"${raw}\" is not a port, but ${Uri.to_str(janky)} survives"
		}
	Stdout.line!(port_line)?
	# "banana" is not a port, but https://x.example:banana/path survives

	Ok({})
}

bool_str : Bool -> Str
bool_str = |b| if b "True" else "False"
