app [main!] {
	pf: platform "https://github.com/niclas-ahden/basic-cli/releases/download/0.22.1/DobkAk7zNyqAgqh2Riaj5c5DtWtKhd5iVYE5RFa6izcd.tar.zst",
	url: "../package/main.roc",
}

# The Uri module carries the full RFC 3986 machinery. resolve turns a
# reference (an href, a redirect Location) into an absolute URI against a base,
# query_params decodes a query string into pairs, and percent_encode /
# percent_decode are exported for standalone use.
#
# Run it with: roc examples/resolve.roc

import pf.Stdout
import url.Uri

main! = |_args| {
	# resolve follows a reference the way a browser resolves an href on a
	# page. The base is the document you are on, and the reference is what you
	# clicked.
	Stdout.line!("# Uri.resolve, base https://example.com/a/b/page.html")?
	base = Uri.parse("https://example.com/a/b/page.html")
	for reference in ["../g", "//cdn.example/lib.js", "/root", "?q=2", "#section", "https://other.example/x"] {
		resolved = Uri.resolve(base, Uri.parse(reference))
		Stdout.line!("  ${reference} -> ${Uri.to_str(resolved)}")?
	}

	# query_params percent-decodes keys and values and skips empty pairs, so
	# you read back exactly what was written, not the encoded bytes.
	Stdout.line!("")?
	Stdout.line!("# Uri.query_params")?
	search = Uri.parse("https://example.com/s?q=roc%20lang&tag=web%2Bdev&page=2")
	for pair in Uri.query_params(search) {
		(key, value) = pair
		Stdout.line!("  ${key} = ${value}")?
	}

	# percent_encode and percent_decode stand on their own. Decoding is strict:
	# a truncated escape is an error, not a silently dropped byte.
	Stdout.line!("")?
	Stdout.line!("# percent_encode / percent_decode")?
	raw = "café & bar/#1"
	encoded = Uri.percent_encode(raw)
	Stdout.line!("  encode ${raw} -> ${encoded}")?
	for candidate in [encoded, "abc%2"] {
		line = 
			match Uri.percent_decode(candidate) {
				Ok(back) => "  decode ${candidate} -> ${back}"
				Err(InvalidEncoding) => "  decode ${candidate} -> Err(InvalidEncoding)"
			}
		Stdout.line!(line)?
	}

	Ok({})
}
