app [main!] {
	pf: platform "https://github.com/niclas-ahden/basic-cli/releases/download/0.22.0/7FuVkuWGpyRSLu5w8vfBx82bhqpiVRv1gqQZcrA2H9m9.tar.zst",
	url: "../package/main.roc",
}

# Uri never fails to parse, and each component answers for itself when you
# read it. When you want all-or-nothing instead, to_try hoists the
# per-component outcomes into one Try at the top, and require_host /
# require_scheme demand a component be present.

import pf.Stdout
import url.Uri

# to_try()? short-circuits the moment a component didn't parse. A malformed
# port is the only thing that can fail today, so a clean URL sails through and
# a "banana" port stops it here. Absence is not an error, so a host-less
# relative reference like "/health" passes too.
first_clean : Str -> Try(Str, [PortParseErr(Str)])
first_clean = |input| {
	uri = Uri.parse(input).to_try()?
	Ok(Uri.to_str(uri))
}

main! = |_args| {
	for input in [
		"https://api.example.com:8443/v1/users",
		"https://api.example.com:banana/v1/users",
		"/health",
	] {
		Stdout.line!(input)?

		# to_try alone: nonsense fails as one Err, and absence is fine.
		total_line = 
			match first_clean(input) {
				Ok(clean) => "Parses cleanly: ${clean}"
				Err(PortParseErr(raw)) => "Bad port \"${raw}\""
			}
		Stdout.line!(total_line)?

		# to_try says "/health" is fine. If you also need a host, that is
		# require_host, and it is the one that rejects "/health".
		host_line = 
			match Uri.require_host(Uri.parse(input)) {
				Ok(host) => "Host present: ${host}"
				Err(NoHost) => "No host (fine for to_try, not for an endpoint)"
				Err(EmptyHost) => "Empty host"
			}
		Stdout.line!(host_line)?
	}

	Ok({})
}
