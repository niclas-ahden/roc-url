module [
    Uri,
    parse_uri,
]

Maybe a : [
    Just a,
    Nothing,
]

Uri : [
    Absolute {
            protocol : Maybe Str,
            userinfo : Maybe Str,
            host : Str,
            port : Maybe U16,
            path : Str,
            query : Maybe Str,
            fragment : Maybe Str,
        },
    Relative {
            path : Str,
            query : Maybe Str,
            fragment : Maybe Str,
        },
]

ParseErr : [
    InvalidUri,
    InvalidPort Str,
    InvalidHost Str,
]

parse_uri : Str -> Result Uri ParseErr
parse_uri = |uri|
    (fragment, sans_fragment) = extract_fragment(uri)
    (query, sans_query) = extract_query(sans_fragment)

    # Query parameters can contain "://" but now that the parameters
    # are already parsed, we can safely extract the protocol.
    (protocol, sans_protocol) = extract_protocol(sans_query)

    # We only deal with URI which have a host or a path
    if sans_protocol == "" then
        Err(InvalidUri)
    else
        (path, sans_path) = extract_path(sans_protocol)
        (userinfo, sans_userinfo) = extract_userinfo(sans_path)
        (m_host, port) = extract_host_and_port(sans_userinfo)?

        when m_host is
            Nothing -> Ok(Relative { path, query, fragment })
            Just(host) ->
                if is_valid_host(host) then
                    Ok(
                        Absolute {
                            protocol,
                            userinfo,
                            host,
                            port,
                            path,
                            query,
                            fragment,
                        },
                    )
                else
                    Err(InvalidHost(host))

is_valid_host = |host|
    bytes = Str.to_utf8(host)
    List.len(bytes) <= 255 and List.all(bytes, is_valid_label_char)

is_valid_label_char : U8 -> Bool
is_valid_label_char = |char|
    (char >= 'a' and char <= 'z')
    or
    (char >= 'A' and char <= 'Z')
    or
    (char >= '0' and char <= '9')
    or
    char
    == '.'
    or
    char
    == '-'

extract_fragment : Str -> (Maybe Str, Str)
extract_fragment = |uri|
    when Str.split_last(uri, "#") is
        Ok({ before: rest, after: fragment }) if fragment != "" -> (Just(fragment), rest)
        _ -> (Nothing, uri)

extract_query : Str -> (Maybe Str, Str)
extract_query = |uri|
    when Str.split_last(uri, "?") is
        Ok({ before: rest, after: query }) if query != "" -> (Just(query), rest)
        _ -> (Nothing, uri)

extract_protocol : Str -> (Maybe Str, Str)
extract_protocol = |uri|
    when Str.split_first(uri, "://") is
        Ok({ before: protocol, after: rest }) if protocol != "" -> (Just(protocol), rest)
        _ -> (Nothing, uri)

extract_path : Str -> (Str, Str)
extract_path = |uri|
    when Str.split_first(uri, "/") is
        Ok({ before: rest, after: path }) -> (Str.concat("/", path), rest)
        _ -> ("/", uri)

extract_userinfo : Str -> (Maybe Str, Str)
extract_userinfo = |uri|
    when Str.split_first(uri, "@") is
        Ok({ before: userinfo, after: rest }) if userinfo != "" -> (Just(userinfo), rest)
        _ -> (Nothing, uri)

extract_host_and_port : Str -> Result (Maybe Str, Maybe U16) _
extract_host_and_port = |uri|
    if uri == "" then
        Ok((Nothing, Nothing))
    else
        when Str.split_first(uri, ":") is
            Ok({ before: host, after: port_str }) ->
                if host == "" then
                    Err(InvalidHost(host))
                else
                    when Str.to_u16(port_str) is
                        Ok(port) -> Ok((Just(host), Just(port)))
                        _ -> Err(InvalidPort(port_str))

            _ -> Ok((Just(uri), Nothing))

expect
    input = "https://www.example.com:8080/path?query=val#fragment"
    expected = Ok(
        Absolute(
            {
                protocol: Just("https"),
                host: "www.example.com",
                port: Just(8080),
                userinfo: Nothing,
                path: "/path",
                query: Just("query=val"),
                fragment: Just("fragment"),
            },
        ),
    )
    output = parse_uri(input)
    output == expected

expect
    input = "ftp://localhost"
    expected = Ok(
        Absolute(
            {
                protocol: Just("ftp"),
                userinfo: Nothing,
                host: "localhost",
                port: Nothing,
                path: "/",
                query: Nothing,
                fragment: Nothing,
            },
        ),
    )
    output = parse_uri(input)
    output == expected

expect
    input = "http://example.com/path/to/resource"
    expected = Ok(
        Absolute(
            {
                protocol: Just("http"),
                userinfo: Nothing,
                host: "example.com",
                port: Nothing,
                path: "/path/to/resource",
                query: Nothing,
                fragment: Nothing,
            },
        ),
    )
    output = parse_uri(input)
    output == expected

expect
    input = "invalid uri"
    expected = Err(InvalidHost("invalid uri"))
    output = parse_uri(input)
    output == expected

expect
    input = ""
    expected = Err(InvalidUri)
    output = parse_uri(input)
    output == expected

expect
    input = "https://"
    expected = Err(InvalidUri)
    output = parse_uri(input)
    output == expected

expect
    input = "https://api.example.com?key=value"
    expected = Ok(
        Absolute(
            {
                protocol: Just("https"),
                userinfo: Nothing,
                host: "api.example.com",
                port: Nothing,
                path: "/",
                query: Just("key=value"),
                fragment: Nothing,
            },
        ),
    )
    output = parse_uri(input)
    output == expected

expect
    input = "example.com:8080/path?query=val#fragment"
    expected = Ok(
        Absolute(
            {
                protocol: Nothing,
                userinfo: Nothing,
                host: "example.com",
                port: Just(8080),
                path: "/path",
                query: Just("query=val"),
                fragment: Just("fragment"),
            },
        ),
    )
    output = parse_uri(input)
    output == expected

expect
    input = "/path?query=val#fragment"
    expected = Ok(
        Relative {
            path: "/path",
            query: Just("query=val"),
            fragment: Just("fragment"),
        },
    )
    output = parse_uri(input)
    output == expected

# Tests a basic URI with userinfo component (username and password)
expect
    input = "https://user:pass@example.com/path"
    expected = Ok(
        Absolute(
            {
                protocol: Just("https"),
                userinfo: Just("user:pass"),
                host: "example.com",
                port: Nothing,
                path: "/path",
                query: Nothing,
                fragment: Nothing,
            },
        ),
    )
    output = parse_uri(input)
    output == expected

# Tests a URI with an invalid port (non-numeric)
expect
    input = "example.com:invalid"
    expected = Err(InvalidPort("invalid"))
    output = parse_uri(input)
    output == expected

# Tests a URI with a port number that exceeds U16 maximum
expect
    input = "example.com:65536"
    expected = Err(InvalidPort("65536"))
    output = parse_uri(input)
    output == expected

# Tests a URI with port and complex query parameters
expect
    input = "http://example.com:8080/?param=value&other=123"
    expected = Ok(
        Absolute(
            {
                protocol: Just("http"),
                userinfo: Nothing,
                host: "example.com",
                port: Just(8080),
                path: "/",
                query: Just("param=value&other=123"),
                fragment: Nothing,
            },
        ),
    )
    output = parse_uri(input)
    output == expected

# Tests a relative path with both query and fragment
expect
    input = "/path/to/resource?query#fragment"
    expected = Ok(
        Relative {
            path: "/path/to/resource",
            query: Just("query"),
            fragment: Just("fragment"),
        },
    )
    output = parse_uri(input)
    output == expected

# Tests a URI with only a fragment component
expect
    input = "#fragment-only"
    expected = Err(InvalidUri)
    output = parse_uri(input)
    output == expected

# Tests a URI with userinfo but missing host (invalid)
expect
    input = "https://user@:8080/"
    expected = Err(InvalidHost(""))
    output = parse_uri(input)
    output == expected

# Tests a URI with a subdomain and file extension
expect
    input = "https://api.example.com/v1/data.json"
    expected = Ok(
        Absolute(
            {
                protocol: Just("https"),
                userinfo: Nothing,
                host: "api.example.com",
                port: Nothing,
                path: "/v1/data.json",
                query: Nothing,
                fragment: Nothing,
            },
        ),
    )
    output = parse_uri(input)
    output == expected

# Tests a URI with a complex path containing special characters
expect
    input = "https://example.com/path/with/special%20characters"
    expected = Ok(
        Absolute(
            {
                protocol: Just("https"),
                userinfo: Nothing,
                host: "example.com",
                port: Nothing,
                path: "/path/with/special%20characters",
                query: Nothing,
                fragment: Nothing,
            },
        ),
    )
    output = parse_uri(input)
    output == expected

# Tests a URI with a complex fragment containing query-like parameters
expect
    input = "https://example.com/page#section?id=123"
    expected = Ok(
        Absolute(
            {
                protocol: Just("https"),
                userinfo: Nothing,
                host: "example.com",
                port: Nothing,
                path: "/page",
                query: Nothing,
                fragment: Just("section?id=123"),
            },
        ),
    )
    output = parse_uri(input)
    output == expected

# Tests a basic URI with non-standard port
expect
    input = "http://localhost:3000/"
    expected = Ok(
        Absolute(
            {
                protocol: Just("http"),
                userinfo: Nothing,
                host: "localhost",
                port: Just(3000),
                path: "/",
                query: Nothing,
                fragment: Nothing,
            },
        ),
    )
    output = parse_uri(input)
    output == expected
