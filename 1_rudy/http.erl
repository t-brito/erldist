-module(http).                            % load http module
-export([parse_request/1]).               % export parse_request function (one argument)
-export([ok/1]).                          % export ok
-export([get/1]).

% PARSE REQUEST

parse_request(R0) ->                      % define parse_request (one argument)
  {Request, R1} = request_line(R0),       % get the Request-Line (function defined below) and assign the rest to R1
  {Headers, R2} = headers(R1),            % get the Headers (function defined below) and assign the rest to R2
  {Body, _} = message_body(R2),           % get the message-body (function defined below), nothing should be left, if it is, assign to anonymous variable
  {Request, Headers, Body}.               % return 3 element tuple

% REQUEST LINE
request_line([$G, $E, $T, 32 |R0]) ->     % only concerned with GET messages, match on "GET "
  {URI, R1} = request_uri(R0),            % get the Request-URI (function defined below) and assign the rest to R1
  {Ver, R2} = http_version(R1),           % get the HTTP-Version (function defined below) and assign the rest to R2
  [13,10|R3] = R2,                        % match with CRLF and assign the rest to R3
  {{get, URI, Ver}, R3}.                  % return 2 element tuple (3 element tuple (Request) + remainder (unparsed Headers, Body, etc...)

% REQUEST URI
request_uri([32|R0]) ->
  {[], R0};

request_uri([C|R0]) ->                    % add each character C to C|Rest until you're left with the string (trailing space is converted to [])
  {Rest, R1} = request_uri(R0),
  {[C|Rest], R1}.                         % return 2 element tuple (uri + remainder)

% HTTP VERSION
http_version([$H, $T, $T, $P, $/, $1, $., $1 | R0]) ->    % match string to "HTTP/1.1"
  {v11, R0};

http_version([$H, $T, $T, $P, $/, $1, $., $0 | R0]) ->    % match string to "HTTP/1.0"
  {v10, R0}.

% HEADERS

headers([13,10|R0]) ->                % remove CRLF and return headers so far
  {[],R0};

headers(R0) ->
  {Header, R1} = header(R0),          % retrieve individual header (assume this removes the CRLF between headers)
  {Rest, R2} = headers(R1),           % move on to next header, unless CRLF is found (defined above)
  {[Header|Rest], R2}.                % return 2 element tuple (header list + remainder)

header([13,10|R0]) ->                 % remove CRLF at the end of each header
  {[], R0};

header([C|R0]) ->                     % retrieve individual character until CRLF is found
  {Rest, R1} = header(R0),            % recursive call
  {[C|Rest], R1}.                     % return 2 element tuple (string for each header + rest)

% BODY

message_body(R) ->          % second element in tuple to be ignored - why include it?
  {R, []}.

% ADDITIONAL FUNCTIONS

ok(Body) ->
  "HTTP/1.1 200 OK\r\n" ++ "\r\n" ++ Body.

get(URI) ->
  "GET " ++ URI ++ " HTTP/1.1\r\n" ++ "\r\n".
