%%%-------------------------------------------------------------------
%%% @doc Convenience client helpers for talking to an MCP server over HTTP.
%%%-------------------------------------------------------------------

-module(mcpe_client).

-export([
    list_tools/2,
    execute_tool/4,
    send/3
]).

-export_type([
    url/0,
    client_opts/0,
    payload/0
]).

%%--------------------------------------------------------------------
%% Types
%%--------------------------------------------------------------------

-type url() :: klsn:binstr().

%% Currently no recognised client options; empty map subtype allows future keys.
-type client_opts() :: #{}.

%% JSON-RPC request and response structure on the client side.
-type payload() :: jsone:json_value().

%%--------------------------------------------------------------------
%% High-level helpers
%%--------------------------------------------------------------------

-spec list_tools(url(), client_opts()) -> {ok, [mcpe_tool:descriptor()]} | {error, term()}.
list_tools(Url, Opts) ->
    Id = make_id(),
    Payload = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">>  => <<"mcp.tools/list">>,
        <<"params">>  => #{},
        <<"id">>      => Id
    },
    case send(Url, Payload, Opts) of
        {ok, Resp} -> parse_single_response(Resp);
        Error -> Error
    end.

-spec execute_tool(url(), klsn:binstr(), payload(), client_opts()) ->
          {ok, mcpe_server:payload()} | {error, term()}.
execute_tool(Url, Name, Args, Opts) ->
    Id = make_id(),
    Params = #{ <<"name">> => Name, <<"args">> => Args },
    Payload = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">>  => <<"mcp.tools/execute">>,
        <<"params">>  => Params,
        <<"id">>      => Id
    },
    case send(Url, Payload, Opts) of
        {ok, Resp} -> parse_single_response(Resp);
        Error -> Error
    end.

%%--------------------------------------------------------------------
%% Low-level JSON-RPC transport helper
%%--------------------------------------------------------------------

-spec send(url(), payload(), client_opts()) -> {ok, mcpe_server:payload()} | {error, term()}.
send(Url, Payload, _Opts) ->
    ensure_httpc(),
    Bin = jsone:encode(Payload),
    Headers = [],
    ContentType = "application/json", % httpc expects string
    Request = {Url, Headers, ContentType, Bin},
    %% Set a finite timeout so the client does not hang forever if the server
    %% fails to respond.  The timeout value is given in milliseconds and
    %% applies to the entire request/response transaction (see httpc manual).
    %% A conservative default of 30 seconds is used; this can be made
    %% configurable via client_opts() in the future if needed.
    Timeout = 30000, % 30 seconds
    Options = [{timeout, Timeout}],
    ReqOpts = [{body_format, binary}],
    case httpc:request(post, Request, Options, ReqOpts) of
        {ok, {{_Version, 200, _ReasonPhrase}, _RespHeaders, RespBody}} ->
            case catch jsone:decode(RespBody) of
                {'EXIT', Reason} -> {error, {invalid_json, Reason}};
                Decoded -> {ok, Decoded}
            end;
        {ok, {{_Version, 204, _}, _RespHeaders, _Body}} ->
            {ok, undefined};
        {ok, {{_Version, Status, _}, _RespHeaders, Body}} ->
            {error, {http_status, Status, Body}};
        {error, Reason} -> {error, {transport_error, Reason}}
    end.

%%--------------------------------------------------------------------
%% Internal helpers
%%--------------------------------------------------------------------

ensure_httpc() ->
    application:ensure_all_started(inets).

make_id() -> erlang:unique_integer([monotonic, positive]).

%%
%% JSON-RPC 2.0 response validator.
%%
%% Ensures that the decoded response obeys the following minimal rules:
%%   * Contains the key "jsonrpc" with the exact value "2.0".
%%   * Contains **either** a "result" or an "error" member.
%%
%% Any violation results in `{error, {protocol_violation, Reason}}` where
%% `Reason` is helpful for debugging but opaque to callers.

parse_single_response(undefined) -> {ok, undefined};

%% Map representation --------------------------------------------------------

parse_single_response(#{ <<"jsonrpc">> := <<"2.0">> } = Resp) ->
    HasResult = maps:is_key(<<"result">>, Resp),
    HasError  = maps:is_key(<<"error">>, Resp),
    case {HasResult, HasError} of
        {true, false}  -> {ok, maps:get(<<"result">>, Resp)};
        {false, true}  -> {error, maps:get(<<"error">>, Resp)};
        {true, true}   -> {error, {protocol_violation, both_result_and_error}};
        {false, false} -> {error, {protocol_violation, missing_result_and_error}}
    end;

%% Map that lacks the required "jsonrpc" marker.
parse_single_response(#{ } = Resp) when is_map(Resp) ->
    {error, {protocol_violation, missing_jsonrpc}};

%% Proplist representation ---------------------------------------------------

parse_single_response({Resp}) when is_list(Resp) ->
    JsonRpcVer = proplists:get_value(<<"jsonrpc">>, Resp),
    case JsonRpcVer of
        <<"2.0">> ->
            Result  = proplists:get_value(<<"result">>, Resp, undefined),
            Error   = proplists:get_value(<<"error">>, Resp, undefined),
            case {Result, Error} of
                {R, undefined} when R =/= undefined -> {ok, R};
                {undefined, E} when E =/= undefined -> {error, E};
                {R, E} when R =/= undefined, E =/= undefined ->
                    {error, {protocol_violation, both_result_and_error}};
                _ -> {error, {protocol_violation, missing_result_and_error}}
            end;
        _Other -> {error, {protocol_violation, missing_jsonrpc}}
    end;

%% Fallback for completely unexpected data structures.
parse_single_response(Other) -> {error, {unexpected_response, Other}}.
