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
    Options = [],
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

parse_single_response(undefined) -> {ok, undefined};
parse_single_response(#{ <<"result">> := Result }) -> {ok, Result};
parse_single_response(#{ <<"error">> := ErrorObj }) -> {error, ErrorObj};
%% proplist representation
parse_single_response({Resp}) ->
    case proplists:get_value(<<"error">>, Resp) of
        undefined ->
            {ok, proplists:get_value(<<"result">>, Resp)};
        Err -> {error, Err}
    end;
parse_single_response(Other) -> {error, {unexpected_response, Other}}.
