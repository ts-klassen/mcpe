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
list_tools(_Url, _Opts) ->
    todo.

-spec execute_tool(url(), klsn:binstr(), payload(), client_opts()) ->
          {ok, mcpe_server:payload()} | {error, term()}.
execute_tool(_Url, _Name, _Args, _Opts) ->
    todo.

%%--------------------------------------------------------------------
%% Low-level JSON-RPC transport helper
%%--------------------------------------------------------------------

-spec send(url(), payload(), client_opts()) -> {ok, mcpe_server:payload()} | {error, term()}.
send(_Url, _Payload, _Opts) ->
    todo.
