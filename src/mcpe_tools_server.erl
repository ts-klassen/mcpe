%%%-------------------------------------------------------------------
%%% @doc Convenience implementation of the mcpe_server behaviour that exposes
%%%       only the *Tools* feature.  Users supply a list of tool modules and
%%%       this server handles mcp.tools/list and mcp.tools/execute.
%%%

%%%-------------------------------------------------------------------

-module(mcpe_tools_server).

-behaviour(mcpe_server).

%%=====================================================================
%%  API exports
%%=====================================================================

-export([
    start_link/3,
    init/1,
    handle/3
]).

-export_type([
    tool_modules/0
]).

%%=====================================================================
%%  Types
%%=====================================================================

-type tool_modules() :: [module()].

%%=====================================================================
%%  Public API
%%=====================================================================

-spec start_link(non_neg_integer(), tool_modules(), mcpe_server:opts()) ->
          {ok, pid()} | {error, term()}.
start_link(_Port, _ToolModules, _Opts) ->
    todo.

%%=====================================================================
%%  mcpe_server behaviour callbacks
%%=====================================================================

-spec init(mcpe_server:args()) -> {ok, mcpe_server:state()}.
init(_Args) ->
    todo.

-spec handle(klsn:binstr(), mcpe_client:payload(), mcpe_server:state()) ->
          mcpe_server:handle_result().
handle(_, _, _State) ->
    todo.

-spec capabilities(mcpe_server:state()) -> {mcpe_server:payload(), mcpe_server:state()}.
capabilities(_State) ->
    todo.
