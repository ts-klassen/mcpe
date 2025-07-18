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
start_link(Port, ToolModules, Opts) ->
    mcpe_server:start_link(Port, ?MODULE, #{tools => ToolModules}, Opts).

%%=====================================================================
%%  mcpe_server behaviour callbacks
%%=====================================================================

-spec init(mcpe_server:args()) -> {ok, mcpe_server:state()}.
init(Args) ->
    {ok, Args}.

-spec handle(klsn:binstr(), mcpe_client:payload(), mcpe_server:state()) ->
          mcpe_server:handle_result().
handle(Method, Params, State = #{tools := ToolMods}) ->

    case Method of
        <<"mcp.tools/list">> ->
            Descriptors = [M:descriptor() || M <- ToolMods],
            {reply, Descriptors, State};

        <<"mcp.tools/execute">> ->
            handle_execute(Params, ToolMods, State);

        _ -> {error, method_not_found}
    end.

handle_execute(Params, ToolMods, State) when is_map(Params) ->
    Name = maps:get(<<"name">>, Params, undefined),
    Args = maps:get(<<"args">>, Params, #{}),
    case find_tool(Name, ToolMods) of
        {ok, Mod} ->
            case Mod:execute(Args, #{caller => self()}) of
                {ok, Result} -> {reply, Result, State};
                {error, Reason} -> {error, Reason}
            end;
        error -> {error, method_not_found}
    end;
handle_execute(_Other, _ToolMods, _State) ->
    {error, invalid_params}.

find_tool(Name, Modules) ->
    case lists:filter(fun(M) -> maps:get(name, M:descriptor(), undefined) =:= Name end, Modules) of
        [Mod|_] -> {ok, Mod};
        [] -> error
    end.

-spec capabilities(mcpe_server:state()) -> {mcpe_server:payload(), mcpe_server:state()}.
capabilities(State) ->
    {#{ <<"tools">> => #{ <<"list">> => true, <<"execute">> => true }}, State}.
