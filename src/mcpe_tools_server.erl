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
init(Args = #{tools := ToolMods}) ->
    %% Build name-to-module map once for fast lookup during execute.
    ToolMap = maps:from_list([{maps:get(name, M:descriptor()), M} || M <- ToolMods]),
    {ok, Args#{tool_map => ToolMap}}.

-spec handle(klsn:binstr(), mcpe_client:payload(), mcpe_server:state()) ->
          mcpe_server:handle_result().
%% Handle incoming JSON-RPC requests.
handle(Method, Params, State = #{tools := ToolMods, tool_map := ToolMap}) ->

    case Method of
        <<"mcp.tools/list">> ->
            Descriptors = [M:descriptor() || M <- ToolMods],
            {reply, Descriptors, State};

        <<"mcp.tools/execute">> ->
            handle_execute(Params, ToolMap, State);

        _ -> {error, method_not_found}
    end.

%% Execute a tool using the pre-built ToolMap.
handle_execute(Params, ToolMap, State) when is_map(Params) ->
    Name = maps:get(<<"name">>, Params, undefined),
    Args = maps:get(<<"args">>, Params, #{}),
    case find_tool(Name, ToolMap) of
        {ok, Mod} ->
            case Mod:execute(Args, #{caller => self()}) of
                {ok, Result} -> {reply, Result, State};
                {error, Reason} -> {error, Reason}
            end;
        error -> {error, method_not_found}
    end;
handle_execute(_Other, _ToolMap, _State) ->
    {error, invalid_params}.

%%--------------------------------------------------------------------
%% Helper functions
%%--------------------------------------------------------------------

%% @private
%% Fast lookup of a tool by name in the pre-computed ToolMap.
find_tool(Name, ToolMap) ->
    case maps:find(Name, ToolMap) of
        {ok, Mod} -> {ok, Mod};
        error -> error
    end.
