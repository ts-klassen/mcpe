-module(mcpe_tools_server_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%%=====================================================================
%% Common Test callbacks
%%=====================================================================

all() -> [tools_lifecycle].

init_per_suite(Config) ->
    Port = 8080,
    ToolModules = [mcpe_sample_tool],

    {ok, Pid} = mcpe_server:start_link(Port,
                                       mcpe_tools_server,
                                       #{tools => ToolModules},
                                       #{path => <<"/mcp">>}),

    [{server_pid, Pid}, {port, Port} | Config].

end_per_suite(Config) ->
    case lists:keyfind(server_pid, 1, Config) of
        {server_pid, Pid} when is_pid(Pid) -> exit(Pid, shutdown);
        _ -> ok
    end,
    ok.

%%=====================================================================
%% Test cases
%%=====================================================================

tools_lifecycle(Config) ->
    {port, Port} = lists:keyfind(port, 1, Config),

    URL = iolist_to_binary([<<"http://localhost:">>, integer_to_binary(Port), <<"/mcp">>]),

    {ok, ToolList} = mcpe_client:list_tools(URL, []),

    true = lists:any(fun
        (#{ <<"name">> := <<"sample">> }) ->
            true;
        (_) ->
            false
    end, ToolList),

    {ok, Result} = mcpe_client:execute_tool(URL, <<"sample">>, #{}, []),

    <<"done">> = Result,

    ok.

