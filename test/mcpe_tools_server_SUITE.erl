-module(mcpe_tools_server_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%%=====================================================================
%% Common Test callbacks
%%=====================================================================

all() -> [tools_lifecycle].

%%--------------------------------------------------------------------
%% @doc
%% Find a free TCP port on the local machine by letting the OS allocate
%% a random available port (by binding to port 0). The chosen port is
%% returned and can subsequently be used to start listeners without the
%% risk of running into port collisions during parallel test runs.
%%--------------------------------------------------------------------

find_free_port() ->
    {ok, Listen} = gen_tcp:listen(0, [{reuseaddr, true}]),
    {ok, Port} = inet:port(Listen),
    ok = gen_tcp:close(Listen),
    Port.

init_per_suite(Config) ->
    Port = find_free_port(),
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

