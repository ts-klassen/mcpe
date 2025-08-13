-module(mcpe_server_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

%% This suite defines the expected behavior for mcpe_server.
%% It intentionally fails today since mcpe_server is not implemented yet.
%% It does not depend on mcpe_client or mcpe_server_sample.

all() -> [t_initialize_ready,
          t_tools_list,
          t_tools_call_echo,
          t_notification_roundtrip,
          t_broadcast].

init_per_suite(Config) -> Config.
end_per_suite(Config) -> Config.

t_initialize_ready(Config) ->
    {Server, Sock} = start_and_connect(Config),
    try
    ok = send_req(Sock, 1, <<"initialize">>, #{}),
        {ok, 1, #{
            <<"protocolVersion">> := _PV,
            <<"capabilities">> := _Caps,
            <<"serverInfo">> := #{<<"name">> := _Name, <<"version">> := _Ver}
        }} = recv_resp(Sock),
        {ok, {notification, <<"notifications/ready">>, _Params}} = recv_line(Sock)
    after
        cleanup(Server, Sock)
    end.

t_tools_list(Config) ->
    {Server, Sock} = start_and_connect(Config),
    try
        ok = send_req(Sock, 2, <<"tools/list">>, #{}),
        {ok, 2, #{<<"tools">> := Tools}} = recv_resp(Sock),
        true = is_list(Tools),
        ok
    after
        cleanup(Server, Sock)
    end.

t_tools_call_echo(Config) ->
    {Server, Sock} = start_and_connect(Config),
    try
        Args = #{<<"name">> => <<"echo">>, <<"arguments">> => #{<<"text">> => <<"hi">>}},
        ok = send_req(Sock, 3, <<"tools/call">>, Args),
        {ok, 3, #{<<"content">> := [#{<<"type">> := <<"text">>, <<"text">> := <<"hi">>} ]}} = recv_resp(Sock),
        ok
    after
        cleanup(Server, Sock)
    end.

t_notification_roundtrip(Config) ->
    {Server, Sock} = start_and_connect(Config),
    try
        ok = send_notif(Sock, <<"notifications/initialized">>, #{}),
        {ok, {notification, <<"notifications/ready">>, _}} = recv_line(Sock),
        ok
    after
        cleanup(Server, Sock)
    end.

t_broadcast(Config) ->
    {Server, Sock} = start_and_connect(Config),
    try
        ok = mcpe_server:broadcast(Server, <<"server/ping">>, #{<<"x">> => 1}),
        {ok, {notification, <<"server/ping">>, #{<<"x">> := 1}}} = recv_line(Sock),
        ok
    after
        cleanup(Server, Sock)
    end.

%% Helpers

start_and_connect(_Config) ->
    Path = "/tmp/mcpe_test_" ++ integer_to_list(erlang:unique_integer([monotonic, positive])) ++ ".sock",
    {ok, Server} = mcpe_server:start_link(#{
        path => Path,
        handler => {mcpe_server_test_handler, #{}}
    }),
    {ok, Sock} = socket:open(local, stream, default),
    ok = socket:connect(Sock, #{family => local, path => Path}),
    {Server, Sock}.

cleanup(Server, Sock) ->
    _ = catch socket:close(Sock),
    _ = catch mcpe_server:stop(Server),
    ok.

send_req(Sock, Id, Method, Params) ->
    Json = #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => Id, <<"method">> => Method, <<"params">> => Params},
    _ = socket:send(Sock, [jsone:encode(Json), <<"\n">>]),
    ok.

send_notif(Sock, Method, Params) ->
    Json = #{<<"jsonrpc">> => <<"2.0">>, <<"method">> => Method, <<"params">> => Params},
    _ = socket:send(Sock, [jsone:encode(Json), <<"\n">>]),
    ok.

recv_resp(Sock) ->
    case recv_line(Sock) of
        {ok, {response, Id, {ok, Result}}} -> {ok, Id, Result};
        Other -> Other
    end.

recv_line(Sock) ->
    Buf0 = case erlang:get({mcpe_test_buf, Sock}) of undefined -> <<>>; B -> B end,
    erlang:put({mcpe_test_buf, Sock}, <<>>),
    recv_line(Sock, Buf0, 5000).

recv_line(Sock, Acc, Timeout) ->
    case binary:match(Acc, <<"\n">>) of
        {Pos, 1} ->
            <<Line:Pos/binary, _Sep:1/binary, Rest/binary>> = Acc,
            case decode_line(Line) of
                {ok, Term} ->
                    case Rest of <<>> -> ok; _ -> erlang:put({mcpe_test_buf, Sock}, Rest) end,
                    {ok, Term};
                Err -> Err
            end;
        nomatch ->
            case socket:recv(Sock, 0, Timeout) of
                {ok, Data} -> recv_line(Sock, <<Acc/binary, Data/binary>>, Timeout);
                {error, E} -> {error, E}
            end
    end.

decode_line(Line) ->
    try
        Map = jsone:decode(Line),
        case classify_jsonrpc(Map) of
            Resp = {response, _, _} -> {ok, Resp};
            Notif = {notification, _, _} -> {ok, Notif};
            _ -> {error, malformed}
        end
    catch _:_ -> {error, badjson}
    end.

classify_jsonrpc(Map) when is_map(Map) ->
    Id = maps:get(<<"id">>, Map, undefined),
    case {maps:is_key(<<"result">>, Map), maps:is_key(<<"error">>, Map)} of
        {true, false} -> {response, Id, {ok, maps:get(<<"result">>, Map)}};
        {false, true} -> {response, Id, {error, maps:get(<<"error">>, Map)}};
        _ ->
            case {Id, maps:get(<<"method">>, Map, undefined), maps:get(<<"params">>, Map, undefined)} of
                {undefined, MethodBin, Params} when is_binary(MethodBin) -> {notification, MethodBin, Params};
                {Id2, MethodBin, Params} when Id2 =/= undefined, is_binary(MethodBin) -> {request, Id2, MethodBin, Params};
                _ -> other
            end
    end;
classify_jsonrpc(_) -> other.
