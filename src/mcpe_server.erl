-module(mcpe_server).

-export([
    start_link/1,
    stop/1,
    notify/3,
    broadcast/3
]).

-export_type([
    server/0,
    conn/0
]).

-define(DEFAULT_BACKLOG, 16).

-opaque server() :: pid().
-opaque conn() :: pid().

-type config() :: #{
    path := binary(),
    handler := {module(), term()},
    backlog => pos_integer()
}.

%% API

-spec start_link(config()) -> {ok, server()} | {error, term()}.
start_link(Conf) when is_map(Conf) ->
    Path0 = maps:get(path, Conf),
    Path = case Path0 of
        B when is_binary(B) -> binary_to_list(B);
        L when is_list(L) -> L
    end,
    {HandlerMod, InitArg} = maps:get(handler, Conf),
    Backlog = maps:get(backlog, Conf, ?DEFAULT_BACKLOG),
    try
        ensure_unix_path_available(Path),
        {ok, LSock} = socket:open(local, stream, default),
        ok = socket:bind(LSock, #{family => local, path => Path}),
        ok = socket:listen(LSock, Backlog),
        Parent = self(),
        Pid = spawn_link(fun() -> server_loop(LSock, Path, HandlerMod, InitArg, [], Parent) end),
        {ok, Pid}
    catch C:R:Stk -> {error, {C, R, Stk}}
    end.

-spec stop(server()) -> ok.
stop(Server) when is_pid(Server) ->
    Server ! stop,
    ok.

-spec notify(conn(), binary(), term()) -> ok.
notify(Conn, Method, Params) when is_pid(Conn), is_binary(Method) ->
    Conn ! {send_notification, Method, Params},
    ok.

-spec broadcast(server(), binary(), term()) -> ok.
broadcast(Server, Method, Params) when is_pid(Server), is_binary(Method) ->
    Server ! {broadcast, Method, Params},
    ok.

%% Internal

ensure_unix_path_available(Path) ->
    case file:read_file_info(Path) of
        {ok, _} -> _ = file:delete(Path), ok;
        _ -> ok
    end.

server_loop(LSock, Path, HandlerMod, InitArg, Conns, _Parent) ->
    process_flag(trap_exit, true),
    Conns1 =
        case socket:accept(LSock, 100) of
            {ok, Sock} ->
                CPid = spawn_link(fun() -> conn_loop(Sock, HandlerMod, InitArg) end),
                [CPid | Conns];
            {error, timeout} -> Conns;
            {error, closed} ->
                cleanup(LSock, Path),
                exit(normal);
            {error, _Other} -> Conns
        end,
    receive
        stop ->
            lists:foreach(fun(P) -> catch exit(P, shutdown) end, Conns1),
            cleanup(LSock, Path),
            exit(normal);
        {broadcast, Method, Params} ->
            lists:foreach(fun(P) -> P ! {send_notification, Method, Params} end, Conns1),
            server_loop(LSock, Path, HandlerMod, InitArg, Conns1, self());
        {'EXIT', Dead, _Reason} ->
            server_loop(LSock, Path, HandlerMod, InitArg, lists:delete(Dead, Conns1), self())
    after 0 ->
        server_loop(LSock, Path, HandlerMod, InitArg, Conns1, self())
    end.

cleanup(LSock, Path) ->
    catch socket:close(LSock),
    _ = file:delete(Path),
    ok.

conn_loop(Sock, HandlerMod, InitArg) ->
    process_flag(trap_exit, true),
    State0 = case safe_init(HandlerMod, InitArg) of {ok, S} -> S; _ -> #{} end,
    conn_recv_loop(Sock, HandlerMod, State0, <<>>).

conn_recv_loop(Sock, HandlerMod, State, Acc0) ->
    receive
        {send_notification, Method, Params} ->
            send_notification(Sock, Method, Params),
            conn_recv_loop(Sock, HandlerMod, State, Acc0)
    after 0 ->
        case socket:recv(Sock, 0, 200) of
            {ok, Data} ->
                Acc1 = <<Acc0/binary, Data/binary>>,
                {Lines, Rest} = take_lines(Acc1, []),
                case handle_lines(Sock, HandlerMod, State, Lines) of
                    {ok, NewState} -> conn_recv_loop(Sock, HandlerMod, NewState, Rest);
                    stop -> catch socket:close(Sock), ok
                end;
            {error, timeout} -> conn_recv_loop(Sock, HandlerMod, State, Acc0);
            {error, closed} -> catch socket:close(Sock), ok;
            {error, _} -> catch socket:close(Sock), ok
        end
    end.

handle_lines(_Sock, _HandlerMod, State, []) -> {ok, State};
handle_lines(Sock, HandlerMod, State0, [Line | Rest]) ->
    case handle_line(Sock, HandlerMod, State0, Line) of
        {ok, State1} -> handle_lines(Sock, HandlerMod, State1, Rest);
        stop -> stop
    end.

handle_line(Sock, HandlerMod, State, Line) ->
    try
        Map = jsone:decode(Line),
        case classify_jsonrpc(Map) of
            {request, Id, Method, Params} ->
                case safe_handle_request(HandlerMod, Method, Params, self(), State) of
                    {reply, Result, NewState} ->
                        send_response_ok(Sock, Id, Result),
                        {ok, NewState};
                    {reply_error, Code, Msg, Data, NewState2} ->
                        send_response_error(Sock, Id, Code, Msg, Data),
                        {ok, NewState2};
                    _ -> {ok, State}
                end;
            {notification, Method, Params} ->
                case safe_handle_notification(HandlerMod, Method, Params, self(), State) of
                    {noreply, NewState} -> {ok, NewState};
                    _ -> {ok, State}
                end;
            _Other -> {ok, State}
        end
    catch _:_ -> {ok, State}
    end.

send_response_ok(Sock, Id, Result) ->
    Json = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"result">> => Result
    },
    _ = socket:send(Sock, [jsone:encode(Json), <<"\n">>]),
    ok.

send_response_error(Sock, Id, Code, Msg, Data) ->
    Json = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"error">> => #{
            <<"code">> => Code,
            <<"message">> => Msg,
            <<"data">> => Data
        }
    },
    _ = socket:send(Sock, [jsone:encode(Json), <<"\n">>]),
    ok.

send_notification(Sock, Method, Params) ->
    Json = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => Method,
        <<"params">> => Params
    },
    _ = socket:send(Sock, [jsone:encode(Json), <<"\n">>]),
    ok.

take_lines(Bin, Acc) ->
    case binary:match(Bin, <<"\n">>) of
        nomatch -> {lists:reverse(Acc), Bin};
        {Pos, 1} ->
            <<Line:Pos/binary, _Sep:1/binary, Rest/binary>> = Bin,
            take_lines(Rest, [Line | Acc])
    end.

safe_init(Mod, Arg) ->
    try Mod:init(Arg) catch _:_ -> {ok, #{}} end.

safe_handle_request(Mod, Method, Params, Conn, State) ->
    try Mod:handle_request(Method, Params, Conn, State)
    catch _:_ -> {reply_error, -32603, <<"Internal error">>, null, State} end.

safe_handle_notification(Mod, Method, Params, Conn, State) ->
    try Mod:handle_notification(Method, Params, Conn, State)
    catch _:_ -> {noreply, State} end.

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
