-module(mcpe_client).

%% Minimal MCP stdio client using jsonrpc2 and jsone

-export([
    start/1,
    stop/1,
    request/3,
    call_async/3,
    recv/1,
    recv/2,
    respond/3,
    notify/3,
    initialize/2
]).

-export_type([
    client/0,
    config/0
]).

-record(client, {
    port       :: port(),
    json_encode :: fun((term()) -> binary()),
    json_decode :: fun((binary()) -> term())
}).

-opaque client() :: #client{}.
-type config() :: #{
    exec := iodata(),
    args => [iodata()],
    env => [{string(), string()}],
    json_encode => fun((term()) -> binary()),
    json_decode => fun((binary()) -> term())
}.

%% API

-spec start(config()) -> {ok, client()} | {error, term()}.
start(Conf0) when is_map(Conf0) ->
    ExecIo = maps:get(exec, Conf0),
    Exec = iolist_to_binary(ExecIo),
    Args0 = maps:get(args, Conf0, []),
    Args = [binary_to_list(iolist_to_binary(A)) || A <- Args0],
    Env = maps:get(env, Conf0, []),
    Enc = maps:get(json_encode, Conf0, fun jsone:encode/1),
    Dec = maps:get(json_decode, Conf0, fun jsone:decode/1),
    PortSettings = [
        exit_status,
        use_stdio,
        binary,
        stream,
        eof,
        {args, Args},
        {env, Env}
    ],
    try
        Port = open_port({spawn_executable, binary_to_list(Exec)}, PortSettings),
        {ok, #client{port=Port, json_encode=Enc, json_decode=Dec}}
    catch C:R -> {error, {C, R}}
    end.

-spec stop(client()) -> ok.
stop(#client{port=Port}) ->
    catch port_close(Port),
    ok.

-spec initialize(client(), map()) -> {ok, term()} | {error, term()}.
initialize(Client, Params) when is_map(Params) ->
    request(Client, <<"initialize">>, Params).

-spec request(client(), binary(), term()) -> {ok, term()} | {error, term()}.
request(#client{}=C0, Method, Params) when is_binary(Method) ->
    Id = erlang:unique_integer([monotonic]),
    Json = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"method">> => Method,
        <<"params">> => Params
    },
    Enc = C0#client.json_encode,
    Bin = Enc(Json),
    send_line(C0#client.port, Bin),
    Dec = C0#client.json_decode,
    recv_response_for_id(C0#client.port, Dec, Id).

-spec call_async(client(), binary(), term()) -> {ok, integer()} | {error, term()}.
call_async(#client{}=C, Method, Params) when is_binary(Method) ->
    Id = erlang:unique_integer([monotonic]),
    Json = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"method">> => Method,
        <<"params">> => Params
    },
    Enc = C#client.json_encode,
    Bin = Enc(Json),
    send_line(C#client.port, Bin),
    {ok, Id}.

-spec recv(client()) -> {response, integer() | binary() | null, term()} |
                         {notification, binary(), term()} |
                         {request, integer() | binary(), binary(), term()} |
                         other | {error, term()}.
recv(#client{}=C) ->
    recv(C, 30000).

-spec recv(client(), non_neg_integer()) -> {response, integer() | binary() | null, term()} |
                                            {notification, binary(), term()} |
                                            {request, integer() | binary(), binary(), term()} |
                                            other | {error, term()}.
recv(#client{}=C, TimeoutMs) when is_integer(TimeoutMs), TimeoutMs >= 0 ->
    Dec = C#client.json_decode,
    case recv_line(C#client.port, TimeoutMs) of
        {ok, Line} ->
            try
                Json = Dec(Line),
                case classify_jsonrpc(Json) of
                    {response, Id, Reply} -> {response, Id, Reply};
                    {notification, Method, Params} -> {notification, Method, Params};
                    {request, Id, Method, Params} -> {request, Id, Method, Params};
                    _ -> other
                end
            catch E:R:Stk -> {error, {decode_failed, E, R, Stk}}
            end;
        {error, _}=Err -> Err
    end.

-spec respond(client(), integer() | binary(), term()) -> ok.
respond(#client{}=C, Id, Result) ->
    Json = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"result">> => Result
    },
    Enc = C#client.json_encode,
    send_line(C#client.port, Enc(Json)),
    ok.

-spec notify(client(), binary(), term()) -> ok | {error, term()}.
notify(#client{}=C, Method, Params) when is_binary(Method) ->
    Json = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => Method,
        <<"params">> => Params
    },
    Enc = C#client.json_encode,
    Bin = Enc(Json),
    send_line(C#client.port, Bin),
    ok.

%% Internal: newline-delimited JSON framing

-spec send_line(port(), iodata()) -> ok.
send_line(Port, Body) ->
    port_command(Port, [Body, <<"\n">>]),
    ok.

-spec recv_line(port(), non_neg_integer()) -> {ok, binary()} | {error, term()}.
recv_line(Port, TimeoutMs) ->
    Buf0 = case erlang:get({mcpe_buf, Port}) of undefined -> <<>>; B -> B end,
    erlang:put({mcpe_buf, Port}, <<>>),
    recv_line(Port, Buf0, TimeoutMs).

recv_line(Port, Acc, TimeoutMs) ->
    case split_line(Acc) of
        {more, Acc1} ->
            receive
                {Port, {data, Bin}} -> recv_line(Port, <<Acc1/binary, Bin/binary>>, TimeoutMs);
                {Port, eof} -> {error, eof};
                {Port, {exit_status, Code}} -> {error, {exit_status, Code}}
            after TimeoutMs -> {error, timeout}
            end;
        {ok, Line, Rest} ->
            case Rest of
                <<>> -> ok;
                _ -> erlang:put({mcpe_buf, Port}, Rest)
            end,
            {ok, Line}
    end.

%% leftover from previous framing; intentionally removed

split_line(Bin) ->
    case binary:match(Bin, <<"\n">>) of
        nomatch -> {more, Bin};
        {Pos, 1} ->
            <<Line:Pos/binary, _Sep:1/binary, Rest/binary>> = Bin,
            {ok, Line, Rest}
    end.

recv_response_for_id(Port, Dec, ExpectId) ->
    case recv_line(Port, 30000) of
        {ok, Line} ->
            try
                Json = Dec(Line),
                case classify_jsonrpc(Json) of
                    {response, ExpectId, Reply} ->
                        case Reply of
                            {ok, Result} -> {ok, Result};
                            {error, Err} -> {error, Err}
                        end;
                    {response, _OtherId, _} ->
                        recv_response_for_id(Port, Dec, ExpectId);
                    _Other ->
                        %% request or notification or malformed: ignore
                        recv_response_for_id(Port, Dec, ExpectId)
                end
            catch E:R:Stk -> {error, {decode_failed, E, R, Stk}}
            end;
        {error, _}=Err -> Err
    end.

classify_jsonrpc(Map) when is_map(Map) ->
    Id = maps:get(<<"id">>, Map, undefined),
    case {maps:is_key(<<"result">>, Map), maps:is_key(<<"error">>, Map)} of
        {true, false} -> {response, Id, {ok, maps:get(<<"result">>, Map)}};
        {false, true} ->
            {response, Id,
             {error, case maps:get(<<"error">>, Map) of
                         #{<<"code">> := Code, <<"message">> := Msg} -> {jsonrpc2, Code, Msg};
                         Other -> {server_error, Other}
                     end}};
        _ ->
            case {Id, maps:get(<<"method">>, Map, undefined), maps:get(<<"params">>, Map, undefined)} of
                {undefined, MethodBin, Params} when is_binary(MethodBin) -> {notification, MethodBin, Params};
                {Id2, MethodBin, Params} when Id2 =/= undefined, is_binary(MethodBin) -> {request, Id2, MethodBin, Params};
                _ -> other
            end
    end;
classify_jsonrpc(_) -> other.
