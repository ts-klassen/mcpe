-module(client).

-export([main/0]).

main() ->
    ExecPath = filename:join(code:priv_dir(client), <<"codex">>),
    Args = [<<"mcp">>],
    {ok, C} = mcpe_client:start(#{exec => ExecPath, args => Args}),
    try
        %% 0) initialize
        InitParams = #{
            <<"protocolVersion">> => <<"2024-11-05">>,
            <<"capabilities">> => #{},
            <<"clientInfo">> => #{
                <<"name">> => <<"mcpe-sample-client">>,
                <<"version">> => <<"0.1.0">>
            }
        },
        _ = mcpe_client:initialize(C, InitParams),
        ok = mcpe_client:notify(C, <<"notifications/initialized">>, null),

        %% 1) list tools and print
        case mcpe_client:request(C, <<"tools/list">>, #{}) of
            {ok, ToolList} ->
                io:format("Tools: ~p~n", [ToolList]);
            {error, E1} ->
                io:format("tools/list error: ~p~n", [E1])
        end,

        %% 2) structured: create a conversation (async) and get conversationId from response
        Cwd = list_to_binary(filename:absname(".")),
        CodexArgs = #{
            <<"name">> => <<"conversationCreate">>,
            <<"arguments">> => #{
                <<"prompt">> => <<"Say this is a test">>,
                <<"model">> => <<"o4-mini">>,
                <<"cwd">> => Cwd
            }
        },
        {ok, Id1} = mcpe_client:call_async(C, <<"tools/call">>, CodexArgs),
        SessIdRes = receive_session_id(C, 60000, Id1),

        %% 3) if we have a sessionId, use codex-reply tool
        case SessIdRes of
            {session, SessId} ->
                %% First message: "Say this is a test"
                Content1 = [#{
                    <<"type">> => <<"text">>,
                    <<"text">> => <<"Say this is a test">>
                }],
                Send1 = #{
                    <<"name">> => <<"conversationSendMessage">>,
                    <<"arguments">> => #{
                        <<"conversation_id">> => SessId,
                        <<"content">> => Content1
                    }
                },
                {ok, MsgId1} = mcpe_client:call_async(C, <<"tools/call">>, Send1),
                wait_for_reply(C, MsgId1, 60000),
                wait_for_task_complete(C, Id1, 60000),

                %% Second message using codex-reply to continue the same session
                CodexReplyArgs = #{
                    <<"name">> => <<"codex-reply">>,
                    <<"arguments">> => #{
                        <<"sessionId">> => SessId,
                        <<"prompt">> => <<"Say it again but in reverse.">>
                    }
                },
                {ok, ReplyId} = mcpe_client:call_async(C, <<"tools/call">>, CodexReplyArgs),
                %% For codex-reply, wait specifically for completion on ReplyId to avoid
                %% accidentally matching the earlier turn and interrupting the reply.
                wait_for_task_complete(C, ReplyId, 60000),

                %% Politely cancel requests to stop background streaming
                ok = mcpe_client:notify(C, <<"notifications/cancelled">>, #{<<"requestId">> => Id1}),
                ok = mcpe_client:notify(C, <<"notifications/cancelled">>, #{<<"requestId">> => MsgId1});
            no_session ->
                io:format("[INFO] no session_configured; skipping codex-reply\n", []),
                %% Cancel create request to stop background streaming if any
                ok = mcpe_client:notify(C, <<"notifications/cancelled">>, #{<<"requestId">> => Id1})
        end,
        ok
    after
        %% Wait for a short quiet period so the server can finish flushing
        %% stdout and stop background streaming before we close the port.
        wait_for_quiet(C, 300, 2000),
        ok = mcpe_client:stop(C)
    end.

receive_session_id(C, TimeoutMs, ReqId) ->
    receive_session_id(C, TimeoutMs, erlang:monotonic_time(millisecond), ReqId).

receive_session_id(C, TimeoutMs, Start, ReqId) ->
    Now = erlang:monotonic_time(millisecond),
    Elapsed = Now - Start,
    case Elapsed >= TimeoutMs of
        true ->
            io:format("[timeout waiting session_configured after ~p ms]~n", [Elapsed]),
            flush_messages(C),
            exit(timeout);
        false ->
            Remaining = TimeoutMs - Elapsed,
            case mcpe_client:recv(C, min(Remaining, 1000)) of
                {notification, <<"notifications/session_configured">>, Params} ->
                    io:format("[DBG] recv: session_configured ~p~n", [Params]),
                    Msg = maps:get(<<"msg">>, Params, #{}),
                    {session, maps:get(<<"session_id">>, Msg)};
                {request, ReqId, <<"elicitation/create">>, Params} ->
                    io:format("[DBG] recv: elicitation/create ~p~n", [Params]),
                    _ = mcpe_client:respond(C, ReqId, #{<<"decision">> => <<"denied">>}),
                    receive_session_id(C, TimeoutMs, Start, ReqId);
                {response, ReqId, {ok, Result}} ->
                    io:format("[DBG] recv: response ~p ok ~p~n", [ReqId, Result]),
                    case maps:get(<<"structuredContent">>, Result, undefined) of
                        #{<<"conversationId">> := ConvId} -> {session, ConvId};
                        #{<<"conversation_id">> := ConvId2} -> {session, ConvId2};
                        _ -> no_session
                    end;
                {response, ReqId, {error, Err}} ->
                    io:format("[DBG] recv: response ~p error ~p~n", [ReqId, Err]),
                    no_session;
                {response, Id, Reply} ->
                    io:format("[DBG] recv: response ~p ~p~n", [Id, Reply]),
                    receive_session_id(C, TimeoutMs, Start, ReqId);
                {notification, Method, Params} ->
                    io:format("[DBG] recv: notification ~p ~p~n", [Method, Params]),
                    receive_session_id(C, TimeoutMs, Start, ReqId);
                other ->
                    io:format("[DBG] recv: other~n", []),
                    receive_session_id(C, TimeoutMs, Start, ReqId);
                {error, timeout} ->
                    receive_session_id(C, TimeoutMs, Start, ReqId);
                {error, E} ->
                    io:format("[DBG] recv: error ~p~n", [E]),
                    receive_session_id(C, TimeoutMs, Start, ReqId)
            end
    end.

wait_for_reply(C, ExpectId, TimeoutMs) ->
    wait_for_reply(C, ExpectId, TimeoutMs, erlang:monotonic_time(millisecond)).

wait_for_reply(C, ExpectId, TimeoutMs, Start) ->
    Now = erlang:monotonic_time(millisecond),
    Elapsed = Now - Start,
    case Elapsed >= TimeoutMs of
        true ->
            io:format("[timeout waiting reply ~p after ~p ms]~n", [ExpectId, Elapsed]),
            flush_messages(C),
            exit(timeout);
        false ->
            Remaining = TimeoutMs - Elapsed,
            case mcpe_client:recv(C, min(Remaining, 1000)) of
                {response, ExpectId, {ok, Result}} ->
                    io:format("[DBG] recv: response ~p ok ~p~n", [ExpectId, Result]);
                {response, ExpectId, {error, Err}} ->
                    io:format("[DBG] recv: response ~p error ~p~n", [ExpectId, Err]);
                {response, OtherId, Reply} ->
                    io:format("[DBG] recv: response ~p ~p~n", [OtherId, Reply]),
                    wait_for_reply(C, ExpectId, TimeoutMs, Start);
                {request, ReqId, <<"elicitation/create">>, Params} ->
                    io:format("[DBG] recv: elicitation/create ~p~n", [Params]),
                    _ = mcpe_client:respond(C, ReqId, #{<<"decision">> => <<"denied">>}),
                    wait_for_reply(C, ExpectId, TimeoutMs, Start);
                {notification, Method, Params} ->
                    io:format("[DBG] recv: notification ~p ~p~n", [Method, Params]),
                    wait_for_reply(C, ExpectId, TimeoutMs, Start);
                other ->
                    io:format("[DBG] recv: other~n", []),
                    wait_for_reply(C, ExpectId, TimeoutMs, Start);
                {error, timeout} ->
                    wait_for_reply(C, ExpectId, TimeoutMs, Start);
                {error, E} ->
                    io:format("[DBG] recv: error ~p~n", [E]),
                    wait_for_reply(C, ExpectId, TimeoutMs, Start)
            end
    end.

flush_messages(C) ->
    case mcpe_client:recv(C, 10) of
        {response, Id, Reply} ->
            io:format("[FLUSH] response ~p ~p~n", [Id, Reply]),
            flush_messages(C);
        {request, Id, Method, Params} ->
            io:format("[FLUSH] request ~p ~p ~p~n", [Id, Method, Params]),
            %% Be conservative: deny/empty result for known interactive requests
            case Method of
                <<"elicitation/create">> -> _ = mcpe_client:respond(C, Id, #{<<"decision">> => <<"denied">>});
                _ -> ok
            end,
            flush_messages(C);
        {notification, Method, Params} ->
            io:format("[FLUSH] notification ~p ~p~n", [Method, Params]),
            flush_messages(C);
        other ->
            io:format("[FLUSH] other~n", []),
            flush_messages(C);
        {error, timeout} ->
            io:format("[FLUSH] done~n", []),
            ok;
        {error, E} ->
            io:format("[FLUSH] error ~p~n", [E]),
            ok
    end.

%% Wait until there has been no message for QuietMs, or MaxMs overall.
wait_for_quiet(C, QuietMs, MaxMs) ->
    Start = erlang:monotonic_time(millisecond),
    wait_for_quiet(C, QuietMs, MaxMs, Start, Start).

wait_for_quiet(C, QuietMs, MaxMs, Start, LastMsgTs) ->
    Now = erlang:monotonic_time(millisecond),
    case Now - Start >= MaxMs of
        true -> ok;
        false ->
            case Now - LastMsgTs >= QuietMs of
                true -> ok;
                false ->
                    Step = min(100, QuietMs - (Now - LastMsgTs)),
                    case mcpe_client:recv(C, Step) of
                        {notification, Method, Params} ->
                            io:format("[QUIET] notification ~p ~p~n", [Method, Params]),
                            wait_for_quiet(C, QuietMs, MaxMs, Start, erlang:monotonic_time(millisecond));
                        {response, Id, Reply} ->
                            io:format("[QUIET] response ~p ~p~n", [Id, Reply]),
                            wait_for_quiet(C, QuietMs, MaxMs, Start, erlang:monotonic_time(millisecond));
                        {request, ReqId, <<"elicitation/create">>, Params} ->
                            io:format("[QUIET] request ~p elicitation ~p~n", [ReqId, Params]),
                            _ = mcpe_client:respond(C, ReqId, #{<<"decision">> => <<"denied">>}),
                            wait_for_quiet(C, QuietMs, MaxMs, Start, erlang:monotonic_time(millisecond));
                        other ->
                            io:format("[QUIET] other~n", []),
                            wait_for_quiet(C, QuietMs, MaxMs, Start, erlang:monotonic_time(millisecond));
                        {error, timeout} ->
                            wait_for_quiet(C, QuietMs, MaxMs, Start, LastMsgTs);
                        {error, E} ->
                            io:format("[QUIET] error ~p~n", [E]),
                            wait_for_quiet(C, QuietMs, MaxMs, Start, erlang:monotonic_time(millisecond))
                    end
            end
    end.

%% Wait until we see a task_complete notification for the given request id,
%% or time out after TimeoutMs. If timed out, sleep 60s then flush everything.
wait_for_task_complete(C, ReqId, TimeoutMs) ->
    wait_for_task_complete(C, ReqId, TimeoutMs, erlang:monotonic_time(millisecond)).

wait_for_task_complete(C, ReqId, TimeoutMs, Start) ->
    Now = erlang:monotonic_time(millisecond),
    Elapsed = Now - Start,
    case Elapsed >= TimeoutMs of
        true ->
            io:format("[INFO] task_complete not observed; sleeping ~p ms then flushing~n", [60000]),
            timer:sleep(60000),
            flush_messages(C);
        false ->
            Remaining = TimeoutMs - Elapsed,
            case mcpe_client:recv(C, min(Remaining, 1000)) of
                {notification, <<"task_complete">>, Params} ->
                    case maps:get(<<"_meta">>, Params, #{}) of
                        #{<<"requestId">> := ReqId} ->
                            io:format("[DBG] task_complete for ~p~n", [ReqId]),
                            Msg = maps:get(<<"msg">>, Params, #{}),
                            Text = maps:get(<<"last_agent_message">>, Msg, <<>>),
                            case Text of
                                <<>> -> ok;
                                _ -> io:format("===============================================================~n~s~n===============================================================~n", [Text])
                            end;
                        _ ->
                            io:format("[DBG] task_complete (other id) ~p~n", [Params]),
                            wait_for_task_complete(C, ReqId, TimeoutMs, Start)
                    end;
                {notification, <<"notifications/task_complete">>, Params} ->
                    case maps:get(<<"_meta">>, Params, #{}) of
                        #{<<"requestId">> := ReqId} ->
                            io:format("[DBG] notifications/task_complete for ~p~n", [ReqId]),
                            Msg = maps:get(<<"msg">>, Params, #{}),
                            Text = maps:get(<<"last_agent_message">>, Msg, <<>>),
                            case Text of
                                <<>> -> ok;
                                _ -> io:format("===============================================================~n~s~n===============================================================~n", [Text])
                            end;
                        _ ->
                            io:format("[DBG] notifications/task_complete (other id) ~p~n", [Params]),
                            wait_for_task_complete(C, ReqId, TimeoutMs, Start)
                    end;
                {notification, <<"codex/event">>, Params} ->
                    Msg = maps:get(<<"msg">>, Params, #{}),
                    Type = maps:get(<<"type">>, Msg, <<>>),
                    case {Type, maps:get(<<"_meta">>, Params, #{})} of
                        {<<"task_complete">>, #{<<"requestId">> := ReqId}} ->
                            io:format("[DBG] codex/event task_complete for ~p~n", [ReqId]),
                            Text = maps:get(<<"last_agent_message">>, Msg, <<>>),
                            case Text of
                                <<>> -> ok;
                                _ -> io:format("===============================================================~n~s~n===============================================================~n", [Text])
                            end;
                        _ ->
                            io:format("[DBG] recv: notification ~p ~p~n", [<<"codex/event">>, Params]),
                            wait_for_task_complete(C, ReqId, TimeoutMs, Start)
                    end;
                {notification, Method, Params} ->
                    io:format("[DBG] recv: notification ~p ~p~n", [Method, Params]),
                    wait_for_task_complete(C, ReqId, TimeoutMs, Start);
                {response, Id, Reply} ->
                    io:format("[DBG] recv: response ~p ~p~n", [Id, Reply]),
                    wait_for_task_complete(C, ReqId, TimeoutMs, Start);
                {request, RId, <<"elicitation/create">>, Params} ->
                    io:format("[DBG] recv: elicitation/create ~p~n", [Params]),
                    _ = mcpe_client:respond(C, RId, #{<<"decision">> => <<"denied">>}),
                    wait_for_task_complete(C, ReqId, TimeoutMs, Start);
                other ->
                    io:format("[DBG] recv: other~n", []),
                    wait_for_task_complete(C, ReqId, TimeoutMs, Start);
                {error, timeout} ->
                    wait_for_task_complete(C, ReqId, TimeoutMs, Start);
                {error, E} ->
                    io:format("[DBG] recv: error ~p~n", [E]),
                    wait_for_task_complete(C, ReqId, TimeoutMs, Start)
            end
    end.

%% Variant that accepts completion for any of the provided request ids.
wait_for_task_complete_ids(C, ReqIds, TimeoutMs) when is_list(ReqIds) ->
    wait_for_task_complete_ids(C, ReqIds, TimeoutMs, erlang:monotonic_time(millisecond)).

wait_for_task_complete_ids(C, ReqIds, TimeoutMs, Start) ->
    Now = erlang:monotonic_time(millisecond),
    Elapsed = Now - Start,
    case Elapsed >= TimeoutMs of
        true ->
            io:format("[INFO] task_complete not observed; sleeping ~p ms then flushing~n", [60000]),
            timer:sleep(60000),
            flush_messages(C);
        false ->
            Remaining = TimeoutMs - Elapsed,
            case mcpe_client:recv(C, min(Remaining, 1000)) of
                {notification, <<"task_complete">>, Params} ->
                    case maps:get(<<"_meta">>, Params, #{}) of
                        #{<<"requestId">> := Rid} ->
                            case lists:member(Rid, ReqIds) of
                                true ->
                                    io:format("[DBG] task_complete for ~p~n", [Rid]),
                                    Msg = maps:get(<<"msg">>, Params, #{}),
                                    Text = maps:get(<<"last_agent_message">>, Msg, <<>>),
                                    case Text of
                                        <<>> -> ok;
                                        _ -> io:format("===============================================================~n~s~n===============================================================~n", [Text])
                                    end;
                                false ->
                                    io:format("[DBG] task_complete (other id) ~p~n", [Params]),
                                    wait_for_task_complete_ids(C, ReqIds, TimeoutMs, Start)
                            end;
                        _ ->
                            io:format("[DBG] task_complete (other id) ~p~n", [Params]),
                            wait_for_task_complete_ids(C, ReqIds, TimeoutMs, Start)
                    end;
                {notification, <<"notifications/task_complete">>, Params} ->
                    case maps:get(<<"_meta">>, Params, #{}) of
                        #{<<"requestId">> := Rid} ->
                            case lists:member(Rid, ReqIds) of
                                true ->
                                    io:format("[DBG] notifications/task_complete for ~p~n", [Rid]),
                                    Msg = maps:get(<<"msg">>, Params, #{}),
                                    Text = maps:get(<<"last_agent_message">>, Msg, <<>>),
                                    case Text of
                                        <<>> -> ok;
                                        _ -> io:format("===============================================================~n~s~n===============================================================~n", [Text])
                                    end;
                                false ->
                                    io:format("[DBG] notifications/task_complete (other id) ~p~n", [Params]),
                                    wait_for_task_complete_ids(C, ReqIds, TimeoutMs, Start)
                            end;
                        _ ->
                            io:format("[DBG] notifications/task_complete (other id) ~p~n", [Params]),
                            wait_for_task_complete_ids(C, ReqIds, TimeoutMs, Start)
                    end;
                {notification, <<"codex/event">>, Params} ->
                    Msg = maps:get(<<"msg">>, Params, #{}),
                    Type = maps:get(<<"type">>, Msg, <<>>),
                    case {Type, maps:get(<<"_meta">>, Params, #{})} of
                        {<<"task_complete">>, #{<<"requestId">> := Rid}} ->
                            case lists:member(Rid, ReqIds) of
                                true ->
                                    io:format("[DBG] codex/event task_complete for ~p~n", [Rid]),
                                    Text = maps:get(<<"last_agent_message">>, Msg, <<>>),
                                    case Text of
                                        <<>> -> ok;
                                        _ -> io:format("===============================================================~n~s~n===============================================================~n", [Text])
                                    end;
                                false ->
                                    io:format("[DBG] recv: notification ~p ~p~n", [<<"codex/event">>, Params]),
                                    wait_for_task_complete_ids(C, ReqIds, TimeoutMs, Start)
                            end;
                        _ ->
                            io:format("[DBG] recv: notification ~p ~p~n", [<<"codex/event">>, Params]),
                            wait_for_task_complete_ids(C, ReqIds, TimeoutMs, Start)
                    end;
                {notification, Method, Params} ->
                    io:format("[DBG] recv: notification ~p ~p~n", [Method, Params]),
                    wait_for_task_complete_ids(C, ReqIds, TimeoutMs, Start);
                {response, Id, Reply} ->
                    io:format("[DBG] recv: response ~p ~p~n", [Id, Reply]),
                    wait_for_task_complete_ids(C, ReqIds, TimeoutMs, Start);
                {request, RId, <<"elicitation/create">>, Params} ->
                    io:format("[DBG] recv: elicitation/create ~p~n", [Params]),
                    _ = mcpe_client:respond(C, RId, #{<<"decision">> => <<"denied">>}),
                    wait_for_task_complete_ids(C, ReqIds, TimeoutMs, Start);
                other ->
                    io:format("[DBG] recv: other~n", []),
                    wait_for_task_complete_ids(C, ReqIds, TimeoutMs, Start);
                {error, timeout} ->
                    wait_for_task_complete_ids(C, ReqIds, TimeoutMs, Start);
                {error, E} ->
                    io:format("[DBG] recv: error ~p~n", [E]),
                    wait_for_task_complete_ids(C, ReqIds, TimeoutMs, Start)
            end
    end.
