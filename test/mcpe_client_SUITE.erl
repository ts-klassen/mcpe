-module(mcpe_client_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([suite/0, all/0, init_per_suite/1, end_per_suite/1]).
-export([tools_list_works/1, codex_call_works/1, codex_reply_works/1]).

all() -> [tools_list_works, codex_call_works, codex_reply_works].

suite() ->
    %% Global guard to prevent long-running tests
    [{timetrap, {seconds, 60}}].

init_per_suite(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    ok = ensure_codex_binary(DataDir),
    ExecPath = filename:join(DataDir, "codex"),
    [{exec_path, ExecPath} | Config].

end_per_suite(_Config) -> ok.

tools_list_works(Config) ->
    ExecPath = proplists:get_value(exec_path, Config),
    Args = [<<"mcp">>],
    {ok, C} = mcpe_client:start(#{exec => ExecPath, args => Args}),
    try
        InitParams = #{
            <<"protocolVersion">> => <<"2024-11-05">>,
            <<"capabilities">> => #{},
            <<"clientInfo">> => #{
                <<"name">> => <<"mcpe-ct">>,
                <<"version">> => <<"0.0.1">>
            }
        },
        {ok, _} = mcpe_client:initialize(C, InitParams),
        ok = mcpe_client:notify(C, <<"notifications/initialized">>, null),
        case mcpe_client:request(C, <<"tools/list">>, #{}) of
            {ok, _Any} -> ok;
            {error, Err} -> ct:fail({tools_list_error, Err})
        end
    after
        ok = mcpe_client:stop(C)
    end.

codex_call_works(Config) ->
    ExecPath = proplists:get_value(exec_path, Config),
    Args = [<<"mcp">>],
    {ok, C} = mcpe_client:start(#{exec => ExecPath, args => Args}),
    try
        InitParams = #{
            <<"protocolVersion">> => <<"2024-11-05">>,
            <<"capabilities">> => #{},
            <<"clientInfo">> => #{
                <<"name">> => <<"mcpe-ct">>,
                <<"version">> => <<"0.0.1">>
            }
        },
        {ok, _} = mcpe_client:initialize(C, InitParams),
        ok = mcpe_client:notify(C, <<"notifications/initialized">>, null),

        Cwd = list_to_binary(filename:absname(".")),
        Call = #{
            <<"name">> => <<"conversationCreate">>,
            <<"arguments">> => #{
                <<"prompt">> => <<"Say this is a test">>,
                <<"model">> => <<"o4-mini">>,
                <<"cwd">> => Cwd
            }
        },
        case mcpe_client:request(C, <<"tools/call">>, Call) of
            {ok, _Res} -> ok;
            {error, Err} -> ct:fail({codex_call_error, Err})
        end
    after
        ok = mcpe_client:stop(C)
    end.

codex_reply_works(Config) ->
    ExecPath = proplists:get_value(exec_path, Config),
    Args = [<<"mcp">>],
    {ok, C} = mcpe_client:start(#{exec => ExecPath, args => Args}),
    try
        InitParams = #{
            <<"protocolVersion">> => <<"2024-11-05">>,
            <<"capabilities">> => #{},
            <<"clientInfo">> => #{
                <<"name">> => <<"mcpe-ct">>,
                <<"version">> => <<"0.0.1">>
            }
        },
        {ok, _} = mcpe_client:initialize(C, InitParams),
        ok = mcpe_client:notify(C, <<"notifications/initialized">>, null),

        Cwd = list_to_binary(filename:absname(".")),
        Create = #{
            <<"name">> => <<"conversationCreate">>,
            <<"arguments">> => #{
                <<"prompt">> => <<"Say this is a test">>,
                <<"model">> => <<"o4-mini">>,
                <<"cwd">> => Cwd
            }
        },
        {ok, CreateRes} = mcpe_client:request(C, <<"tools/call">>, Create),
        SessId = extract_session_id(CreateRes),

        Reply = #{
            <<"name">> => <<"codex-reply">>,
            <<"arguments">> => #{
                <<"sessionId">> => SessId,
                <<"prompt">> => <<"Say it again but shorter.">>
            }
        },
        {ok, ReplyId} = mcpe_client:call_async(C, <<"tools/call">>, Reply),
        case wait_for_task_complete(C, ReplyId, 55000) of
            ok ->
                _ = mcpe_client:notify(C, <<"notifications/cancelled">>, #{<<"requestId">> => ReplyId}),
                _ = wait_for_quiet(C, 300, 1000),
                ok;
            {error, timeout} -> ct:fail({codex_reply_error, timeout});
            {error, E} -> ct:fail({codex_reply_error, E})
        end
    after
        ok = mcpe_client:stop(C)
    end.

extract_session_id(Res) ->
    %% Accept a few shapes from the server result
    case Res of
        #{<<"structuredContent">> := SC} -> from_sc(SC);
        #{<<"conversationId">> := Id} -> Id;
        #{<<"conversation_id">> := Id2} -> Id2;
        _ -> ct:fail({no_session_in_result, Res})
    end.

from_sc(#{<<"conversationId">> := Id}) -> Id;
from_sc(#{<<"conversation_id">> := Id}) -> Id;
from_sc(Other) -> ct:fail({no_session_in_structuredContent, Other}).

%% Wait for task completion notification for specific request id.
wait_for_task_complete(C, ReqId, TimeoutMs) ->
    wait_for_task_complete(C, ReqId, TimeoutMs, erlang:monotonic_time(millisecond)).

wait_for_task_complete(C, ReqId, TimeoutMs, Start) ->
    Now = erlang:monotonic_time(millisecond),
    Elapsed = Now - Start,
    case Elapsed >= TimeoutMs of
        true -> {error, timeout};
        false ->
            Remaining = TimeoutMs - Elapsed,
            case mcpe_client:recv(C, min(Remaining, 1000)) of
                {notification, <<"task_complete">>, Params} ->
                    case maps:get(<<"_meta">>, Params, #{}) of
                        #{<<"requestId">> := ReqId} -> ok;
                        _ -> wait_for_task_complete(C, ReqId, TimeoutMs, Start)
                    end;
                {notification, <<"notifications/task_complete">>, Params} ->
                    case maps:get(<<"_meta">>, Params, #{}) of
                        #{<<"requestId">> := ReqId} -> ok;
                        _ -> wait_for_task_complete(C, ReqId, TimeoutMs, Start)
                    end;
                {notification, <<"codex/event">>, Params} ->
                    Msg = maps:get(<<"msg">>, Params, #{}),
                    Type = maps:get(<<"type">> , Msg, <<>>),
                    case {Type, maps:get(<<"_meta">>, Params, #{})} of
                        {<<"task_complete">>, #{<<"requestId">> := ReqId}} -> ok;
                        _ -> wait_for_task_complete(C, ReqId, TimeoutMs, Start)
                    end;
                %% Accept generic progress notifications and keep waiting
                {notification, _Method, _Params} ->
                    wait_for_task_complete(C, ReqId, TimeoutMs, Start);
                {request, RId, <<"elicitation/create">>, _Params} ->
                    _ = mcpe_client:respond(C, RId, #{<<"decision">> => <<"denied">>}),
                    wait_for_task_complete(C, ReqId, TimeoutMs, Start);
                {response, ReqId, {ok, _}} -> ok;
                {response, ReqId, {error, Err}} -> {error, Err};
                {response, _Id, _Reply} ->
                    wait_for_task_complete(C, ReqId, TimeoutMs, Start);
                other ->
                    wait_for_task_complete(C, ReqId, TimeoutMs, Start);
                {error, timeout} ->
                    wait_for_task_complete(C, ReqId, TimeoutMs, Start);
                {error, E} -> {error, E}
            end
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
                        {notification, _Method, _Params} ->
                            wait_for_quiet(C, QuietMs, MaxMs, Start, erlang:monotonic_time(millisecond));
                        {response, _Id, _Reply} ->
                            wait_for_quiet(C, QuietMs, MaxMs, Start, erlang:monotonic_time(millisecond));
                        {request, RId, <<"elicitation/create">>, _Params} ->
                            _ = mcpe_client:respond(C, RId, #{<<"decision">> => <<"denied">>}),
                            wait_for_quiet(C, QuietMs, MaxMs, Start, erlang:monotonic_time(millisecond));
                        other ->
                            wait_for_quiet(C, QuietMs, MaxMs, Start, erlang:monotonic_time(millisecond));
                        {error, timeout} ->
                            wait_for_quiet(C, QuietMs, MaxMs, Start, LastMsgTs);
                        {error, _} ->
                            wait_for_quiet(C, QuietMs, MaxMs, Start, erlang:monotonic_time(millisecond))
                    end
            end
    end.

%% Ensure the Codex binary is available in the CT data dir.
ensure_codex_binary(DataDir) when is_list(DataDir) ->
    ExecPath = filename:join(DataDir, "codex"),
    case filelib:is_file(ExecPath) of
        true -> ok;
        false ->
            filelib:ensure_dir(ExecPath),
            Dir = filename:dirname(ExecPath),
            BinName = "codex-x86_64-unknown-linux-musl",
            case os:getenv("CODEX_BIN") of
                false -> ensure_codex_via_download(Dir, BinName, ExecPath);
                EnvPath ->
                    case filelib:is_file(EnvPath) of
                        true -> copy_and_make_executable(EnvPath, ExecPath);
                        false -> ensure_codex_via_download(Dir, BinName, ExecPath)
                    end
            end
    end.

ensure_codex_via_download(Dir, BinName, ExecPath) ->
    case download_and_prepare_codex(Dir, BinName) of
        ok -> copy_and_make_executable(filename:join(Dir, BinName), ExecPath);
        {error, Reason} -> ct:fail({enoent_codex_binary, Reason})
    end.

copy_and_make_executable(Src, Dst) ->
    case file:copy(Src, Dst) of
        {ok, _Bytes} ->
            ok = file:change_mode(Dst, 8#755),
            ok;
        {error, Reason} ->
            ct:fail({copy_failed, Src, Dst, Reason})
    end.

extract_tar_to_dir(TarPath, DestDir) ->
    case erl_tar:extract(TarPath, [{cwd, DestDir}, compressed]) of
        ok -> ok;
        {error, _}=Err -> Err
    end.

download_and_prepare_codex(DestDir, BinName) ->
    TarPath = filename:join(DestDir, "codex.tar.gz"),
    Url = "https://github.com/openai/codex/releases/download/rust-v0.20.0/codex-x86_64-unknown-linux-musl.tar.gz",
    case ensure_http_started() of
        ok ->
            HTTPOpts = [{autoredirect, true}, {timeout, 30000}, {ssl, [{verify, verify_none}]}],
            Opts = [{body_format, binary}],
            case httpc:request(get, {Url, []}, HTTPOpts, Opts) of
                {ok, {{_Vsn, 200, _Reason}, _Headers, Body}} ->
                    ok = filelib:ensure_dir(TarPath),
                    ok = file:write_file(TarPath, Body),
                    case extract_tar_to_dir(TarPath, DestDir) of
                        ok ->
                            _ = file:delete(TarPath),
                            case filelib:is_file(filename:join(DestDir, BinName)) of
                                true -> ok;
                                false -> {error, not_extracted}
                            end;
                        {error, Reason} -> {error, {extract_failed, Reason}}
                    end;
                {ok, {{_Vsn, Code, _}, _Headers, _}} -> {error, {bad_status, Code}};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason0} -> {error, {http_start_failed, Reason0}}
    end.

ensure_http_started() ->
    _ = application:ensure_all_started(ssl),
    application:ensure_all_started(inets),
    ok.

