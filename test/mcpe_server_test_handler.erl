-module(mcpe_server_test_handler).
-export([init/1, tools/1, handle_request/4, handle_notification/4, terminate/2]).

%% Test handler module used by mcpe_server_SUITE only.

init(_Arg) -> {ok, #{}}.

tools(_State) ->
    EchoSchema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"text">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Text to echo">>}
        },
        <<"required">> => [<<"text">>]
    },
    [#{
        <<"name">> => <<"echo">>,
        <<"description">> => <<"Echo back the provided text">>,
        <<"inputSchema">> => EchoSchema
      }].

handle_request(<<"initialize">>, _Params, Conn, State) ->
    _ = mcpe_server:notify(Conn, <<"notifications/ready">>, #{<<"ok">> => true}),
    {reply, #{
        <<"protocolVersion">> => <<"2024-11-05">>,
        <<"capabilities">> => #{},
        <<"serverInfo">> => #{
            <<"name">> => <<"erlang_echo">>,
            <<"version">> => <<"0.1.0">>
        }
    }, State};
handle_request(<<"tools/list">>, _Params, _Conn, State) ->
    {reply, #{<<"tools">> => tools(State)}, State};
handle_request(<<"tools/call">>, #{<<"name">> := <<"echo">>, <<"arguments">> := Args}, _Conn, State) ->
    Text = maps:get(<<"text">>, Args, <<>>),
    {reply, #{<<"content">> => [#{<<"type">> => <<"text">>, <<"text">> => Text}]}, State};
handle_request(_Other, _Params, _Conn, State) ->
    {reply_error, -32601, <<"Method not found">>, null, State}.

handle_notification(<<"notifications/initialized">>, _Params, Conn, State) ->
    _ = mcpe_server:notify(Conn, <<"notifications/ready">>, #{<<"ok">> => true}),
    {noreply, State};
handle_notification(_Other, _Params, _Conn, State) ->
    {noreply, State}.

terminate(_Reason, _State) -> ok.
