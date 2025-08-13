-module(server).

%% This is a design-only sample demonstrating how a hypothetical
%% mcpe_server module could be used. It does NOT implement a server; it
%% models the intended API shape for your review.
%%
%% Goals covered by this sample:
%% - Show list of tools (via tools/list)
%% - Handle incoming notifications
%% - Send notifications (to a connection or broadcast)
%%
%% Proposed mcpe_server API (to be implemented later):
%% - mcpe_server:start_link(#{
%%       path := binary(),              %% UNIX socket path
%%       handler := {module(), term()}  %% {HandlerModule, InitArg}
%%   }) -> {ok, server()} | {error, term()}.
%% - mcpe_server:stop(server()) -> ok.
%% - mcpe_server:notify(conn(), binary(), term()) -> ok.          %% to a single connection
%% - mcpe_server:broadcast(server(), binary(), term()) -> ok.     %% to all connections
%%
%% Proposed handler callbacks invoked by mcpe_server:
%% - init(InitArg) -> {ok, State}.
%% - tools(State) -> [ToolSpec], where ToolSpec is a map:
%%     #{
%%       <<"name">> := binary(),
%%       <<"description">> := binary(),
%%       <<"inputSchema">> := map()
%%     }
%% - handle_request(MethodBin, Params, Conn, State) ->
%%       {reply, Result, NewState} |
%%       {reply_error, Code :: integer(), Message :: binary(), Data :: term(), NewState}.
%% - handle_notification(MethodBin, Params, Conn, State) -> {noreply, NewState}.
%% - terminate(Reason, State) -> ok.

-export([
    start/0,
    start/1,
    stop/1,
    announce/2
]).

-export([
    init/1,
    tools/1,
    handle_request/4,
    handle_notification/4,
    terminate/2
]).

start() ->
    start(<<"/tmp/mcpe_sample_server">>).

%% Not a callback: public API for starting/stopping the sample server using mcpe_server
-spec start(binary() | list()) -> {ok, mcpe_server:server()} | {error, term()}.
start(Path) ->
    BinPath = iolist_to_binary(Path),
    %% Start a UNIX-socket server at BinPath, using this module as handler
    mcpe_server:start_link(#{
        path => BinPath,
        handler => {?MODULE, #{}}
    }).

-spec stop(mcpe_server:server()) -> ok.
stop(Server) -> mcpe_server:stop(Server).

%% Not a callback: broadcast a server-side notification to all connected clients
-spec announce(mcpe_server:server(), iodata()) -> ok.
announce(Server, Text) ->
    mcpe_server:broadcast(Server, <<"notifications/announcement">>,
                          #{<<"text">> => iolist_to_binary(Text)}).

%% Callback: mcpe_server handler init/1
-spec init(term()) -> {ok, map()}.
init(_InitArg) ->
    io:format("init~n"),
    {ok, #{}}.

%% Callback: mcpe_server handler tools/1
-spec tools(map()) -> [map()].
tools(_State) ->
    io:format("tools~n"),
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

-spec handle_request(binary(), term(), mcpe_server:conn(), map()) ->
          {reply, term(), map()} |
          {reply_error, integer(), binary(), term(), map()}.
%% Callback: mcpe_server handler handle_request/4
handle_request(<<"initialize">>, _Params, Conn, State) ->
    io:format("handle_request.initialize~n"),
    %% Example: send a ready notification in response to initialize
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
    io:format("handle_request.tools/list~n"),
    {reply, #{<<"tools">> => tools(State)}, State};
handle_request(<<"tools/call">>, Params=#{<<"name">> := <<"echo">>, <<"arguments">> := Args}, _Conn, State) ->
    io:format("handle_request.tools/call(~p)~n", [Params]),
    Text = maps:get(<<"text">>, Args, <<>>),
    {reply, #{<<"content">> => [#{<<"type">> => <<"text">>, <<"text">> => Text}]}, State};
handle_request(<<"echo">>, Params, _Conn, State) ->
    io:format("handle_request.echo(~p)~n", [Params]),
    {reply, Params, State};
handle_request(<<"ping">>, _Params, _Conn, State) ->
    io:format("handle_request.ping(~p)~n", [_Params]),
    {reply, <<"pong">>, State};
handle_request(_Other, _Params, _Conn, State) ->
    io:format("handle_request.~p(~p, ~p, ~p)~n", [_Other, _Params, _Conn, State]),
    {reply_error, -32601, <<"Method not found">>, null, State}.

-spec handle_notification(binary(), term(), mcpe_server:conn(), map()) -> {noreply, map()}.
%% Callback: mcpe_server handler handle_notification/4
handle_notification(<<"notifications/initialized">>, _Params, Conn, State) ->
    io:format("handle_notification.notifications/initialized~n"),
    _ = mcpe_server:notify(Conn, <<"notifications/hello">>, #{<<"msg">> => <<"hello from server">>}),
    {noreply, State};
handle_notification(_Other, _Params, _Conn, State) ->
    {noreply, State}.

-spec terminate(term(), map()) -> ok.
%% Callback: mcpe_server handler terminate/2
terminate(_Reason, _State) -> ok.

%% Types come from mcpe_server and are referenced above using remote types.
