-module(mcpe_http_handler).

%% Cowboy handler used by mcpe_server to bridge HTTP requests to the jsonrpc2
%% library and the user-supplied mcpe_server callback module.

-behaviour(cowboy_handler).

-export([init/2]).

%%--------------------------------------------------------------------
%% Types
%%--------------------------------------------------------------------

-type state() :: #{callback_module := module(), callback_state := term()}.

%%--------------------------------------------------------------------
%% cowboy_handler callback
%%--------------------------------------------------------------------

-spec init(cowboy_req:req(), state()) -> {ok, cowboy_req:req(), state()}.
init(Req0, State = #{callback_module := Mod, callback_state := CBState}) ->
    case cowboy_req:method(Req0) of
        <<"POST">> ->
            handle_post(Req0, State, Mod, CBState);
        _Other ->
            Req = cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0),
            {ok, Req, State}
    end.

handle_post(Req0, State, Mod, CBState) ->
    %% Enforce a reasonable maximum body size (1 MiB) to avoid
    %% memory-exhaustion attacks. Cowboy returns `{more, _, Req}` when the
    %% limit is exceeded; we convert that into a 413 response.

    MaxBodySize = 1048576, %% 1 MiB

    case cowboy_req:read_body(Req0, #{length => MaxBodySize}) of
        {ok, Body, Req1} ->

            %% The user callback may return an updated state that needs to be kept
            %% around for subsequent JSON-RPC calls that are part of the *same* HTTP
            %% request **and** for future requests handled by the same Cowboy routing
            %% rule.

            %% We keep the most recent callback state inside the process dictionary
            %% while handling the current request. This allows us to update the state
            %% when a batch request is processed (jsonrpc2 may invoke the handler
            %% function multiple times).

            put(mcpe_callback_state, CBState),

            HandlerFun = fun(Method, Params) ->
                %% Retrieve the latest state for every individual JSON-RPC call so
                %% that batch requests see updates performed by previous calls.
                CurrentCBState = get(mcpe_callback_state),

                case Mod:handle(Method, Params, CurrentCBState) of
                    {reply, Result, NewState} ->
                        put(mcpe_callback_state, NewState),
                        Result;
                    {noreply, NewState} ->
                        put(mcpe_callback_state, NewState),
                        ok;
                    {error, Err} ->
                        throw_error(Err)
                end
            end,

            case jsonrpc2:handle(Body, HandlerFun, fun jsone:decode/1, fun jsone:encode/1) of
                noreply ->
                    Req2 = cowboy_req:reply(204, #{}, <<>>, Req1),
                    Tmp = erase(mcpe_callback_state),
                    FinalState = case Tmp of undefined -> CBState; _ -> Tmp end,
                    {ok, Req2, State#{callback_state := FinalState}};
                {reply, Bin} when is_binary(Bin) ->
                    Headers = #{ <<"content-type">> => <<"application/json">> },
                    Req2 = cowboy_req:reply(200, Headers, Bin, Req1),
                    Tmp = erase(mcpe_callback_state),
                    FinalState = case Tmp of undefined -> CBState; _ -> Tmp end,
                    {ok, Req2, State#{callback_state := FinalState}}
            end;

        {more, _Bin, Req1} ->
            %% Body exceeded the maximum allowed size – return 413.
            Req2 = cowboy_req:reply(413, #{}, <<"Payload Too Large">>, Req1),
            {ok, Req2, State}
    end.

%%--------------------------------------------------------------------
%% Internal helpers
%%--------------------------------------------------------------------

throw_error(parse_error)        -> throw(parse_error);
throw_error(invalid_request)    -> throw(invalid_request);
throw_error(method_not_found)   -> throw(method_not_found);
throw_error(invalid_params)     -> throw(invalid_params);
throw_error(internal_error)     -> throw(internal_error);
throw_error(server_error)       -> throw(server_error);
throw_error(capability_denied)  -> throw({jsonrpc2, -32001, <<"Capability denied">>});
throw_error({jsonrpc2, _Code, _Msg} = Term) -> throw(Term);
throw_error({jsonrpc2, _Code, _Msg, _Data} = Term) -> throw(Term);
%%
%% Generic catch-all that turns an arbitrary error coming from the user
%% callback into a JSON-RPC error tuple.  We used to ignore the original
%% error term and always returned a static <<"Server error">> message which
%% made debugging difficult because the useful details were hidden in the
%% server logs only.  We now embed a textual representation of the original
%% error in the error *message* and also keep the raw term in the *data*
%% field so that clients – and developers looking at traces – can access the
%% necessary information.
%%
throw_error(Other) ->
    %% Convert the original term to a binary to include it in the message.
    MsgBin = iolist_to_binary([<<"Server error: ">>, io_lib:format("~p", [Other])]),
    throw({jsonrpc2, -32000, MsgBin, Other}).
