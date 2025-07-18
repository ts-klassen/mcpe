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
    %% Read entire request body (assuming small JSON payloads).
    {ok, Body, Req1} = cowboy_req:read_body(Req0),

    HandlerFun = fun(Method, Params) ->
        case Mod:handle(Method, Params, CBState) of
            {reply, Result, _NewState} ->
                Result;
            {noreply, _NewState} ->
                ok;
            {error, Err} ->
                throw_error(Err)
        end
    end,

    case jsonrpc2:handle(Body, HandlerFun, fun jsone:decode/1, fun jsone:encode/1) of
        noreply ->
            Req2 = cowboy_req:reply(204, #{}, <<>>, Req1),
            {ok, Req2, State};
        {reply, Bin} when is_binary(Bin) ->
            Headers = #{ <<"content-type">> => <<"application/json">> },
            Req2 = cowboy_req:reply(200, Headers, Bin, Req1),
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
throw_error(Other) -> throw({jsonrpc2, -32000, <<"Server error">>, Other}).
