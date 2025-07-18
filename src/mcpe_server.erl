-module(mcpe_server).

-export([start_link/4]).

-export_type([
    payload/0,
    method/0,
    state/0,
    args/0,
    opts/0,
    error_reason/0,
    handle_result/0
]).

%% Data exchanged in requests/responses (Erlang representation of JSON).
-type payload() :: jsone:json_value().

%% JSON-RPC method name.
-type method()  :: klsn:binstr().

%% Opaque user-maintained connection state.
-type state()   :: term().

%% Term supplied to init/1 when starting the connection.
-type args()    :: term().

%% Options for start_link/4 (extended later as needed).
-type opts()    :: #{path := klsn:binstr()}.

%% Error reason recognised by the framework and converted to a JSON-RPC error
%% object.
-type error_reason() ::
        parse_error | invalid_request | method_not_found | invalid_params |
        internal_error | server_error | capability_denied |
        {jsonrpc2, integer(), klsn:binstr()} |
        {jsonrpc2, integer(), klsn:binstr(), payload()}.

%% Return value of handle/3.
-type handle_result() :: {reply, payload(), state()} |
                        {noreply, state()}          |
                        {error, error_reason()}.

%% Behaviour callbacks --------------------------------------------------

-callback init(args()) -> {ok, state()}.

-callback handle(method(), mcpe_client:payload(), state()) -> handle_result().

%% Public API -----------------------------------------------------------

-spec start_link(inet:port_number(), module(), args(), opts()) ->
          {ok, pid()} | {error, term()}.
start_link(_Port, _CallbackModule, _Args, _Opts) ->
    todo.
