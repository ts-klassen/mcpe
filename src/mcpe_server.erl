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
start_link(Port, CallbackModule, Args, Opts) ->
    %% Ensure the callback module is loaded so function_exported works.
    _ = code:ensure_loaded(CallbackModule),
    %% Validate and initialise the callback implementation.
    case erlang:function_exported(CallbackModule, init, 1) andalso
         erlang:function_exported(CallbackModule, handle, 3) of
        false ->
            {error, {invalid_callback_module, CallbackModule}};
        true ->
            case catch CallbackModule:init(Args) of
                {'EXIT', Reason} -> {error, {init_failed, Reason}};
                {ok, CBState} ->
                    PathBin = maps:get(path, Opts, <<"/">>),
                    PathStr = binary_to_list(PathBin),

                    Dispatch = cowboy_router:compile([
                        {'_', [], [{PathStr, mcpe_http_handler,
                                   #{callback_module => CallbackModule,
                                     callback_state => CBState}}]}
                    ]),

                    %% Ensure cowboy application is running.
                    application:ensure_all_started(cowboy),

                    case cowboy:start_clear(make_ref(PathBin, Port),
                                             [{port, Port}],
                                             #{env => #{dispatch => Dispatch}}) of
                        {ok, Pid} -> {ok, Pid};
                        {error, Reason} = Err -> Err
                    end
            end
    end.

%% Generate a unique listener name based on path + port so multiple test
%% suites can run concurrently.
-spec make_ref(binary(), inet:port_number()) -> atom().
make_ref(Path, Port) ->
    NameStr = lists:flatten(["mcpe_listener_", integer_to_list(Port), "_",
                             integer_to_list(erlang:phash2(Path))]),
    list_to_atom(NameStr).
