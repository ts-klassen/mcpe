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
            %% Initialise the callback module, catching only exits.
            InitRes =
                try CallbackModule:init(Args) of
                    {ok, CBState0} -> {ok, CBState0}
                catch
                    exit:R -> {error, {init_failed, R}}
                end,

            case InitRes of
                {error, _} = Err -> Err;
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
                        {ok, Pid} ->
                            {ok, Pid};

                        {error, _Reason} = Err ->
                            maybe_cleanup(CallbackModule, CBState),
                            Err
                    end
            end
    end.

%% Generate a unique listener name based on path + port so multiple test
%% suites can run concurrently.
-spec make_ref(binary(), inet:port_number()) -> atom().
make_ref(Path, Port) ->
    %% Add a unique component to avoid collisions when the function is called
    %% multiple times with the same Path/Port combination (for example when
    %% starting and stopping listeners rapidly in tests). We rely on
    %% erlang:unique_integer/1 which returns a monotonically increasing
    %% integer that is unique to the Erlang VM instance.
    Unique = erlang:unique_integer([monotonic, positive]),
    NameStr = lists:flatten([
        "mcpe_listener_",
        integer_to_list(Port),
        "_",
        integer_to_list(erlang:phash2(Path)),
        "_",
        integer_to_list(Unique)
    ]),
    list_to_atom(NameStr).

%%--------------------------------------------------------------------
%%  Helper functions
%%--------------------------------------------------------------------

%% Call CallbackModule:cleanup/1 if it exists, swallowing any exception.
-spec maybe_cleanup(module(), state()) -> ok.
maybe_cleanup(Module, CBState) ->
    case erlang:function_exported(Module, cleanup, 1) of
        true ->
            %% Ignore any failure inside the user-supplied cleanup callback.
            try Module:cleanup(CBState) of
                _ -> ok
            catch
                _:_ -> ok
            end;
        false -> ok
    end.
