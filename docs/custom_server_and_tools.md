# Building Custom MCP Servers and Tools with the **mcpe** Library

This guide shows **application developers** how to plug their own logic into the
`mcpe` Erlang library by writing a **server module** (to expose MCP features)
and optional **tool modules** (to implement individual functions that the AI
model can call).  It deliberately stays away from the internals of `mcpe`
itself – you only need to implement a handful of straightforward callbacks.

---

## 1. Terminology

* **Server module** – the module that owns one JSON-RPC connection.  It decides
  which MCP features are available and handles every incoming request.
* **Tool module** – an optional helper module that encapsulates a single tool
  (function) you wish to expose to the AI model.

Both are regular Erlang modules that declare a behaviour so the compiler can
check the required callbacks.

---

## 2. Writing a Server Module (`mcpe_server` behaviour)

Add the `-behaviour(mcpe_server).` attribute and implement **two mandatory
callbacks** (`init/1` and `handle/3`).

```erlang
-module(my_mcp_server).
-behaviour(mcpe_server).

%%--------------------------------------------------------------------
%%  Required callback: init/1
%%--------------------------------------------------------------------

%% @doc  Initialise the connection-state for *one* client.
%%
-spec init(mcpe_server:args()) -> {ok, mcpe_server:state()}.
init(_Args) ->
    %% Decide which tool modules are available
    ToolModules = [time_tool, echo_tool],

    %% Build helper structures once so they can be reused quickly later
    Descriptors = [ Module:descriptor() || Module <- ToolModules ],
    ToolMap     = maps:from_list(
                    [{ Name, Module } ||
                       Module <- ToolModules,
                       #{ name := Name } = Module:descriptor() ]),

    {ok, #{ tools       => ToolMap,
            descriptors => Descriptors }}.

%%--------------------------------------------------------------------
%%  Required callback: handle/3
%%--------------------------------------------------------------------

%% @doc  Dispatch every JSON-RPC request.
-spec handle(mcpe_server:method(),
            mcpe_client:payload(),
            mcpe_server:state()) -> mcpe_server:handle_result().

handle(<<"mcp.tools/list">>, _Params, State=#{descriptors := Descs}) ->
    {reply, Descs, State};

handle(<<"mcp.tools/execute">>, #{ <<"name">> := Name,
                                     <<"args">> := Args },
       State=#{tools := ToolMap}) ->
    case maps:get(Name, ToolMap, undefined) of
        undefined ->
            {error, {jsonrpc2, -32601, <<"tool_not_found">>}};
        ToolModule ->
            %% Optional user-consent hook (see §4)
            case mcpe_util:maybe_security_check({tool, Name}, Args) of
                ok -> ToolModule:execute(Args, #{ caller => self() });
                {error, denied} ->
                    {error, {jsonrpc2, -32001, <<"capability_denied">>}}
            end
    end;

handle(_, _, _State) ->
    {error, method_not_found}.

```

### 2.1  Starting the server

```erlang
Port = 8080,
{ok, _Pid} = mcpe_server:start_link(Port, my_mcp_server, #{path => <<"/mcp">>}).
```

`mcpe_server` handles Cowboy, JSON parsing and response framing for you – only
your callbacks above run *your* code.

---

## 3. Writing a Tool Module (`mcpe_tool` behaviour – optional)

You can embed tool logic directly in `handle/3`, **or** separate each tool into
its own module for clarity and easy testing.  Such a module declares
`mcpe_tool` and provides two callbacks.

```erlang
-module(time_tool).
-behaviour(mcpe_tool).

-spec descriptor() -> mcpe_tool:descriptor().
descriptor() ->
    #{ name        => <<"time">>,
       description => <<"Return current UTC ISO-8601 timestamp">>,
       parameters  => #{}}.

%% Execute receives the JSON-RPC params as a payload() map and returns either
%% success or error.
-spec execute(mcpe_tool:payload(), mcpe_tool:ctx()) ->
          {ok, mcpe_tool:payload()} | {error, term()}.
execute(_Args, _Ctx) ->
    Now = calendar:system_time_to_rfc3339(erlang:system_time(second)),
    {ok, Now}.
```

Nothing else is required; the server module decides which tool modules it
exposes.

---

## 4. Adding an Optional Security / Consent Check

If you create a module named `mcpe_security` **and** export
`check/2` (`Op, Ctx -> ok | {error, denied}`) the library will call it before
executing a tool.  If the module or function is missing the action is allowed
by default.  Example stub:

```erlang
-module(mcpe_security).

check({tool, _Name}, _Ctx) ->
    %% Always allow; replace with real user-consent logic.
    ok.
```

---

## 5. Calling the Server from Erlang

`mcpe_client` provides a small convenience wrapper that performs real HTTP
requests.

```erlang
URL = <<"http://localhost:8080/mcp">>,

%% List available tools
{ok, ToolList} = mcpe_client:list_tools(URL, []),

%% Execute the "time" tool
{ok, Time} = mcpe_client:execute_tool(URL, <<"time">>, #{}, []).
```

You can use any HTTP client or even `curl` – the server simply speaks
JSON-RPC-over-HTTP POST.

---

## 6. Testing Your Modules

1. **Unit-test** each tool with eunit:
   ```erlang
   time_tool_test() ->
       {ok, Ts} = time_tool:execute(#{}, #{}),
       ?assert(is_binary(Ts)).
   ```

2. **Integration-test** the whole stack with common_test:
   start `mcpe_server` in `init_per_suite/1`, call the real HTTP endpoint from
   your test cases, then stop it in `end_per_suite/1`.

---

## 7. What’s Next?

As the `mcpe` library gains support for other MCP features (Resources, Prompts,
Roots, Sampling, Elicitation, Utilities) you simply extend `handle/3` with new
method clauses **or** delegate to additional helper modules – the pattern is
exactly the same.

Happy coding!
