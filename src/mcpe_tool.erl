%%%-------------------------------------------------------------------
%%% @doc Behaviour for individual MCP tool modules.
%%%-------------------------------------------------------------------

-module(mcpe_tool).

%% This module exists solely to declare callback and type specifications.

-export_type([
    payload/0,
    descriptor/0,
    ctx/0
]).

%%--------------------------------------------------------------------
%% Types
%%--------------------------------------------------------------------

-type payload() :: mcpe_server:payload().

%% Descriptor advertised to clients via mcp.tools/list.
-type descriptor() :: #{name := klsn:binstr(),
                        description := klsn:binstr(),
                        parameters := payload()}.

%% Execution context passed back to the tool implementation.  Additional keys
%% may be added in the future – only the caller PID is guaranteed right now.
-type ctx() :: #{caller := pid()}.

%%--------------------------------------------------------------------
%% Behaviour callbacks
%%--------------------------------------------------------------------

-callback descriptor() -> descriptor().

-callback execute(payload(), ctx()) -> {ok, payload()} | {error, term()}.
