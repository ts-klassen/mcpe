-module(client).

-export([main/0]).

main() ->
    ExecPath = filename:join(code:priv_dir(client), <<"codex-x86_64-unknown-linux-musl">>),
    Args = [<<"mcp">>],
    % 1) list tools and print
    % 2) use codex tool with prompt <<"Say this is a test">>
    % 3) use codex-reply tool with prompt <<"Say it again">>
    ok.

