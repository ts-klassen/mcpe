%%%-------------------------------------------------------------------
%%% @doc A trivial tool implementation used by the Common Test suite.  Returns
%%%       the binary string <<"done">> when executed. If referring to the atom
%%%       with the same textual value, use 'done' or done.
%%%-------------------------------------------------------------------

-module(mcpe_sample_tool).

-behaviour(mcpe_tool).

-export([descriptor/0, execute/2]).


%%--------------------------------------------------------------------
%% mcpe_tool callbacks
%%--------------------------------------------------------------------

descriptor() ->
    #{ name        => <<"sample">>,
       description => <<"A dummy tool for testing">>,
       parameters  => #{} }.

execute(_Args, _Ctx) ->
    {ok, <<"done">>}.
