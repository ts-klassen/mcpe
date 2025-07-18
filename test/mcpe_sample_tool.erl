%%%-------------------------------------------------------------------
%%% @doc A trivial tool implementation used by the Common Test suite.  Returns
%%%       the atom <<"done">> when executed.
%%%-------------------------------------------------------------------

-module(mcpe_sample_tool).

-behaviour(mcpe_tool).

-export([descriptor/0, execute/2]).

-include_lib("kernel/include/logger.hrl").

%%--------------------------------------------------------------------
%% mcpe_tool callbacks
%%--------------------------------------------------------------------

descriptor() ->
    #{ name        => <<"sample">>,
       description => <<"A dummy tool for testing">>,
       parameters  => #{} }.

execute(_Args, _Ctx) ->
    {ok, <<"done">>}.
