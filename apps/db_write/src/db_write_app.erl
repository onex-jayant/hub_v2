%%%-------------------------------------------------------------------
%% @doc db_write public API
%% @end
%%%-------------------------------------------------------------------

-module(db_write_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    db_write_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
