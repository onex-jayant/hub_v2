%%%-------------------------------------------------------------------
%% @doc db_write top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(db_write_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
  SupFlags =
    #{strategy  => one_for_all,
      intensity => 0,
      period    => 1},

  ChildSpecs =
  [
    #{id => db_writer,
      start => {db_writer, start_link, []}},
    #{id => db_dlr_writer,
      start => {db_dlr_writer, start_link, []}},
    #{id => table_creation,
      start => {table_creation, start_link, []}}
  ],

  {ok, {SupFlags, ChildSpecs}}.
