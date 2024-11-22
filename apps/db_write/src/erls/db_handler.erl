-module(db_handler).
-include("include/onex.hrl").

-export([
  get_db_connection/0,
  execute/3
  ]).

%% need to decide which pool to use
get_db_connection()->
  case ?DBTYPE of
    oracle -> oracle_connection;
    _      -> dbwrite
  end.


execute(DBConn, Query, Date) ->
  case pgpool:squery(DBConn, Query) of
    {ok, _}                                       -> ok;
    {error, {error, _, _, undefined_table, _, _}} ->
      pg_queries:create_tables(DBConn, Date),
      pgpool:squery(DBConn, Query);
    {error, _}                                    -> error
  end.
