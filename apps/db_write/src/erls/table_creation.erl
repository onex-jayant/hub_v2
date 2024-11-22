-module(table_creation).
% table_creation:start_link().
-behaviour(gen_server).

-export(
  [
    stop/0,
    start_link/0
  ]).

%% gen_server callbacks
-export(
  [
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
  ]).

start_link() ->
  gen_server:start_link(?MODULE, [], []).

stop()->
  gen_server:stop(?MODULE).

init([]) ->
  erlang:send_after(5000, self(), create_smsrec_table),
  {ok, []}.

handle_call(stop, _From, State)     ->
  {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(create_smsrec_table,State) ->
  manage_smsrec_table(),
  WaitTime  = wait_time(),
  erlang:send_after(WaitTime, self(), create_smsrec_table),
  {noreply, State};

handle_info(_Info, State)              ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

%%####################################### Table creation for smsrec db ##################################################

manage_smsrec_table()->
  lists:foreach(fun(Date)->
    create_smsrec_table(Date)
  end, get_formatted_dates()).

create_smsrec_table(Date)->
  FinalQuery = get_formatted_tables_query(Date),
  case pgpool:squery(dbwrite, FinalQuery) of
    {error, Reason} ->
      logger:error("Error in creating smsrec tables for date: ~p, reason : ~p~n", [Date, Reason]);
    _Response       -> ok
  end.

wait_time() ->
  CurrentTime = calendar:local_time(),
  TargetTime  = {edate:today(), {2, 0, 0}},
  Difference  = calculate_difference(CurrentTime, TargetTime),
  case Difference > 0 of
    true  -> Difference;
    false -> calculate_difference(CurrentTime, {edate:tomorrow(), {2, 0, 0}})
  end.

calculate_difference(CurrentTime, TargetTime) ->
  CurrentSecs = calendar:datetime_to_gregorian_seconds(CurrentTime),
  TargetSecs  = calendar:datetime_to_gregorian_seconds(TargetTime),
  (TargetSecs - CurrentSecs)*1000.

get_formatted_dates() ->
  {Today, Tomorrow, DayAfterTomorrow} = get_dates(),
  [format_date(Today), format_date(Tomorrow), format_date(DayAfterTomorrow)].

get_dates() ->
  Today            = edate:today(),
  Tomorrow         = edate:tomorrow(),
  DayAfterTomorrow = edate:shift(Tomorrow, 1, day),
  {Today, Tomorrow, DayAfterTomorrow}.

format_date({Year, Month, Day}) -> io_lib:format("~2.10.0B~2.10.0B~4.10.0B", [Day, Month, Year]).

get_formatted_tables_query(Date) ->
  lists:concat(pg_queries:get_create_table_query(Date)).
