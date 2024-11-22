-module(db_dlr_writer).
% db_dlr_writer:start_link().
-behaviour(gen_server).

-define(SLEEP_TIME, 1). %% sleep time to read records
-define(REAP_SIZE, 1000).   % record size to insert data into db
-define(RANDOM_TIME,200).

-export([stop/0, start_link/0,init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([query_formation/1]).

-record(state, {db}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [],[]).

stop()->
  gen_server:stop(?MODULE).

init([]) ->
  rand:seed(exsss),
  process_flag(trap_exit, true),
  io:format("Started db_dlr_writer ~n"),
  erlang:send_after(?SLEEP_TIME, self(), insert_to_db),
  {ok, #state{db = db_handler:get_db_connection()}}.

handle_call(stop, _From, State) ->
  {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(insert_to_db,State) ->
  SleepTime = 10 + rand:uniform(40),
  timer:sleep(SleepTime),
  case dlr_queue:out(?REAP_SIZE) of
   []         -> ok;
   {error,_}  -> ok;
   Recs       ->
    DB = State#state.db,
    give_records_pg_writer(DB, Recs)
  end,
  erlang:send_after(?SLEEP_TIME, self(), insert_to_db),
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

give_records_pg_writer(DB,Recs)->
  case catch insert_batch(DB,Recs) of
    inserted -> inserted;
    Resp     -> io:format("Response insertion ~p~n",[Resp]),
                failed_insertion
  end.

insert_batch(DB, Recs)->
  T1         = erlang:system_time(millisecond),
  DlrQueries = query_formation(Recs),
  T2         = erlang:system_time(millisecond),
  _Resp      = util:execute_queries(DB, DlrQueries),
  T3         = erlang:system_time(millisecond),
  io:format("~n___________ DB-DLR writer Recs Count : ~p Query Form Time: ~p  Query Exec Time: ~p _________~n",[length(Recs),T2-T1,T3-T2]),
  inserted.

query_formation(SmsRecList)->
  ListOfDlrValues =
    lists:foldl(fun(#{payload := {SmsState, SmsRec}}, DlrAcc)->
      case (SmsState == dlvr) of
        true  ->
          {Date, DlrValues} = insertion_query:create_insertion_query({SmsState, SmsRec}),
          [{Date, DlrValues} | DlrAcc];
        false -> DlrAcc
      end
    end, [], SmsRecList),
  util:create_queries(ListOfDlrValues, smsrec_dlr).
