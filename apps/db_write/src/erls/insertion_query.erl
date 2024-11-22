-module(insertion_query).

-export([create_insertion_query/1,milliseconds_to_datetime/1,text_format_pg/1,make_my_query/3]).

create_insertion_query({queued,Data})->
  hub_daily_counter:inc(db_queued),

  {smsrec, Uuid, _SequenceId, TenantId, TucId, _TelcoId, ApiKey, SmppId, Channel, Header, EntityId,
    TemplateId, TelemarId, Msisdn, Message, Count, Flash, _Multipart, PartId, _IsPrimaryPart,
    PartInfo, _SmsCost, _ScrubCost, _CostUnit, Encoding, _Strategy, IncomingTS, AcceptOneXTS, QueueTs,
    _SubmitTs, _AcceptTs, _Status, CampaignId,CampaignInstanceId, _, _, _, _, _, _, _, _, _, _,
    Retry, _SubmitCount, MaxTries, OrgPriority, CurrPriority, _, _, _, Trace, Kv, _, _} = Data,
%% ^ add circle_id in Data
  CircleId      = rand:uniform(20), %% Hardcoding circle_id till we get it from the data
  Date          = util:ts_to_date_value_string(IncomingTS),
  SubmitValues  = [text_format_pg(Uuid), milliseconds_to_datetime(IncomingTS), handle_null_and_undefined(TenantId),
    handle_null_and_undefined(TucId), handle_null_and_undefined(CampaignId), handle_null_and_undefined(CampaignInstanceId), text_format_pg(Header),
    text_format_pg(TemplateId), handle_null_and_undefined(SmppId), handle_null_and_undefined(CircleId), text_format_pg(Channel), text_format_pg(ApiKey),
    encode_kv(Kv)],

  MsgInfoValues = [milliseconds_to_datetime(IncomingTS), text_format_pg(Uuid), text_format_pg(EntityId), text_format_pg(TelemarId), text_format_pg(Msisdn),
    fix_single_quote_in_quoted_string(Message), handle_null_and_undefined(Count), handle_null_and_undefined(Flash),
    handle_null_and_undefined(PartId), encode_part_info(PartInfo), text_format_pg(Encoding), milliseconds_to_datetime(AcceptOneXTS),
    milliseconds_to_datetime(QueueTs), handle_null_and_undefined(Retry), handle_null_and_undefined(MaxTries),
    handle_null_and_undefined(OrgPriority), handle_null_and_undefined(CurrPriority), encode_kv(Trace), encode_kv(Kv)],
  {Date, SubmitValues, MsgInfoValues};


create_insertion_query({acked,Data})->
  hub_daily_counter:inc(db_submit),
  {Uuid, TelcoId, IncomingTS, _TucId, SubmitTs, AckTs, GatewayId, PorterId} = Data,
  Date        = util:ts_to_date_value_string(IncomingTS),
  AckedValues = [text_format_pg(Uuid), text_format_pg(TelcoId), milliseconds_to_datetime(IncomingTS),
    milliseconds_to_datetime(SubmitTs), milliseconds_to_datetime(AckTs), handle_null_and_undefined(GatewayId),
    handle_null_and_undefined(PorterId)],
  {Date, AckedValues};


create_insertion_query({dlvr,Data})->
  hub_daily_counter:inc(db_dlrvd),

  {Uuid, TelcoId, IncomingTS, _TucId, DlrErrorCode, DlrSubmitDate, DlrDoneDate, DlrSub,
    SubmitTs, AckTs, DlrTs, PorterId, GatewayId, DlrStatus, DlrText} = Data,
  DlrCode        = "001", %% harcoding DlrCode till we get it from the data
  OnexDlrErrCode = DlrErrorCode, %% harcoding OnexDlrErrCode till we get it from the data
  OnexDlrCode    = DlrCode, %% harcoding OnexDlrCode till we get it from the data
  DlrRespTs      = DlrTs, %% harcoding DlrRespTs till we get it from the data
  RetryCount     = 0, %% harcoding RetryCount till we get it from the data
  Date           = util:ts_to_date_value_string(IncomingTS),

  DlrValues = [text_format_pg(TelcoId), text_format_pg(Uuid), milliseconds_to_datetime(IncomingTS),
    text_format_pg(DlrSubmitDate), text_format_pg(DlrDoneDate), text_format_pg(DlrStatus), text_format_pg(DlrSub), text_format_pg(DlrErrorCode),
    text_format_pg(DlrCode), text_format_pg(OnexDlrErrCode), text_format_pg(OnexDlrCode), milliseconds_to_datetime(SubmitTs), milliseconds_to_datetime(AckTs),
    text_format_pg(GatewayId), text_format_pg(PorterId), milliseconds_to_datetime(DlrTs), milliseconds_to_datetime(DlrRespTs),
    text_format_pg(binary_to_list(DlrText)), handle_null_and_undefined(RetryCount)],
  {Date, DlrValues};

create_insertion_query({_,_})->
  [].

to_list(Val) when is_list(Val)->  % Converts any data format to list
  Val;
to_list(Val)->
  lists:flatten(io_lib:format("~p", [Val])).

encode_kv(undefined)->
  "null";
encode_kv(null)->
  "null";
encode_kv([])->
  "null";
encode_kv(KV)->
  EncodedKV = jiffy:encode({KV}),
  text_format_pg(EncodedKV).

handle_null_and_undefined(null)->
  "null";

handle_null_and_undefined([])->
  "null";

handle_null_and_undefined(undefined)->
  "null";

handle_null_and_undefined(all)->
  "null";

handle_null_and_undefined(Value)->
  Value.

text_format_pg(null)->
  "''";

text_format_pg([])->
  "''";

text_format_pg(undefined)->
  "''";

text_format_pg(all)->
  "''";

text_format_pg(FieldVal) when is_atom(FieldVal) ->
  text_format_pg(to_list(FieldVal));

text_format_pg(FieldVal) when is_integer(FieldVal)->
  text_format_pg(erlang:integer_to_list(FieldVal));

text_format_pg(FieldVal) when is_binary(FieldVal)->
  text_format_pg(erlang:binary_to_list(FieldVal));

text_format_pg(FieldVal) when is_list(FieldVal)->
  "'"++FieldVal++"'";

text_format_pg(FieldVal)->
  to_list(FieldVal).

make_my_query([],_Parameters,Query)->
  lists:flatten(Query);

make_my_query([$$|T],Parameters,Query)->
  [Current|NewT] = T,
  {RemainingQuery,ParamIndex} = handle_double_digit_value(NewT,Current),
  Param = to_list(lists:nth(ParamIndex,Parameters)), % Parameter is added in string format
  make_my_query(RemainingQuery,Parameters,[Query|[Param]]);

make_my_query([H|T],Parameters,Query)->
  make_my_query(T,Parameters,[Query|[H]]).

handle_double_digit_value([],Val)->
  {[],Val-48};
handle_double_digit_value(Query,Val)->
  [PararmIndex|Remaining] = Query, % Param index will denote ascii value of number , so subtract 48 from actual value
  IsNum = ((PararmIndex - 48)>=0) and ((PararmIndex - 48)=<9),
  case IsNum of
    true  ->
      Index = (Val-48)*10 + (PararmIndex - 48),
      {Remaining,Index};
    false -> {Query,Val-48}
  end.

milliseconds_to_datetime(undefined)->
  null;

milliseconds_to_datetime(null)->
  null;

milliseconds_to_datetime(Milliseconds)->
  BaseDate      = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
  Seconds       = BaseDate + (Milliseconds div 1000),
  DateTime = calendar:gregorian_seconds_to_datetime(Seconds),
  {{Year,Month,Day},{Hour,Min,Sec}}=calendar:universal_time_to_local_time(DateTime),
  "'"++integer_to_list(Year)++"-"++integer_to_list(Month)++"-"++integer_to_list(Day)++" "++integer_to_list(Hour)++":"++integer_to_list(Min)++":"++integer_to_list(Sec)++"'".

fix_single_quote_in_quoted_string([])->
  "null";
fix_single_quote_in_quoted_string(null)->
  "null";
fix_single_quote_in_quoted_string("null")->
  "null";
fix_single_quote_in_quoted_string(undefined)->
  "null";
fix_single_quote_in_quoted_string(QuotedString) when is_number(QuotedString)->
  fix_single_quote_in_quoted_string(to_list(QuotedString));

fix_single_quote_in_quoted_string(QuotedString) when is_binary(QuotedString)->
  fix_single_quote_in_quoted_string(to_list(QuotedString));

fix_single_quote_in_quoted_string(QuotedString)->
  UnquotedString = lists:sublist(QuotedString,2,length(QuotedString)-2),
  FixedQuotedString = lists:flatten([case X of $' -> "''"; _ -> X end || X <- UnquotedString]),
  text_format_pg(FixedQuotedString).

encode_part_info(undefined)->
  "null";
encode_part_info(null)->
  "null";
encode_part_info([])->
  "null";
encode_part_info(PartInfo)->
  ValList = [erlang:binary_to_list(Val) || Val<-PartInfo],
  "'{"++string:join(ValList,",")++"}'".
