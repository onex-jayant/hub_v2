-module(pg_queries).

-export([
  create_tables/2,
  columns/1,
  get_create_table_query/1

  ]).


create_tables(DB, Date) ->
  lists:map(fun(Query)-> pgpool:squery(DB, Query) end, get_create_table_query(Date)).

get_create_table_query(Date) ->
  [
    smsrec_submit_table(lists:concat(["smsrec_submit_", Date])),
    smsrec_msg_info_table(lists:concat(["smsrec_msg_info_", Date])),
    smsrec_acked_table(lists:concat(["smsrec_ack_", Date])),
    smsrec_dlr_table(lists:concat(["smsrec_dlr_", Date])),
    smsrec_dlr_sent_table(lists:concat(["smsrec_dlr_sent_", Date]))
  ].

smsrec_submit_table(TableName) ->
  lists:concat(["CREATE TABLE IF NOT EXISTS ", TableName, " (tx_id SERIAL, msg_id TEXT, onex_received_ts TIMESTAMP(3) with time zone, tenant_id INTEGER, tuc_id INTEGER, campaign_id INTEGER, campaign_instance_id BIGINT, header TEXT, template_id TEXT, smpp_id INTEGER, circle_id SMALLINT, channel TEXT, api_key TEXT, cust_ref JSONB default '{}');"]).

smsrec_msg_info_table(TableName) ->
  lists:concat(["CREATE TABLE IF NOT EXISTS ", TableName, " (onex_received_ts TIMESTAMP(3) with time zone, msg_id text, entity_id TEXT, telemar_id TEXT, msisdn TEXT, message_txt TEXT, count SMALLINT, flash BOOLEAN, part_id SMALLINT, part_info TEXT[], encoding TEXT, accept_onex_ts TIMESTAMP(3) with time zone, queue_ts TIMESTAMP(3) with time zone, retry BOOLEAN, max_tries SMALLINT, org_priority SMALLINT, curr_priority SMALLINT, trace JSONB, kv JSONB);"]).

smsrec_acked_table(TableName) ->
  lists:concat(["CREATE TABLE IF NOT EXISTS ", TableName, " (msg_id TEXT, telco_id TEXT, onex_received_ts TIMESTAMP(3) with time zone, submit_ts TIMESTAMP(3) with time zone, accept_ts TIMESTAMP(3) with time zone, gatewayid INTEGER, porterid INTEGER);"]).

smsrec_dlr_table(TableName) ->
  lists:concat(["CREATE TABLE IF NOT EXISTS ", TableName, " (tx_id SERIAL, telco_id TEXT, msg_id TEXT, onex_received_ts TIMESTAMP(3) with time zone, dlr_submit_ts text, dlr_done_ts text, dlr_status text, dlr_sub text, dlr_err_code text, dlr_code text, onex_dlr_err_code text, onex_dlr_code text, submit_ts TIMESTAMP(3) with time zone, accept_ts TIMESTAMP(3) with time zone, gateway_id INTEGER, porter_id INTEGER, dlr_ts TIMESTAMP(3) with time zone, dlr_resp_ts timestamp, dlr_text TEXT, retry_count SMALLINT);"]).

smsrec_dlr_sent_table(TableName) ->
  lists:concat(["CREATE TABLE IF NOT EXISTS ", TableName, " (msg_id TEXT, onex_received_ts TIMESTAMP(3) with time zone, dlr_sent_ts TIMESTAMP(3) with time zone, dlr_ack_ts TIMESTAMP(3) with time zone, dlr_retry_count INTEGER, response_code TEXT);"]).


columns(smsrec_submit)   -> " (msg_id, onex_received_ts, tenant_id, tuc_id, campaign_id, campaign_instance_id, header, template_id, smpp_id, circle_id, channel, api_key, cust_ref)";

columns(smsrec_msg_info) -> " (onex_received_ts, msg_id, entity_id, telemar_id, msisdn, message_txt, count, flash, part_id, part_info, encoding, accept_onex_ts, queue_ts, retry, max_tries, org_priority, curr_priority, trace, kv)";

columns(smsrec_ack)      -> " (msg_id, telco_id, onex_received_ts, submit_ts, accept_ts, gatewayid, porterid)";

columns(smsrec_dlr)      -> " (telco_id, msg_id, onex_received_ts, dlr_submit_ts, dlr_done_ts, dlr_status, dlr_sub, dlr_err_code, dlr_code, onex_dlr_err_code, onex_dlr_code, submit_ts, accept_ts, gateway_id, porter_id, dlr_ts, dlr_resp_ts, dlr_text, retry_count)".
