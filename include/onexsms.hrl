
-record(smsrec,
  {
    uuid,           %% We issue this UUID (for now it is ksuid)
    seq_id,         %% Integer sequence on smpp conn for the pdu, its needed for collating send_sms and its response
    tenant_id,      %% Tenant ID
    user_id,        %% TUC ID - this is unique for each of our customer sending SMS
    telco_id,       %% ID assigned by telco on succesful submission (we get it in submit_sm_resp)
    api_key,        %% key used in case came via api
    smpp_id,        %% System id if came via smpp
    channel,        %% came via (atom) - w (web), a (api), s (smpp)
    header,         %% 6 char long header
    entity_id,      %% Entity ID of sender
    template_id,    %% Template ID of SMS text
    telemar_id,     %% Telemarketer ID
    msisdn,         %% Phone no to send message to
    message_txt,    %% Text of the message
    count,          %% Normal message will be 1, but if long this denotes the number of msgs this msg refers to
    flash,          %% true or false
    multipart,      %% true or false
    part_id,        %% integer
    is_primary_part,%% true or false
    part_info,      %% if primary this is a list containing uuids of secondry part
    smscost,        %% cost of SMS
    scrubcost,      %% porteridcost of Scrub
    costunit,       %% Unit of costs - should be paisa for now
    encoding,       %% 8 = unicode, 0 = simple
    strategy,       %% {fixed, PrimaryGW, BackupGW}, onnet, roundrobin (for testing only),
    incoming_ts,    %% TS when SMS lands into OneXTel
    accept_onex_ts, %% TS when basic validations are done and message accepted, on sending submit_sm_resp
    queue_ts,       %% TS when its added to queue (not sure there is much difference with accept_ts
    submit_ts,      %% TS when SMS is submitted to telco - when esmpp_lib's send is called
    accept_ts,      %% TS when telco return OK for this and returns Telco UUID - update this when we get telcouuid
    status,
    campain_id,
    campain_instance_id,
    pdu,
    dlr,             %% true/false flag to mark if drl done
    dlr_submit_date, %% submit date to telco
    dlr_done_date,   %% dlr date from telco
    dlr_status,     %% actual status of dlr
    dlr_sub,        %% dlr sub code
    dlr_err_code,    %% dlr error code
    dlr_code,       %% Dlr code
    dlr_ts,         %% TS when dlr reveived
    dlr_resp_ts,     %% TS when dlr_resp received
    retry,          %% true/false flag to decide if this is to be retried
    submit_count,   %% Increment when submitted to telco (useful in case of failure)
    max_tries,      %% Max times this should be tried
    org_priority,   %% Priority this was submitted with
    curr_priority,  %% Current priority
    dlr_sent,       %% Flag to mark if DLR has been sent to customer
    gatewayid,      %% ID of Gateway Group
    porterid,       %% ID of Gatewway used
    trace = [],     %% Trace of actions happening on this SMS
    kv = [],        %% Extra params can be added here as key-values
    ready_for_arch, %% true/false flag to mark if record is ready to be archived
    archived        %% true/false flag to mark if this record has been archived
  }).