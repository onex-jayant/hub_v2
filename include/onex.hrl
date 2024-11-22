-define(oxlog_d(Format, Args),
  logger:debug("~s" ++ Format ++ "~n",
    [string:left(lists:flatten(io_lib:format("~p(~p):", [?MODULE, ?LINE])), 20, $ ) | Args])).

-define(oxlog_i(Format, Args),
  logger:info("~s" ++ Format ++ "~n",
    [string:left(lists:flatten(io_lib:format("~p(~p):", [?MODULE, ?LINE])), 20, $ ) | Args])).

-define(oxlog_w(Format, Args),
  logger:warning("~s" ++ Format ++ "~n",
    [string:left(lists:flatten(io_lib:format("~p(~p):", [?MODULE, ?LINE])), 20, $ ) | Args])).

-define(oxlog_e(Format, Args),
  logger:error("~s" ++ Format ++ "~n",
    [string:left(lists:flatten(io_lib:format("~p(~p):", [?MODULE, ?LINE])), 20, $ ) | Args])).

-define(HEADERS, [{"Content-Type", "application/json"}]).

-define(API_KEYS_TABLE, api_keys_table).

-define(SERVER_TYPE, "hub").

-define(REGISTRY_IP, "10.133.31.163").

-define(REGISTRY_PORT, 8078).

-define(REGISTRY_ENDPOINT, "/publish").

-define(READY_STATUS, "ready").

-define(UP_STATUS, "up").

-define(REGISTRY_TIME_TO_REFRESH, 10000).

-define(REGISTRY_CACHE, registry_cache).

-define(DBTYPE, postgresql).