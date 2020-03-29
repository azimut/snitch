-define(DNS_SERVERS, [{8,8,8,8},{8,8,4,4},{1,1,1,1},{9,9,9,9}]).
-define(DNS_TIMEOUT, 5).
-define(DNS_FILE, "/home/sendai/snitch_domains.txt").
-define(SERVER, ?MODULE).
-define(TABLE_NAME, mytable).
-define(RANGE_HOURS        , 12).
-define(DEFAULT_TICK       ,  5 * 60).
-define(DEFAULT_CHECKPOINT , 10 * 60).
-define(INIT_TICK          ,  1 * 60).
-define(INIT_CHECKPOINT    ,  5 * 60).
