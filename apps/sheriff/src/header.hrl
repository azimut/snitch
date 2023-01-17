-record(results, {qtype  :: inet_res:dns_rr_type(),
                  rtype  :: inet_res:dns_rr_type(),
                  domain :: string(),
                  server :: inet:ip_address(),
                  answers = [] :: list(string())}).

-record(error, {qtype :: inet_res:dns_rr_type(),
                server :: inet:ip_address(),
                domain :: string(),
                reason :: atom()}).
