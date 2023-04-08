-record(dns_data, {domain :: string(),
                   ns     :: inet:ip_address(),
                   qtype  :: inet_res:dns_rr_type(),
                   rtype  :: inet_res:dns_rr_type(),
                   result :: string()}).

-record(dns_error, {domain :: string(),
                    qtype  :: inet_res:dns_rr_type(),
                    rerror :: atom(),
                    ns     :: inet:ip_address()}).
