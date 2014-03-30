-ifndef(HEADER_PB_H).
-define(HEADER_PB_H, true).
-record(header, {
    type = erlang:error({required, type}),
    protocol = erlang:error({required, protocol}),
    apiversion = erlang:error({required, apiversion}),
    packet = erlang:error({required, packet})
}).
-endif.

