-ifndef(ERROR_PACKET_PB_H).
-define(ERROR_PACKET_PB_H, true).
-record(error_packet, {
    code = erlang:error({required, code}),
    descr = erlang:error({required, descr})
}).
-endif.

