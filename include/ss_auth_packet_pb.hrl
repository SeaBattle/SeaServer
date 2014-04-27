-ifndef(AUTH_PACKET_PB_H).
-define(AUTH_PACKET_PB_H, true).
-record(auth_packet, {
    uid = erlang:error({required, uid}),
    password
}).
-endif.

