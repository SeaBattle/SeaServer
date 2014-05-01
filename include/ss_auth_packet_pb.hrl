-ifndef(AUTH_PACKET_PB_H).
-define(AUTH_PACKET_PB_H, true).
-record(auth_packet, {
    uid = erlang:error({required, uid}),
    password
}).
-endif.

-ifndef(REGISTER_PACKET_PB_H).
-define(REGISTER_PACKET_PB_H, true).
-record(register_packet, {
    login = erlang:error({required, login}),
    password = erlang:error({required, password}),
    uid = erlang:error({required, uid}),
    name,
    icon_url,
    motto
}).
-endif.

