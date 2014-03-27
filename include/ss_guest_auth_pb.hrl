-ifndef(GUEST_AUTH_PB_H).
-define(GUEST_AUTH_PB_H, true).
-record(guest_auth, {
    header = erlang:error({required, header}),
    uid = erlang:error({required, uid})
}).
-endif.

-ifndef(HEADER_PB_H).
-define(HEADER_PB_H, true).
-record(header, {
    type = erlang:error({required, type}),
    protocol = erlang:error({required, protocol}),
    apiversion = erlang:error({required, apiversion})
}).
-endif.

