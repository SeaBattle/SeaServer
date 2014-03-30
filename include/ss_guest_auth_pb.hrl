-ifndef(GUEST_AUTH_PB_H).
-define(GUEST_AUTH_PB_H, true).
-record(guest_auth, {
    uid = erlang:error({required, uid})
}).
-endif.

