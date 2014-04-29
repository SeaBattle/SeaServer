-ifndef(PLAYER_PACKET_PB_H).
-define(PLAYER_PACKET_PB_H, true).
-record(player_packet, {
    level = erlang:error({required, level}),
    ban_type = erlang:error({required, ban_type}),
    ban_end = erlang:error({required, ban_end}),
    name = erlang:error({required, name}),
    icon = erlang:error({required, icon})
}).
-endif.

