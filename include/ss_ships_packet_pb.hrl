-ifndef(SHIPS_PACKET_PB_H).
-define(SHIPS_PACKET_PB_H, true).
-record(ships_packet, {
    ship = []
}).
-endif.

-ifndef(SHIP_PB_H).
-define(SHIP_PB_H, true).
-record(ship, {
    id = erlang:error({required, id}),
    experience = erlang:error({required, experience}),
    type = erlang:error({required, type}),
    level = erlang:error({required, level}),
    repair_date = erlang:error({required, repair_date}),
    name = erlang:error({required, name})
}).
-endif.

