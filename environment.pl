:-module(environment,[]).

:- dynamic ([
        thief_location/1,
        victim_location/1,
        thief_direction/2,
        victim_direction/2,
        thief_vigour/1,
        victim_vigour/1,
        victim_suspiciousness/1,
        thief_suspiciousness/1,
        available_object/1,
        thief_says/1,
        victim_says/1,
        victim_handles/1,
        thief_handles/1,
        thief_uses_force_against_victim/1,
        victim_uses_force_against_thief/1
]).
        
