:-use_module(expert).
:-use_module(environment).
:-use_module(thief).
:-use_module(victim).

start:-
        begin,
        end.

begin:-
        format("~n~n~n-------STREET ROBBERY SIMULATOR--------~n~n~n",[]),
        format("----!#$----!#$----!#$----!#$----!#$----!#$----~n~n~n",[]),
		format("In this simple simulation agents start on the opposite sides of a 10x3 board.~n",[]),
		format("The thief wants to approach the victim and 'convince' it to 'handle' its property.~n",[]),
		format("The victim wants to pass to the other side of the board.~n",[]),
		format("Agents behave according to a utility function.~n",[]),
		format("While calculating the function agent takes under consideration both its ad opponent's features.~n",[]),
		format("Agent sees opponent's vigour, suspiciousness and weapon if it is drawn during the simulation.~n",[]),
		format("The simulation is divided into two main parts:~n",[]),
		format("when agents approach each other and they confront each other.~n",[]),
		format("For more details look under the hood~n",[]),
		format("~nENJOY!!!~n",[]),
		format("~n~n----!#$----!#$----!#$----!#$----!#$----!#$----~n~n~n",[]),
        random_1_to_5(TV),
        format("Random thief's vigour index(1-5) is ~p.~n",[TV]),
        %read(TV),
        assert(thief:thief_vigour(TV)),
        assert(environment:thief_vigour(TV)),
        random_1_to_5(TB),
        format("Random thief's bravery index(1-5) is ~p.~n",[TB]),
        assert(thief:thief_bravery(TB)),
        random_1_to_8(TS),
        format("Random thief's suspiciousness index(1-8) is ~p.~n",[TS]),
        assert(thief:thief_suspiciousness(TS)),
        assert(environment:thief_suspiciousness(TS)),
        random_weapon(TW),
        format("Random thief's weapon: fists(1), knife(5), pistol(20) is ~p.~n",[TW]),
        assert(thief:thief_weapon(TW)),
        random_1_to_5(VV),
        format("Random victim's vigour index(1-5) is ~p.~n",[VV]),
        assert(victim:victim_vigour(VV)),
        assert(environment:victim_vigour(VV)),
        random_1_to_5(VB),
        format("Random victim's bravery index(1-5) is ~p.~n",[VB]),
        assert(victim:victim_bravery(VB)),
        random_1_to_8(VS),
        format("Random victim's suspiciousness index(1-8) is ~p.~n",[VS]),
        assert(victim:victim_suspiciousness(VS)),
        assert(environment:victim_suspiciousness(VS)),
        random_weapon(VW),
        format("Random victim's weapon: fists(1), knife(5), pistol(20) is ~p.~n",[VW]),
        assert(victim:victim_weapon(VW)),
        random_direction(DIR),
        format("Random direction the victim should be heading(east/west) is ~p.~n",[DIR]),
        assert(victim:aim_direction(DIR)),
        
		set_locations(DIR),
		
        set_eloquence,
        assert(thief:thief_distance_memory([11,11])),
        assert(thief:thief_action_utility(0)),
        assert(thief:thief_handles(0)),
        assert(environment:thief_handles(0)),
        assert(environment:thief_says(nothing)),
        assert(thief:thief_conversation_memory([])),
        assert(thief:thief_wants(phone)),
        assert(environment:thief_direction(none,none)),
        assert(thief:thief_possession(nothing)),
        assert(thief:game(on)),
        
		random_belief(BEL),
		assert(victim:victim_belief(BEL)),
        format("Victim's random beflief(conformist/evolutionist) is ~p.~n",[BEL]),
		assert(victim:victim_distance_memory([11,11])),
        assert(victim:victim_action_utility(0)),
        assert(victim:victim_handles(0)),
        assert(environment:victim_handles(0)),
        assert(environment:victim_says(nothing)),
        assert(victim:victim_conversation_memory([])),
        assert(environment:victim_direction(none,none)),
        assert(victim:victim_possession([phone,money,wallet])),
        assert(victim:game(on)),
        assert(environment:available_object(nothing)),
        assert(thief:action(nothing)),
		assert(victim:action(nothing)),
		assert(thief:abandonment(no)),
		assert(thief:property_taken(no)),
		assert(victim:justification(no)),
		assert(thief:excuse(no)),
		
		environment:thief_location(TLOC),
		environment:victim_location(VLOC),
		
		draw_street(TLOC,VLOC),
		
        round.
        
end:-
        retractall(environment:thief_location(_)),
        retractall(environment:victim_location(_)),
        retractall(environment:thief_direction(_,_)),
        retractall(environment:victim_direction(_,_)),
        retractall(environment:thief_vigour(_)),
        retractall(environment:victim_vigour(_)),
        retractall(environment:victim_suspiciousness(_)),
        retractall(environment:thief_suspiciousness(_)),
        retractall(environment:available_object(_)),
        retractall(environment:thief_says(_)),
        retractall(environment:victim_says(_)),
        retractall(environment:victim_handles(_)),
        retractall(environment:thief_handles(_)),
        retractall(environment:thief_uses_force_against_victim(_)),
        retractall(environment:victim_uses_force_against_thief(_)),
		
        retractall(thief:distance_from_each_other(_)),
        retractall(thief:thief_distance_memory(_)),
        retractall(thief:neighborhood(_)),
        retractall(thief:thief_location_memory(_)),
        retractall(thief:thief_action_utility(_)),
        retractall(thief:thief_wants(_)),
        retractall(thief:thief_says(_)),
        retractall(thief:thief_conversation_memory(_)),
        retractall(thief:thief_vigour(_)),
        retractall(thief:thief_bravery(_)),
        retractall(thief:thief_suspiciousness(_)),
        retractall(thief:thief_weapon(_)),
        retractall(thief:thief_possession(_)),
        retractall(thief:game(_)),
        retractall(thief:thief_eloquence(_)),
        retractall(thief:action(_)),
		retractall(thief:abandonment(_)),
		retractall(thief:property_taken(_)),
		retractall(thief:excuse(_)),
		
        retractall(victim:distance_from_each_other(_)),
        retractall(victim:victim_distance_memory(_)),
        retractall(victim:neighborhood(_)),
        retractall(victim:victim_location_memory(_)),
        retractall(victim:victim_action_utility(_)),
        retractall(victim:victim_belief(_)),
        retractall(victim:victim_conversation_memory(_)),
        retractall(victim:victim_says(_)),
        retractall(victim:victim_vigour(_)),
        retractall(victim:victim_bravery(_)),
        retractall(victim:victim_suspiciousness(_)),
        retractall(victim:victim_weapon(_)),
        retractall(victim:victim_possession(_)),
        retractall(victim:aim_direction(_)),
        retractall(victim:game(_)),
        retractall(victim:victim_eloquence(_)),
		retractall(justification(_)),
		retractall(victim:action(_)).
        
round:-
        thief:game(over).

round:-
        victim:game(over).
        
round:-
        thief:game(on),
        victim:game(on),
        environment:thief_location(TLOC),
        environment:victim_location(VLOC),
        set_victim_direction(TLOC,VLOC),
        thief:thief_turn,
		thief:thief_location_memory([TLM,_]),
        retractall(environment:victim_says(_)),
        assert(environment:victim_says(nothing)),
        set_thief_direction(VLOC,TLOC),
        victim:victim_turn,
		victim:victim_location_memory([VLM,_]),
		draw_street(TLM,VLM),
        retractall(environment:thief_says(_)),
        assert(environment:thief_says(nothing)),
		expert:is_there_a_crime,
        %thief:thief_conversation_memory(TCM),
        %victim:victim_conversation_memory(VCM),
        %format("~nRound has finished: Thief's conversation memory is ~p and victim's conversation memory is ~p.~n",[TCM,VCM]),
        %format("~nThief's location is ~p and victim's location is ~p.~n",[TLM,VLM]),
        round.
        
%-----------------------------------------
%   randomise some of the initial values
%-----------------------------------------
		
random_1_to_5(RAND):-
        random_between(1,5,RAND).

random_1_to_8(RAND):-
        random_between(1,8,RAND).
        
random_weapon(WEAPON):-
        random_between(1,3,RAND),
        nth1(RAND,[1,5,20],WEAPON).
        
random_direction(DIR):-
        random_between(1,2,RAND),
        nth1(RAND,[east,west],DIR).
        
random_belief(BEL):-
        random_between(1,2,RAND),
        nth1(RAND,[evolutionist,conformist],BEL).
                
set_eloquence:-
        random_between(1,3,TELO),
        assert(thief:thief_eloquence(TELO)),
        random_between(1,3,VELO),
        assert(victim:victim_eloquence(VELO)).
        
set_locations(east):-
        random_between(1,3,Y1),
        assert(environment:thief_location([10,Y1])),
        assert(thief:thief_location_memory([[10,Y1],[10,Y1]])),
        random_between(1,3,Y2),
        assert(environment:victim_location([1,Y2])),
        assert(victim:victim_location_memory([[1,Y2],[1,Y2]])),
        format("Thief's location is ~p and victim's location is ~p.~n~n~n",[[10,Y1],[1,Y2]]).

set_locations(west):-
        random_between(1,3,Y1),
        assert(environment:thief_location([1,Y1])),
        assert(thief:thief_location_memory([[1,Y1],[1,Y1]])),
        random_between(1,3,Y2),
        assert(environment:victim_location([10,Y2])),
        assert(victim:victim_location_memory([[10,Y2],[10,Y2]])),
        format("Thief's location is ~p and victim's location is ~p.~n",[[1,Y1],[10,Y2]]).
      
%------------------------------------------------
%            draw agents' locations
%------------------------------------------------
	  
draw_street([X1,Y1],[X2,Y2]):-
	format("~n|",[]),
	draw_street([X1,Y1],[X2,Y2],1,1),
	format("~n|",[]),
	draw_street([X1,Y1],[X2,Y2],2,1),
	format("~n|",[]),
	draw_street([X1,Y1],[X2,Y2],3,1).
	
draw_street(_,_,_,11).
	
draw_street([_,Y1],[_,Y2],LEVEL,_):-
	Y1 \= LEVEL,
	Y2 \= LEVEL,
	format(" | | | | | | | | | |",[]).
	draw_street([_,_],[_,_],_,11).
	
draw_street([X,Y],XY,LEVEL,ACCUM):-
	Y =:= LEVEL,
	X =:= ACCUM,
	format("T|",[]),
	AC is ACCUM + 1,
	draw_street([X,Y],XY,LEVEL,AC).
	
draw_street(XY,[X,Y],LEVEL,ACCUM):-
	Y =:= LEVEL,
	X =:= ACCUM,
	format("V|",[]),
	AC is ACCUM + 1,
	draw_street(XY,[X,Y],LEVEL,AC).
	
draw_street([X1,Y1],[X2,Y2],LEVEL,ACCUM):-
	X1 \= ACCUM,
	X2 \= ACCUM,
	format(" |",[]),
	AC is ACCUM + 1,
	draw_street([X1,Y1],[X2,Y2],LEVEL,AC).
	
draw_street([X,Y],XY,LEVEL,ACCUM):-
	X = ACCUM,
	Y \= LEVEL,
	format(" |",[]),
	AC is ACCUM + 1,
	draw_street([X,Y],XY,LEVEL,AC).
	
draw_street(XY,[X,Y],LEVEL,ACCUM):-
	X = ACCUM,
	Y \= LEVEL,
	format(" |",[]),
	AC is ACCUM + 1,
	draw_street(XY,[X,Y],LEVEL,AC).
		
		
%------------------------------------------------
% calculate agents directions towards one another
%
% and dr Adam Meissner's elegant solution:
%
% rel_geo_pos((Xt,Yt),(Xv,Yv),(Xrg,Yrg)) :-
%   rel_geo_pos1(Xt,Xv,[east,none,west],Xrg),
%   rel_geo_pos1(Yt,Yv,[north,none,south],Yrg).
%
% rel_geo_pos1(Pt,Pv,Geos,P) :-
%   N is sign((Pt-Pv))+2,
%   nth1(N,Geos,P).
%
%------------------------------------------------
		
set_victim_direction([X1,Y1],[X2,Y2]):-
        X1 - X2 < 0,
        Y1 - Y2 < 0,
        retract(environment:victim_direction(_,_)),
        assert(environment:victim_direction(north,east)).
        
set_victim_direction([X1,Y1],[X2,Y2]):-
        X1 - X2 < 0,
        Y1 - Y2 > 0,
        retract(environment:victim_direction(_,_)),
        assert(environment:victim_direction(south,east)).
        
set_victim_direction([X1,Y1],[X2,Y2]):-
        X1 - X2 > 0,
        Y1 - Y2 < 0,
        retract(environment:victim_direction(_,_)),
        assert(environment:victim_direction(north,west)).
        
set_victim_direction([X1,Y1],[X2,Y2]):-
        X1 - X2 > 0,
        Y1 - Y2 > 0,
        retract(environment:victim_direction(_,_)),
        assert(environment:victim_direction(south,west)).
        
set_victim_direction([X1,Y1],[X2,Y2]):-
        X1 - X2 =:= 0,
        Y1 - Y2 > 0,
        retract(environment:victim_direction(_,_)),
        assert(environment:victim_direction(south,none)).
        
set_victim_direction([X1,Y1],[X2,Y2]):-
        X1 - X2 =:= 0,
        Y1 - Y2 < 0,
        retract(environment:victim_direction(_,_)),
        assert(environment:victim_direction(north,none)).
        
set_victim_direction([X1,Y1],[X2,Y2]):-
        X1 - X2 > 0,
        Y1 - Y2 =:= 0,
        retract(environment:victim_direction(_,_)),
        assert(environment:victim_direction(none,west)).

set_victim_direction([X1,Y1],[X2,Y2]):- 
        X1 - X2 < 0,
        Y1 - Y2 =:= 0,
        retract(environment:victim_direction(_,_)),
        assert(environment:victim_direction(none,east)).
        
set_victim_direction([X1,Y1],[X2,Y2]):-
        X1 - X2 =:= 0,
        Y1 - Y2 =:= 0,
        retract(environment:victim_direction(_,_)),
        assert(environment:victim_direction(none,none)).
        
set_thief_direction([X1,Y1],[X2,Y2]):-
        X1 - X2 < 0,
        Y1 - Y2 < 0,
        retract(environment:thief_direction(_,_)),
        assert(environment:thief_direction(north,east)).
        
set_thief_direction([X1,Y1],[X2,Y2]):-
        X1 - X2 < 0,
        Y1 - Y2 > 0,
        retract(environment:thief_direction(_,_)),
        assert(environment:thief_direction(south,east)).
        
set_thief_direction([X1,Y1],[X2,Y2]):-
        X1 - X2 > 0,
        Y1 - Y2 < 0,
        retract(environment:thief_direction(_,_)),
        assert(environment:thief_direction(north,west)).
        
set_thief_direction([X1,Y1],[X2,Y2]):-
        X1 - X2 > 0,
        Y1 - Y2 > 0,
        retract(environment:thief_direction(_,_)),
        assert(environment:thief_direction(south,west)).
        
set_thief_direction([X1,Y1],[X2,Y2]):-
        X1 - X2 =:= 0,
        Y1 - Y2 > 0,
        retract(environment:thief_direction(_,_)),
        assert(environment:thief_direction(south,none)).
        
set_thief_direction([X1,Y1],[X2,Y2]):-
        X1 - X2 =:= 0,
        Y1 - Y2 < 0,
        retract(environment:thief_direction(_,_)),
        assert(environment:thief_direction(north,none)).
        
set_thief_direction([X1,Y1],[X2,Y2]):-
        X1 - X2 > 0,
        Y1 - Y2 =:= 0,
        retract(environment:thief_direction(_,_)),
        assert(environment:thief_direction(none,west)).
        
set_thief_direction([X1,Y1],[X2,Y2]):-
        X1 - X2 < 0,
        Y1 - Y2 =:= 0,
        retract(environment:thief_direction(_,_)),
        assert(environment:thief_direction(none,east)).
        
set_thief_direction([X1,Y1],[X2,Y2]):-
        X1 - X2 =:= 0,
        Y1 - Y2 =:= 0,
        retract(environment:thief_direction(_,_)),
        assert(environment:thief_direction(none,none)).
