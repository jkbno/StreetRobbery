:-module(victim,[

]).

:-use_module(environment).

:- dynamic([
        distance_from_each_other/1,
        victim_distance_memory/1,
        neighborhood/1,
        victim_location_memory/1,
        victim_action_utility/1,
        victim_belief/1,
        victim_conversation_memory/1,
        victim_says/1,
        victim_vigour/1,
        victim_bravery/1,
        victim_suspiciousness/1,
        victim_weapon/1,
        victim_possession/1,
        aim_direction/1,
        game/1,
        victim_eloquence/1,
		justification/1
        ]).
        
victim_turn:-
        thief:game(over).
        
victim_turn:-
        format("~n~n~nVictim's turn.~n~n",[]),
        estimate_distance_from_each_other(DFEO),
        moore_neighborhood(DFEO),
        victim_update_distance_memory(DFEO),
        %format("Distance from each other is ~p.~n",[DFEO]),
        victim_perceives(TVIG,TSUS,TSAYS,_),
        victim_remembers_conversation(TSAYS),
        victim_features(VVIG,VSUS,VBR,VWEAP),
        agent_confidence(VVIG,VSUS,VBR,VWEAP,VCONF),
        agent_action_utility(TVIG,TSUS,VCONF,VAUTIL),
        retract(victim_action_utility(_)),
        assert(victim_action_utility(VAUTIL)),
		format("Victim's action utility index is ~p.~n",[VAUTIL]),
        will(to_pass_along),
        environment:victim_location(VLOC),
		%format("Victim's location is ~p.~n",[VLOC]),
        retract(victim_location_memory([X,_])),
        victim_update_location_memory(VLOC,X),
        retract(distance_from_each_other(_)),
        retract(neighborhood(_)).
        
%-----------------------------------------
%        victim's cognitive processes
%-----------------------------------------
		
estimate_distance_from_each_other(DFEO):-
        environment:thief_location([X1,Y1]),
        environment:victim_location([X2,Y2]),
        X is abs(X1 - X2),
        Y is abs(Y1 - Y2),
        DFEO is X + Y - 1,
        assert(distance_from_each_other(DFEO)).
        
moore_neighborhood(DFEO):-
        DFEO == 1,
        environment:thief_location([X1,Y1]),
        environment:victim_location([X2,Y2]),
        TEMP1 is abs(X1 - X2),
		TEMP1 == 1,
        TEMP2 is abs(Y1 - Y2),
		TEMP2 == 1,
        assert(neighborhood(yes)).
		%format("Neighbourhood is yes.~n",[]).

moore_neighborhood(DFEO):-
        DFEO == 0,
        assert(neighborhood(yes)).
		%format("Neighbourhood is yes.~n",[]).

moore_neighborhood(DFEO):-
        DFEO > 0,
        assert(neighborhood(no)).
		%format("Neighbourhood is no.~n",[]).
                
victim_update_distance_memory(DFEO):-
        victim_distance_memory([DM,_]),
        NewDM = [DFEO,DM],
        retract(victim_distance_memory(_)),
        assert(victim_distance_memory(NewDM)).
        
victim_update_location_memory(VLOC,X):-
        assert(victim_location_memory(X)),
        assert(victim_location_memory(VLOC)),
        victim_get_list([],LIST),
        assert(victim_location_memory(LIST)).
        
victim_get_list(LIST,TEMP):-
        retract(victim_location_memory(X)),
        victim_get_list([X|LIST],TEMP).

victim_get_list(TEMP,TEMP).
        
thief_closer(yes):- 
        distance_from_each_other(DFEO),
        victim_distance_memory([_,PDFEO]),
        DFEO =< PDFEO.
        
thief_closer(no).
        
agents_in_line(straight):-
        environment:thief_location([_,Y1]),
        environment:victim_location([_,Y2]),
        Y1 = Y2.

agents_in_line(adjacent):-
        environment:thief_location([_,Y1]),
        environment:victim_location([_,Y2]),
        TEMP is (abs(Y1 - Y2)),
        TEMP == 1.
        
agents_in_line(opposite):-
        environment:thief_location([_,Y1]),
        environment:victim_location([_,Y2]),
        TEMP is (abs(Y1 - Y2)),
        TEMP == 2.
        
victim_features(VVIG,VSUS,VBR,VWEAP):-
        victim_vigour(VVIG),
        victim_suspiciousness(VSUS),
        victim_bravery(VBR),
        victim_weapon(VWEAP).
        
victim_perceives(TVIG,TSUS,TSAYS,THAND):-
        environment:thief_vigour(TVIG),
        environment:thief_suspiciousness(TSUS),
        environment:thief_says(TSAYS),
        environment:thief_handles(THAND).

victim_remembers_conversation(nothing).
        
victim_remembers_conversation(SAYS):-
        dif(SAYS,nothing),
        victim_conversation_memory(TCM),
        retractall(victim_conversation_memory(_)),
        append(TCM,[SAYS],NewTCM),
        assert(victim_conversation_memory(NewTCM)).
        
agent_action_utility(VIG,SUS,ACONF,AAUTIL):-
        estimate_weapon(SUS,EWEAP),
        estimate_opponent_action_utility(VIG,SUS,EWEAP,OAUTIL),
        AAUTIL is (ACONF - OAUTIL).
        
agent_confidence(VIG,SUS,BR,WEAP,ACONF):-
        ACONF is (VIG + SUS + BR + WEAP).
        
estimate_opponent_action_utility(VIG,SUS,EWEAP,OAUTIL):-
        OAUTIL is (VIG + SUS + EWEAP).
        
estimate_weapon(SUS,0):-
        SUS < 5.

estimate_weapon(SUS,2):-
        SUS > 4,
        SUS < 7.
        
estimate_weapon(SUS,5):-
        SUS > 6,
        SUS < 9.
        
%-----------------------------------------
%     intention is plan and will
%-----------------------------------------
		
will(to_pass_along):-
        victim_action_utility(VAU),
        VAU < 1,
        retract(game(_)),
        assert(game(over)),
        format("Game over, victim is running away!~n",[]).
        
will(to_pass_along):-
        plan(to_pass_along).
        
plan(to_pass_along):-
        neighborhood(no),
        victim_action_utility(VAUTIL),
        environment:victim_location(VLOC),
        agents_in_line(AILIN),
        thief_closer(TCLO),
        how_to_avoid_thief(VAUTIL,VLOC,AILIN,TCLO).
		%format("Victim avoids.~n",[]).
        
plan(to_pass_along):-
        neighborhood(yes),
		victim_conversation_memory([]),
        victim_action_utility(VAUTIL),
        environment:victim_location(VLOC),
        agents_in_line(AILIN),
        thief_closer(TCLO),
        how_to_avoid_thief(VAUTIL,VLOC,AILIN,TCLO).
		%format("Victim avoids.~n",[]).
		
plan(to_pass_along):-
        neighborhood(yes),
        victim_action_utility(VAUTIL),
        victim_belief(VB),
        victim_perceives(_,_,_,THAND),
        how_to_deal_with_thief(VB,VAUTIL,THAND).
		%format("Victim deals.~n",[]).
        
%-----------------------------------------
% victim's actions to reach the other side
%-----------------------------------------      

how_to_avoid_thief(VAUTIL,VLOC,AILIN,TCLO):-
        AILIN == straight,
        TCLO == yes,
        VAUTIL > 9,
        aim_direction(AD),
        move(AD,VLOC).
        
how_to_avoid_thief(VAUTIL,VLOC,AILIN,TCLO):-
        AILIN == straight,
        TCLO == yes,
        VAUTIL > 0,
        VAUTIL < 10,
        move(VLOC).
        
how_to_avoid_thief(VAUTIL,VLOC,AILIN,TCLO):-
        AILIN == adjacent,
        TCLO == yes,
        VAUTIL > 0,
        aim_direction(AD),
        move(AD,VLOC).
        
how_to_avoid_thief(VAUTIL,VLOC,AILIN,TCLO):-
        AILIN == opposite,
        TCLO == yes,
        VAUTIL > 0,
        aim_direction(AD),
        move(AD,VLOC).
        
move(east,[X,Y]):- 
        X1 is X + 1,
        retract(environment:victim_location(_)),
        assert(environment:victim_location([X1,Y])).

move(west,[X,Y]):- 
        X1 is X - 1,
        retract(environment:victim_location(_)),
        assert(environment:victim_location([X1,Y])).
		
move([X,Y]):-
        Y == 1,
        Y1 is Y + 1,
        retract(environment:victim_location(_)),
        assert(environment:victim_location([X,Y1])).
        
move([X,Y]):-
        Y == 2,
        Y1 is Y + 1,
        retract(environment:victim_location(_)),
        assert(environment:victim_location([X,Y1])).
        
move([X,Y]):-
        Y == 3,
        Y1 is Y - 1,
        retract(environment:victim_location(_)),
        assert(environment:victim_location([X,Y1])).
        
%-----------------------------------------
%           when agents meet
%-----------------------------------------
        
how_to_deal_with_thief(VB,VAUTIL,_):-
        victim_conversation_memory([TW]),
        TW \= nothing,
        VB == evolutionist,
        VAUTIL > 0,
        answer(no).

how_to_deal_with_thief(VB,VAUTIL,_):-
        victim_conversation_memory([TW,_,_]),
        VB == evolutionist,
        VAUTIL > 0,
        VAUTIL < 4,
        answer(yes),
        give(TW),
		format("Victim gives its property: ~p.~n",[TW]).
        
how_to_deal_with_thief(VB,VAUTIL,THAND):-
        victim_conversation_memory([_,_,_]),
        VB == evolutionist,
        VAUTIL > 3,
        THAND == 0,
        answer(no).

how_to_deal_with_thief(VB,VAUTIL,THAND):-
        victim_conversation_memory([TW,_,_,_]),
        VB == evolutionist,
        THAND > 0,
        VAUTIL > 3,
        VAUTIL < 7,
        answer(yes),
        give(TW),
		format("Victim gives its property: ~p.~n",[TW]).

how_to_deal_with_thief(VB,VAUTIL,THAND):-
        victim_conversation_memory([_,_,_,_]),
        VB == evolutionist,
        THAND > 0,
        VAUTIL > 6,
        VAUTIL < 10,
        answer(no).
        
how_to_deal_with_thief(VB,VAUTIL,THAND):-
        victim_conversation_memory([_,_,_,_]),
        VB == evolutionist,
        THAND > 0,
        VAUTIL > 9,
        victim_weapon(VW),
        retract(environment:victim_handles(_)),
        assert(environment:victim_handles(VW)),
        victim_eloquence(VE),
        answer(VE),
        assert(environment:victim_uses_force_against_thief),
        retract(game(_)),
        assert(game(over)),
		retractall(justification(_)),
		assert(justification(self-defence)),
		format("Victim pulls out: ~p.~n",[VW]),
		format("Victim uses force against thief.~n",[]),
		format("&%$#@!@$@~n",[]),
        format("Game over, victim wins!~n",[]).
        
how_to_deal_with_thief(VB,VAUTIL,_):-
        victim_conversation_memory([TW]),
        TW \= nothing,
        VB == conformist,
        VAUTIL > 0,
        VAUTIL < 4,
        answer(yes),
        give(TW),
		format("Victim gives its property: ~p.~n",[TW]).

how_to_deal_with_thief(VB,VAUTIL,_):-
        victim_conversation_memory([TW]),
        TW \= nothing,
        VB == conformist,
        VAUTIL > 3,
        answer(no).
        
how_to_deal_with_thief(VB,VAUTIL,_):-
        victim_conversation_memory([TW,_,_]),
        VB == conformist,
        VAUTIL > 3,
        VAUTIL < 7,
        answer(yes),
        give(TW),
		format("Victim gives its property: ~p.~n",[TW]).
        
how_to_deal_with_thief(VB,VAUTIL,THAND):-
        victim_conversation_memory([_,_,_]),
        VB == conformist,
        THAND == 0,
        VAUTIL > 6,
        answer(no).

how_to_deal_with_thief(VB,VAUTIL,THAND):-
        victim_conversation_memory([TW,_,_,_]),
        VB == conformist,
        THAND > 0,
        VAUTIL > 6,
        VAUTIL < 10,
        answer(yes),
        give(TW).

how_to_deal_with_thief(VB,VAUTIL,THAND):-
        victim_conversation_memory([_,_,_,_]),
        VB == conformist,
        THAND > 0,
        VAUTIL > 9,
        answer(no).

give(TW):-
        victim_possession(VP),
        delete(VP,TW,NEWVP),
        retract(victim_possession(_)),
        assert(victim_possession(NEWVP)),
        retract(environment:available_object(_)),
        assert(environment:available_object(TW)).
        
%-----------------------------------------
%           what victim has to say
%-----------------------------------------
		
answer(yes):-
        retract(environment:victim_says(_)),
        assert(environment:victim_says(yes)),
        format("VICTIM: Yes.~n",[]),
        victim_remembers_conversation(yes).

answer(no):-
        retract(environment:victim_says(_)),
        assert(environment:victim_says(no)),
        format("VICTIM: No.~n",[]),
        victim_remembers_conversation(no).
        
defense_answer(1):-
        retract(environment:victim_says(_)),
        assert(environment:victim_says(pity_fool)),
        format("VICTIM: Pity fool.~n",[]),
        victim_remembers_conversation(pity_fool).
        
defense_answer(2):-
        retract(environment:victim_says(_)),
        assert(environment:victim_says(yippee_ki-yay)),
        format("VICTIM: Yippee_ki-yay!~n",[]),
        victim_remembers_conversation(yippee_ki-yay).
        
defense_answer(3):-
        retract(environment:victim_says(_)),
        assert(environment:victim_says(you_talking_to_me)),
        format("VICTIM: You talking to me?~n",[]),
        victim_remembers_conversation(you_talking_to_me).
