:-module(thief,[

]).

:-use_module(environment).

:- dynamic([
        distance_from_each_other/1,
        thief_distance_memory/1,
        neighborhood/1,
        thief_location_memory/1,
        thief_action_utility/1,
        thief_wants/1,
        thief_says/1,
        thief_conversation_memory/1,
        thief_vigour/1,
        thief_bravery/1,
        thief_suspiciousness/1,
        thief_weapon/1,
        thief_possession/1,
        game/1,
        thief_eloquence/1,
		action/1,
		abandonment/1,
		property_taken/1,
		excuse/1
        ]).
        
thief_turn:-
        format("~n~n~nThief's turn.~n~n",[]),
        estimate_distance_from_each_other(DFEO),
        moore_neighborhood(DFEO),
        thief_update_distance_memory(DFEO),
        %format("Distance from each other is ~p.~n",[DFEO]),
        thief_perceives(VVIG,VSUS,VSAYS,_),
        thief_remembers_conversation(VSAYS),
        thief_features(TVIG,TSUS,TBR,TWEAP),
        agent_confidence(TVIG,TSUS,TBR,TWEAP,TCONF),
        agent_action_utility(VVIG,VSUS,TCONF,TAUTIL),
        retract(thief_action_utility(_)),
        assert(thief_action_utility(TAUTIL)),
        format("Thief's action utility index is ~p.~n",[TAUTIL]),
        will(to_get_somethnig),
        environment:thief_location(TLOC),
		%format("Thief's location is ~p.~n",[TLOC]),
        retract(thief_location_memory([X,_])),
        thief_update_location_memory(TLOC, X),
        retract(distance_from_each_other(_)),
        retract(neighborhood(_)).
        
%-----------------------------------------
%        thief's cognitive processes
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
                
thief_update_distance_memory(DFEO):-
        thief_distance_memory([DM,_]),
        NewDM = [DFEO,DM],
        retract(thief_distance_memory(_)),
        assert(thief_distance_memory(NewDM)).
        
thief_update_location_memory(TLOC,X):-
        assert(thief_location_memory(X)),
        assert(thief_location_memory(TLOC)),
        thief_get_list([],LIST),
        assert(thief_location_memory(LIST)).
        
thief_get_list(LIST,TEMP):-
        retract(thief_location_memory(X)),
        thief_get_list([X|LIST],TEMP).
        
thief_get_list(TEMP,TEMP).

victim_closer(yes):-
        distance_from_each_other(DFEO),
        thief_distance_memory([_,PDFEO]),
        DFEO =< PDFEO.
        
victim_closer(no).
        
thief_features(TVIG,TSUS,TBR,TWEAP):-
        thief_vigour(TVIG),
        thief_suspiciousness(TSUS),
        thief_bravery(TBR),
        thief_weapon(TWEAP).
        
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
        
thief_perceives(VVIG,VSUS,VSAYS,VHAND):-
        environment:victim_vigour(VVIG),
        environment:victim_suspiciousness(VSUS),
        environment:victim_says(VSAYS),
        environment:victim_handles(VHAND).
        
thief_remembers_conversation(nothing).
        
thief_remembers_conversation(SAYS):-
        dif(SAYS,nothing),
        thief_conversation_memory(TCM),
        retractall(thief_conversation_memory(_)),
        append(TCM,[SAYS],NewTCM),
        assert(thief_conversation_memory(NewTCM)).
        
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
		
will(to_get_somethnig):-
        thief_action_utility(TAU),
        TAU < 1,
		retractall(action(_)),
		assert(action(move)),
        retract(game(_)),
        assert(game(over)),
        format("Game over, thief is running away!~n",[]).
        
will(to_get_somethnig):-
        plan(to_get_somethnig).
        
plan(to_get_somethnig):-
        distance_from_each_other(DFEO),
        neighborhood(no),
        thief_action_utility(TAUTIL),
        environment:thief_location(TLOC),
        agents_in_line(AILIN),
        victim_closer(VCLO),
        how_to_approach_victim(DFEO,TAUTIL,AILIN,VCLO,TLOC).
		%format("Thief approaches.~n",[]).
        
plan(to_get_somethnig):-
        thief_action_utility(TAUTIL),
        neighborhood(yes),
        thief_wants(TW),
        how_to_talk_to_victim(TW,TAUTIL).
		%format("Thief talks.~n",[]).
        
plan(to_get_somethnig):-
        thief_action_utility(TAUTIL),
        neighborhood(yes),
        TAUTIL > 6,
        thief_wants(TW),
        how_to_confront_victim(TW,TAUTIL).
		%format("Thief confronts.~n",[]).
        
%-----------------------------------------
%     thief's actions to meet victim
%-----------------------------------------
                
how_to_approach_victim(DFEO,TAUTIL,AILIN,VCLO,[X,Y]):-
        AILIN == straight,
        VCLO == yes,
        DFEO > 0,
        TAUTIL > 0,
        environment:victim_direction(_,VD),
        move(VD,[X,Y]),
		retractall(action(_)),
		assert(action(move)).
        
how_to_approach_victim(DFEO,TAUTIL,AILIN,VCLO,[X,Y]):-
        AILIN == adjacent,
        VCLO == yes,    
        DFEO > 1,
        TAUTIL > 0,
        environment:victim_direction(_,VD),
        move(VD,[X,Y]),
		retractall(action(_)),
		assert(action(move)).
        
how_to_approach_victim(DFEO,TAUTIL,AILIN,VCLO,[X,Y]):-
        AILIN == adjacent,
        VCLO == yes,
        DFEO == 2,
        TAUTIL > 9,
        environment:victim_direction(VD,_),
        move(VD,[X,Y]),
		retractall(action(_)),
		assert(action(move)).
        
how_to_approach_victim(DFEO,TAUTIL,AILIN,VCLO,[X,Y]):-
        AILIN == opposite,
        VCLO == yes,
        DFEO == 2,
        TAUTIL > 0,
        TAUTIL < 10,
        environment:victim_direction(VD,_),
        move(VD,[X,Y]),
		retractall(action(_)),
		assert(action(move)).
        
how_to_approach_victim(DFEO,TAUTIL,AILIN,VCLO,[X,Y]):-
        AILIN == opposite,
        VCLO == yes,
        DFEO == 4,
        TAUTIL > 9,
        environment:victim_direction(VD,_),
        move(VD,[X,Y]),
		retractall(action(_)),
		assert(action(move)).
		
how_to_approach_victim(DFEO,TAUTIL,AILIN,VCLO,[X,Y]):-
        AILIN == opposite,
        VCLO == yes,
        DFEO > 2,
        TAUTIL > 0,
        environment:victim_direction(_,VD),
        move(VD,[X,Y]),
		retractall(action(_)),
		assert(action(move)).
				
move(east,[X,Y]):-
        X1 is X + 1,
        retract(environment:thief_location(_)),
        assert(environment:thief_location([X1,Y])).
        
move(west,[X,Y]):-
        X1 is X - 1,
        retract(environment:thief_location(_)),
        assert(environment:thief_location([X1,Y])).

move(north,[X,Y]):-
        Y1 is Y + 1,
        retract(environment:thief_location(_)),
        assert(environment:thief_location([X,Y1])).

move(south,[X,Y]):- 
        Y1 is Y - 1,
        retract(environment:thief_location(_)),
        assert(environment:thief_location([X,Y1])).
        
%-----------------------------------------
%           when agents meet
%-----------------------------------------
        
how_to_talk_to_victim(TW,TAUTIL):-
        TAUTIL > 0,
        thief_conversation_memory([]),
        want(TW).
        
how_to_talk_to_victim(_,TAUTIL):-
        thief_conversation_memory([_,VA]),
        VA == no,
        TAUTIL > 0,
        TAUTIL < 4,
        retract(game(_)),
        assert(game(over)),
		retractall(abandonment(_)),
		assert(abandonment(yes)),
		format("Thief capitulates.~n",[]),
        format("Game over, victim wins!~n",[]).

how_to_talk_to_victim(_,TAUTIL):-
        thief_conversation_memory([_,VA]),
        VA == yes,
        TAUTIL > 0,
        take,
        retract(game(_)),
        assert(game(over)),
        format("Game over, thief wins!~n",[]).
        
how_to_talk_to_victim(_,TAUTIL):-
        thief_conversation_memory([_,VA]),
        VA == no,
        TAUTIL > 3,
        thief_eloquence(TE),
        threat(TE).
                
how_to_talk_to_victim(_,TAUTIL):-
        thief_conversation_memory([_,_,_,VA]),
        VA == no,
        TAUTIL > 3,
        TAUTIL < 7,
        retract(game(_)),
        assert(game(over)),
		retractall(abandonment(_)),
		assert(abandonment(yes)),
		format("Thief capitulates.~n",[]),
        format("Game over, victim wins!~n",[]).

how_to_talk_to_victim(_,TAUTIL):-
        thief_conversation_memory([_,_,_,VA]),
        VA == yes,
        TAUTIL > 3,
        take,
        retract(game(_)),
        assert(game(over)),
        format("Game over, thief wins!~n",[]).
        
how_to_confront_victim(_,TAUTIL):-
        TAUTIL > 6,
        TAUTIL < 10,
        thief_conversation_memory([_,_,_,VA]),
        VA == no,
        environment:available_object(nothing),
        thief_weapon(TW),
        retract(environment:thief_handles(_)),
        assert(environment:thief_handles(TW)),
		format("Thief pulls out: ~p.~n",[TW]).
        
how_to_confront_victim(_,TAUTIL):-
        TAUTIL > 9,
        thief_conversation_memory([_,_,_,VA]),
        VA == no,
        environment:available_object(nothing),
        thief_weapon(TW),
        retract(environment:thief_handles(_)),
        assert(environment:thief_handles(TW)),
        assert(environment:thief_uses_force_against_victim),
		retract(game(_)),
        assert(game(over)),
		retractall(action(_)),
		assert(action(force)),
		format("Thief pulls out: ~p.~n",[TW]),
		format("Thief uses force against victim.~n",[]),
		format("&%$#@!@$@~n",[]),
        format("Game over, thief wins!~n",[]).
                
how_to_confront_victim(TW,_):-
        thief_conversation_memory([_,_,_,_,VA]),
        VA == yes,
        environment:available_object(TW),
        take,
        retract(game(_)),
        assert(game(over)),
        format("Game over, thief wins!~n",[]).
        
how_to_confront_victim(_,_):-
        thief_conversation_memory([_,_,_,_,VA]),
        VA == no,
        environment:available_object(nothing),
        retract(game(_)),
        assert(game(over)),
		retractall(abandonment(_)),
		assert(abandonment(yes)),
		format("Thief capitulates.~n",[]),
        format("Game over, victim wins!~n",[]).
        
take:-
        environment:available_object(TW),
        retract(thief_possession(_)),
        assert(thief_possession(TW)),
        retract(environment:available_object(_)),
        assert(environment:available_object(nothing)),
		retractall(property_taken(_)),
		assert(property_taken(yes)).
        
%-----------------------------------------
%           what thief has to say
%-----------------------------------------
		
want(phone):-
        retract(environment:thief_says(_)),
        assert(environment:thief_says(phone)),
        format("THIEF: Give me a phone.~n",[]),
        thief_remembers_conversation(phone).

want(money):-
        retract(environment:thief_says(_)),
        assert(environment:thief_says(money)),
        format("THIEF: Give me all your money.~n",[]),
        thief_remembers_conversation(money).
        
want(wallet):-
        retract(environment:thief_says(_)),
        assert(environment:thief_says(wallet)),
        format("THIEF: Give me your wallet.~n",[]),
        thief_remembers_conversation(wallet).
        
threat(1):-
        retract(environment:thief_says(_)),
        assert(environment:thief_says(assault)),
        format("THIEF: Or I'll assault you!~n",[]),
        thief_remembers_conversation(assault).

threat(2):-
        retract(environment:thief_says(_)),
        assert(environment:thief_says(kidnap)),
        format("THIEF: Or I'll kidnap you!~n",[]),
        thief_remembers_conversation(kidnap).
        
threat(3):-
        retract(environment:thief_says(_)),
        assert(environment:thief_says(forge)),
        format("THIEF: Or I'll forge you!~n",[]),
        thief_remembers_conversation(forge).
