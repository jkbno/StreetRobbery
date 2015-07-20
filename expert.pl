:-module(expert,[

]).

:-use_module(thief).
:-use_module(victim).

robbery(common_subject).
theft(common_subject).
robbery(material_offence).
theft(material_offence).
robbery(intentional_offence).
theft(intentional_offence).
robbery(direct_object).
theft(direct_object).
robbery(socially_harmful).
theft(socially_harmful).

is_there_a_crime:-
	act_determinant,
	forbidden_act_determinant,
	illegal_forbidden_act_determinant,
	illegal_forbidden_harmful_act_determinant,
	guilty_illegal_forbidden_harmful_act_determinant.

guilty_illegal_forbidden_harmful_act_determinant:-
	thief:excuse(EXCUSE),
	member(EXCUSE, [insane, minor, vis_compulsiva, mistake, superior_order]),
	result(excuse).
	
guilty_illegal_forbidden_harmful_act_determinant:-
	thief:excuse(no).

guilty_illegal_forbidden_harmful_act_determinant:-
	thief:excuse(yes).
	
illegal_forbidden_harmful_act_determinant:-
	robbery(socially_harmful),
	theft(socially_harmful).
	
illegal_forbidden_act_determinant:-
	victim:justification(JUST),
	member(JUST, [self-defence, necessity, collision_of_duties, risk_of_innovation, consent]),
	result(justification).
	
illegal_forbidden_act_determinant:-
	victim:justification(no).
	
act_determinant:-
	thief:action(ACTION),
	member(ACTION, [vis_absoluta, reflex, lack_of_consciousness, state_excluding_physiological_action]),
	result(no_action).
	
act_determinant:-
	thief:action(move),
	result(action).
	
forbidden_act_determinant:-
	thief:action(force),
	result(force).
	
forbidden_act_determinant:-
	thief:abandonment(yes),
	result(abandonment).
	
forbidden_act_determinant:-
	robbery(common_subject),
	theft(common_subject),
	robbery(material_offence),
	theft(material_offence),
	robbery(intentional_offence),
	theft(intentional_offence),
	robbery(direct_object),
	theft(direct_object),
	thief:property_taken(yes),
	result(property_taken_yes).
		
forbidden_act_determinant:-
	robbery(common_subject),
	theft(common_subject),
	robbery(material_offence),
	theft(material_offence),
	robbery(intentional_offence),
	theft(intentional_offence),
	robbery(direct_object),
	theft(direct_object),
	thief:property_taken(no),
	result(property_taken_no).
		
result(action):-
	format("~n~nAction: 1~n",[]).
		
result(no_action):-
	format("~n~nAction: 0~n",[]).
	
result(abandonment):-
	format("Forbidden action: 0~n",[]),
	format("Illegal forbidden action: 0~n",[]),
	format("Illegal forbidden socially harmful action: 0~n",[]),
	format("Guilty illegal forbidden socially harmful action: 0~n",[]),
	format("There is no crime~n",[]).

result(property_taken_yes):-
	format("Forbidden action: 1~n",[]),
	format("Illegal forbidden action: 1~n",[]),
	format("Illegal forbidden socially harmful action: 1~n",[]),
	format("Guilty illegal forbidden socially harmful action: 1~n",[]),
	format("There is a crime~n",[]).
	
result(property_taken_no):-
	format("Forbidden action: 0~n",[]),
	format("Illegal forbidden action: 0~n",[]),
	format("Illegal forbidden socially harmful action: 0~n",[]),
	format("Guilty illegal forbidden socially harmful action: 0~n",[]),
	format("There is no crime~n",[]).

result(force):-
	format("Forbidden action: 1~n",[]),
	format("Illegal forbidden action: 1~n",[]),
	format("Illegal forbidden socially harmful action: 1~n",[]),
	format("Guilty illegal forbidden socially harmful action: 1~n",[]),
	format("There is a crime~n",[]).
	
result(justification):-
	format("Victim's justification: self-defence~n",[]).
