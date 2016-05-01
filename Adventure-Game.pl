% DRUMPF WORLD - A simple adventure game
% in which the player must accumulate all
% the votes of his peers in order to still
% lose the election.

main:- drumpf_world.       % main entry point

drumpf_world:-
  init_dynamic_facts,     % predicates which are not compiled

  write('Drumpf World! - A simple adventure game.'),nl,
  nl,
  write('Your name is Ronald Drumpf.'),nl,
  write('You are a young pseudo-politician who is running'),nl,
  write('for class president of Messiah College in 2016.'),nl,
  write('You have nothing in your pocket but a small loan'),nl,
  write('of a million dollars and a voter sheet on which to'),nl,
  write('record your votes. Your mission is to collect'),nl,
  write('the votes of your peers in an effort to win the'),nl,
  write('election. You must collect all 11 votes to win.'),nl,
  nl,
  write('You control the game by using simple English commands'),nl,
  write('expressing the action you wish to take.  You can go to'),nl,
  write('other locations, look at your surroundings, look in things'),nl,
  write('take things, drop things, eat things, inventory the'),nl,
  write('things you have, and talk to people and give items to people.'),nl,
  nl,
  write('Hit enter to continue.'),get0(_),nl,
  write('Type "help" if you need more help on mechanics.'),nl,
  write('Type "hint" if you want a big hint.'),nl,
  write('Type "quit" if you give up.'),nl,
  nl,
  write('Go get those voters!'),nl,

  look,                   % give a look before starting the game
  command_loop.

% command_loop - repeats until either the nani is found or the
%     player types quit

command_loop:-
  repeat,
  get_command(X),
  do(X),
  (havevotesccc; havevotes; X == quit).

% do - matches the input command with the predicate which carries out
%     the command.  More general approaches which might work in the
%     listener are not supported in the compiler.  This approach
%     also gives tighter control over the allowable commands.

%     The cuts prevent the forced failure at the end of "command_loop"
%     from backtracking into the command predicates.

do(goto(X)):-goto(X),!.
do(nshelp):-nshelp,!.
do(hint):-hint,!.
do(inventory):-inventory,!.
do(take(X)):-take(X),!.
do(drop(X)):-drop(X),!.
do(give(X)):-give(X),!.
do(make(X)):-make(X),!.
do(talk(X)):-talk(X),!.
do(eat(X)):-eat(X),!.
do(look):-look,!.
do(talk(X)):-talk(X),!.
do(solve(X)):-solve(X),!.
do(make_speech):-make_speech,!.
do(look_in(X)):-look_in(X),!.
do(get_In(X)):-get_In(X),!.
do(quit):-quit,!.

% These are the predicates which control exit from the game.  If
% the player has taken the nani, then the call to "(nani)" will
% succeed and the command_loop will complete.  Otherwise it fails
% and command_loop will repeat.

havevotesccc:-
  checked_votes(X),        
  X =:= 11,
  have('ccc book'),
  location(milkshake,hobo),
  write('Congratulations, you have all the votes.'),nl,
  write('However, everyone lied about voting for you'),nl,
  write('and you still lose the election. Sorry!'),nl,nl,
  write('However, the words from the ccc book you found come back to you.'),nl,
  write('''Messiah College is a community and that''s what makes us special.'''),nl,
  write('You remember the warm feeling of giving a hobo a delicious milkshake.'),nl,
  write('You wonder if maybe there is more to life than just winning?'),nl,
  write('Maybe, there''s still hope for old Drumpf yet!'),nl,nl,
  write('You win!'),nl,nl,
  write('Thanks for playing!'),nl.

havevotes:-
  checked_votes(X),        
  X =:= 11,
  write('Congratulations, you have all the votes.'),nl,
  write('However, everyone lied about voting for you'),nl,
  write('and you still lose the election. Sorry!'),nl,nl,
  write('Thanks for playing!'),nl.
  
has_vote(Person):-
	asserta(checked_vote(Person)),
	checked_votes(X),
	retract(checked_votes(X)),
	NewX is X + 1,
	asserta(checked_votes(NewX)),!.

quit:-
  write('Giving up? A man who sells his own brand of steaks'),nl,
  write('would never give up!'),nl,
  nl.

% The help command

nshelp:-
  write('Use simple English sentences to enter commands.'),nl,
  write('The commands can cause you to:'),nl,
  nl,
  write('   go to a room          (ex. go to the office)'),nl,
  write('   look around           (ex. look)'),nl,
  write('   look in something     (ex. look in the desk)'),nl,
  write('   take something        (ex. take the apple)'),nl,
  write('   drop something        (ex. drop the apple)'),nl,
  write('   eat something         (ex. eat the apple)'),nl,
  write('   turn something on     (ex. turn on the light)'),nl,
  write('   inventory your things (ex. inventory)'),nl,
  write('   talk to someone		  (ex. talk to dr phillippy'),nl,
  write('   give to the person in the same room'),nl,
  write('		  (ex. give apple)'),nl,
  write('   make something        (ex. make a milkshake)'),nl,
  write('   solve something        (ex. solve phillippy quiz)'),nl,
  nl,
  write('The examples are verbose, terser commands and synonyms'),nl,
  write('are usually accepted.'),nl,nl,
  write('Hit any key to continue.'),nl,
  get0(_),
  look.

hint:-
  write('You are Ronald Drumpf You don''t need any hints!'),nl,
  nl,
  look.

% Initial facts describing the world.  Rooms and doors do not change,
% so they are compiled.

place(union).
place(frey).
place(fishbowl).
place('construction site').
place('d lot').
place('entrance to campus').
place(boyer).
place('rohrbaughs office').
place('naugle lobby').
place('starry athletic field').
place('high center').
place('engle center').
place('back forty').
place(lottie).
place(sewers).

path(union, 'naugle lobby').
path(union, fishbowl).
path(union, 'engle center').
path(union, frey).
path(union, 'construction site').
path(frey, 'engle center').
path(frey, boyer).
path(frey, 'rohrbaughs office').
path(frey, 'construction site').
path(frey, 'lottie').
path(fishbowl, 'd lot').
path(fishbowl, 'entrance to campus').
path(fishbowl, 'naugle lobby').
path('construction site', 'entrance to campus').
path('construction site', lottie).
path('d lot', 'entrance to campus').
path('d lot', 'back forty').
path(boyer, lottie).
path(boyer, 'high center').
path(boyer, 'starry athletic field').
path('rohrbaughs office', sewers).
path('starry athletic field', 'high center').
path('starry athletic field', 'back forty').
path('high center', lottie).
path('back forty', sewers).

connect(X,Y):-
  path(X,Y).
connect(X,Y):-
  path(Y,X).

% These facts are all subject to change during the game, so rather
% than being compiled, they are "asserted" to the listener at
% run time.  This predicate is called when "nanisrch" starts up.

init_dynamic_facts:-
  assertz(location('milkshake machine',union)),
  assertz(location('milkshake ingredients','milkshake machine')),
  assertz(location('drumpf hat',sewers)),
  assertz(location('brady miller','back forty')),
  assertz(location('dr rohrbaugh''s desk','rohrbaughs office')),
  assertz(location('ccc book','dr rohrbaugh''s desk')),
  assertz(location('Messiah College is a community and that''s what makes us special.','ccc book')),
  assertz(location('back hoe','construction site')),
  assertz(location('dr rohrbaugh','rohrbaughs office')),
  assertz(location('construction worker','construction site')),
  assertz(location('dr phillippy',frey)),
  assertz(location('ping pong players',fishbowl)),
  assertz(location('hobo','d lot')),
  assertz(location('dr miller',boyer)),
  assertz(location('lobby josh','naugle lobby')),
  assertz(location('p safety officer','entrance to campus')),
  assertz(location(yard,union)),
  assertz(location('white car','d lot')),
  assertz(location('red car','d lot')),
  assertz(location('blue car','d lot')),
  assertz(location('silver car','d lot')),
  assertz(location('lots of other cars','d lot')),
  assertz(location('man behind cars','red car')),
  assertz(location('jake and julie','union')),
  assertz(location(athlete,'starry athletic field')),
  assertz(location('construction plans','back hoe')),
  assertz(location('Step 1: Take as long as possible to finish construction. Step 2: Profit.','construction plans')),
  assertz(location('business student','high center')),
  assertz(location('marginally ok food','lottie')),
  assertz(location('campus nurse','engle center')),
  assertz(location('someone sketchy','lottie')),
  assertz(have('small loan of a million dollars')),
  assertz(have('voter sheet')),
  assertz(here('entrance to campus')),
  assertz(nurse_count(0)),
  assertz(sketch_count(0)),
  assertz(josh_count(1)),
  assertz(construction_count(0)),
  assertz(phillippy_count(0)),
  assertz(ping_count(0)),
  assertz(car_count(0)),
  assertz(speech_made(0)),
  assertz(checked_votes(0)),
  dynamic(quiz_solved/1),
  dynamic(checked_vote/1).

edible(milkshake).
edible('marginally ok food').

lookableObject('dr rohrbaugh''s desk').
lookableObject(yard).
lookableObject('back hoe').
lookableObject('milkshake machine').

driveableObject('back hoe').
driveableObject('white car').
driveableObject('red car').
driveableObject('blue car').
driveableObject('silver car').
driveableObject('lots of other cars').

character('dr rohrbaugh').
character('construction worker').
character('dr phillippy').
character('ping pong players').
character('hobo').
character('dr miller').
character('lobby josh').
character('man behind cars').
character('p safety officer').
character('jake and julie').
character('athlete').
character('business student').
character('campus nurse').
character('someone sketchy').

%%%%%%%% COMMANDS %%%%%%%%%%%%%%%%%%%%%%%%%%

% goto moves the player from room to room.

goto(Place):-
  can_go(Place),                 % check for legal move
  moveto(Place),                 % go there and tell the player
  look.
goto(_):- look.

can_go(Place):-                  % if there is a connection it 
  here(Here),                   % is a legal move.
  connect(Here,Place),!.
can_go(Place):-
  respond(['You can''t get to ',Place,' from here']),fail.

moveto(Place):-                  % update the logicbase with the
  retract(here(_)),             % new room
  asserta(here(Place)).

% look lists the things in a room, and the connections

look:-
  here(Here),
  respond(['You are at ',Here]),
  write('You can see the following things:'),nl,
  list_things(Here),
  write('You can go to the following places:'),nl,
  list_connections(Here).

list_things(yard):-
	have(milkshake),nl,
	tab(2),write('Somehow, all the boys have been brought'),nl,
	tab(2),write('to the yard. You didn'' have anything to do'),nl,
	tab(2),write('with this did you?'),nl,
	fail.
list_things(Place):-
  location(X,Place),
  tab(2),write(X),nl,
  fail.
list_things(_).

list_connections(Place):-
  connect(Place,X),
  tab(2),write(X),nl,
  fail.
list_connections(_).

% look_in allows the player to look inside a thing which might
% contain other things

look_in(Thing):-
  location(_,Thing),               % make sure there's at least one
  write('The '),write(Thing),write(' contains:'),nl,
  list_things(Thing).
look_in(yard):-
	have(milkshake),
	list_things(yard).
look_in(Thing):-
  respond(['There is nothing in the ',Thing]).
  
% talk allows the player to talk to other characters

talk('brady miller'):-
	have('brady miller'),
	respond(['You can''t see his face while carrying him, ',
	'but you hear muffled sobbing']).		
talk('brady miller'):-
	is_here('brady miller'),
	location('brady miller','dr miller'),
	respond(['He still looks so sad']).
talk('brady miller'):-
	is_here('brady miller'),
	respond(['He looks at you with sadness in his eyes. ',
		'You know he doesn''t want to go with you, but you''ve ',
		'got votes to win. You decide you should pick him up']).
talk(Character):-
	is_present(Character),
	dialog(Character).
talk(Character):-
	is_present(Character),
	respond(['Their speech is incoherent. They should speak more english']).
talk(Character):-
	character(Character),
	respond(['That person is not here']).
talk(Thing):-
	have(Thing),
	respond(['You hold it up to your face and whisper to it. ',
		'It makes no sudden movements... Or any movements... ',
		'Maybe it''s scared?']).
talk(_):-
	respond(['It doesn''t respond. Well... That means it''s either ',
		'deaf or it doesn''t like you... Probably because it''s a ',
		'lightweight loser']).
	
% dialog allows different characters to respond in different ways

dialog(Character):-
	checked_vote(Character),
	respond(['You already have his vote! Why would you want to talk to him?']).
dialog(Character):-
	Character = 'dr rohrbaugh',
	location('drumpf hat', 'dr rohrbaugh'),
	has_vote(Character),
	asserta(location('dr rohrbaugh''s vote', 'voter sheet')),
	respond(['His head is now less cold! He gives you his vote']).
dialog(Character):-
	Character = 'dr rohrbaugh',
	respond(['He mutters something about his head being cold']).
dialog(Character):-
	Character = 'lobby josh',
	josh_count(Num),
	Num = 3,
	retract(location('lobby josh','naugle lobby')),
	respond(['He became very annoyed and left']).
dialog(Character):-
	Character = 'lobby josh',
	josh_count(Num),
	Num = 1,
	NewNum is Num + 1,
	retract(josh_count(Num)),
	asserta(josh_count(NewNum)),
	respond(['He does not want to hear what you have to say, but you''re Ronald Drumpf!']),
	respond(['You''ve got words... You''ve got the best words! Keep trying.']).
dialog(Character):-
	Character = 'lobby josh',
	josh_count(Num),
	NewNum is Num + 1,
	retract(josh_count(Num)),
	asserta(josh_count(NewNum)),
	respond(['He does not want to hear what you have to say']).
dialog(Character):-
	Character = 'construction worker',
	construction_count(1),
	has_vote('construction worker'),
	asserta(location('construction worker''s vote','voter sheet')),
	respond(['You threatened to sue him. He scrawls his signature ',
		'on the voter sheet and shrinks away in fear. You''re glad ',
		'someone finally recognizes the sincerity of your threats']).
dialog(Character):-
	Character = 'construction worker',
	retract(construction_count(0)),
	asserta(construction_count(1)),
	respond(['Please sir! Don''t sue me! (talk again to threaten a law suit)']).
dialog(Character):-
	Character = 'ping pong players',
	ping_count(1),
	has_vote(Character),
	asserta(location('ping pong players'' votes','voter sheet')),
	respond(['Alright, man. Just make sure we get those paddles!']).
dialog(Character):-
	Character = 'ping pong players',
	retract(ping_count(0)),
	asserta(ping_count(1)),
	respond(['Hey, Drumpf. We''ll vote for you if you promise us better ',
		'ping pong paddles! (talk again to promise something that''s not ',
		'going to happen and you know it)']).
dialog(Character):-
	Character = 'dr phillippy',
	location('finished phillippy quiz', 'dr phillippy'),
	has_vote(Character),
	asserta(location('dr phillippy''s vote', 'voter sheet')),
	respond(['Wow, this looks great! Thanks, Drumpf. You have my vote']).
dialog(Character):-
	Character = 'dr phillippy',
	phillippy_count(1),
	location('phillippy quiz', 'dr phillippy'),
	move('phillippy quiz',have),
	respond(['Drumpf, this doesn''t look anywhere near close to done. ',
		'I''ll give it back to you and you can try again']).
dialog(Character):-
	Character = 'dr phillippy',
	have('finished phillippy quiz'),
	respond(['It looks like that quiz is finished! Give it to me so I ',
		'can make sure it''s correct']).
dialog(Character):-
	Character = 'dr phillippy',
	phillippy_count(1),
	have('phillippy quiz'),
	respond(['Get that quiz back to me as soon as possible']).
dialog(Character):-
	Character = 'dr phillippy',
	phillippy_count(0),
	asserta(have('phillippy quiz')),
	asserta(location('gibberish','phillippy quiz')),
	retract(phillippy_count(0)),
	asserta(phillippy_count(1)),
	respond(['Hi, Drumpf. Would you mind finishing this quiz for me? Thanks']).
dialog(Character):-
	Character = 'man behind cars',
	car_count(0),
	retract(car_count(0)),
	asserta(car_count(1)),
	retract(location('man behind cars','red car')),
	asserta(location('man behind cars','white car')),
	respond(['Please don''t report me to P-Safety! ',
		'(He runs and hides)']).
dialog(Character):-
	Character = 'man behind cars',
	car_count(1),
	retract(car_count(1)),
	asserta(car_count(2)),
	retract(location('man behind cars','white car')),
	asserta(location('man behind cars','blue car')),
	respond(['Please don''t report me to P-Safety! ',
		'(He runs and hides again)']).
dialog(Character):-
	Character = 'man behind cars',
	car_count(2),
	retract(car_count(2)),
	asserta(car_count(3)),
	retract(location('man behind cars','blue car')),
	asserta(location('man behind cars','red car')),
	respond(['C''mon man! Stop following me! ',
		'(He runs and hides again)']).
dialog(Character):-
	Character = 'man behind cars',
	car_count(3),
	retract(car_count(3)),
	asserta(car_count(4)),
	retract(location('man behind cars','red car')),
	asserta(location('man behind cars','silver car')),
	respond(['I''m serious, man! Stop it! ',
		'(He runs and hides again)']).
dialog(Character):-
	Character = 'man behind cars',
	car_count(4),
	retract(location('man behind cars',_)),
	has_vote(Character),
	asserta(location('man behind cars''s vote','voter sheet')),
	respond(['Alright, I''ll vote for you! Just leave me alone!']).
dialog(Character):-
	Character = 'p safety officer',
	sketch_count(1),
	has_vote(Character),
	asserta(location('p safety officer''s vote','voter sheet')),
	respond(['(You tell him about someone sketchy in lottie) ',
		'Alright, Drumpf! Thanks for letting me know. Anyone who is ',
		'as conscientious and responsible as you must be a good candidate! ',
		'You have my vote']).
dialog(Character):-
	Character = 'p safety officer',
	car_count(X),
	X =\= 0,
	has_vote(Character),
	asserta(location('p safety officer''s vote','voter sheet')),
	respond(['(You tell him about the man behind the cars in d lot) ',
		'Alright, Drumpf! Thanks for letting me know. Anyone who is ',
		'as conscientious and responsible as you must be a good candidate! ',
		'You have my vote']).
dialog(Character):-
	Character = 'p safety officer',
	car_count(0),
	respond(['Let me know if you see anything suspicious on campus!']).
dialog(Character):-
	Character = 'dr miller',
	location('brady miller','dr miller'),
	has_vote(Character),
	asserta(location('dr miller''s vote','voter sheet')),
	respond(['You''ve returned my son to me! Thank you so much! ',
		'You have my vote, Drumpf']).
dialog(Character):-
	Character = 'dr miller',
	have('brady miller'),
	respond(['I see you have my son with you. Maybe you could give ',
		'him back to me?']).
dialog(Character):-
	Character = 'dr miller',
	respond(['I''ve been looking all over for my son! Have you seen him?']).
dialog(Character):-
	Character = 'jake and julie',
	respond(['No! You must not associate with the competition!']).
dialog(Character):-
	Character = 'athlete',
	location('small loan of a million dollars', 'athlete'),
	has_vote(Character),
	asserta(location('athlete''s vote', 'voter sheet')),
	respond(['Oh, ok this will be just enough for me to vote for you.']),
	respond(['(You are pleased to have made such a smart business decision)']).
dialog(Character):-
	Character = 'athlete',
	respond(['It would take, like, a million dollars for me to vote for you! Hahahaha']).	
dialog(Character):-
	Character = 'business student',
	location('construction plans','business student'),
	has_vote(Character),
	asserta(location('business student''s vote','voter sheet')),
	respond(['Wow, I just looked through this, and it looks like the best money ',
		'making scheme ever! Thanks, Drumpf!']).
dialog(Character):-
	Character = 'business student',
	respond(['I''m looking for new business propositions! If you can find me some way ',
		'to make a lot of money without doing much work, I''ll vote for you! I hope ',
		'you can find a way. People seem to think you know what you''re doing, but I''m not so sure ',
		'You''re legacy mostly consists of making lower than average business decisions ',
		' but getting lucky in the long run, right? I mean, that time you started a mortgage ',
		'company six months before the housing bubble collapse is a classic. Anyway, let me know ',
		'if you find any ideas! (By ideas, I mean someone else''s idea. Even as a college ',
		'business major just starting out, I know enough to not trust yours.)']).
dialog(Character):-
	Character = 'campus nurse',
	location('forged psychological health verification','campus nurse'),
	has_vote(Character),
	asserta(location('campus nurse''s vote','voter sheet')),
	respond(['Well, despite my initial concern, everything appears to check out. I suppose ',
		'I''ll vote for you, then']).
dialog(Character):-
	Character = 'campus nurse',
	nurse_count(1),
	respond(['I need you to give me verification that you''re not crazy']).
dialog(Character):-
	Character = 'campus nurse',
	retract(nurse_count(0)),
	asserta(nurse_count(1)),
	respond(['My word, Drumpf! With the things you''ve been saying, I seriously doubt you ',
		'are of sound mind. I refuse to vote for you until you can verify your ',
		'psychological health!']).
dialog(Character):-
	Character = 'someone sketchy',
	nurse_count(1),
	has_vote(Character),
	asserta(location('someone sketchy''s vote','voter sheet')),
	asserta(have('forged psychological health verification')),
	respond(['(You ask him for psychological health verification)']),
	respond(['Alright, I can do that. I promise to vote. Here you go. (He slips you ',
		'a piece of paper)']).
dialog(Character):-
	Character = 'someone sketchy',
	sketch_count(1),
	respond(['Let me know what you need, man']).
dialog(Character):-
	Character = 'someone sketchy',
	retract(sketch_count(0)),
	asserta(sketch_count(1)),
	respond(['Hey man. I can get you whatever you need. Just let me know what it is. ',
		'There might even be a vote in it for you if you spread the word to those ',
		'who can be discrete, if you know what I mean. Talk to me again if you need ',
		'anything']).

% take allows the player to take something.  As long as the thing is
% contained in the room it can be taken, even if the adventurer hasn't
% looked in the the container which contains it.  Also the thing
% must not be furniture.

take('marginally ok food'):-
	asserta(have('marginally ok food')),
	respond(['You now have the marginally ok food']).
take('brady miller'):-
	is_here('brady miller'),
	move('brady miller',have),
	respond(['Somehow you manage to pick him up']).
take(Thing):-
	driveableObject(Thing),
	respond(['What, you''re just going to take it out for a spin?',
		' I don''t think so. It''s locked anyway']).
take(Thing):-
  is_here(Thing),
  is_takable(Thing),
  move(Thing,have),
  respond(['You now have the ',Thing]).

is_here(Thing):-
  here(Here),
  contains(Thing,Here),!.          % don't backtrack
is_here(Thing):-
  respond(['There is no ',Thing,' here']),
  fail.

is_present(Person):-
	character(Person),
	here(Here),
	contains(Person,Here).

contains(Thing,Here):-             % recursive definition to find
  location(Thing,Here).            % things contained in things etc.
contains(Thing,Here):-
  location(Thing,X),
  contains(X,Here).

is_takable(Thing):-                % you can't take things too big
  lookableObject(Thing),
  respond(['That ',Thing, ' is yuuuuuugggeee!',
  	' There''s no way you could take it']),
  !,fail.
is_takable(Person):-			   % can't take people either
  character(Person),
  respond(['You can''t take a person. Maybe you can deport them']),
  !,fail.
is_takable(_).                     % not too big or person, ok to take

move(Thing,have):-
  retract(location(Thing,_)),      % take it from its old place
  asserta(have(Thing)).            % and add to your possessions

% drop - allows the player to transfer a possession to a room

drop(Thing):-
  have(Thing),                     % you must have the thing to drop it
  here(Here),                      % where are we
  retract(have(Thing)),
  asserta(location(Thing,Here)),
  respond(['You have dropped the ',Thing]).
drop(Thing):-
  respond(['You don''t have the ',Thing]).

% give - allows the player to give an item to the character in the room
give('milkshake'):-
	have('milkshake'),
	is_present('hobo'),
	retract(have('milkshake')),
	asserta(location('milkshake','hobo')),
	respond(['You gave the milkshake to the hobo.']),
	respond(['He thanks you.']),
	respond(['You feel an odd warm sensation inside.']).
give(Thing):-
	have(Thing),
	is_present(Person),
	retract(have(Thing)),
	asserta(location(Thing,Person)),
	respond(['You gave the ',Thing,' to ',Person]).
give(Thing):-
	have(Thing),
	respond(['Nobody is here']).
give(Thing):-
	respond(['You don''t have the ',Thing]).

% eat, because every adventure game lets you eat stuff.

eat(Thing):-
  have(Thing),
  eat2(Thing).
eat(Thing):-
  respond(['You don''t have the ',Thing]).

eat2(Food):-
	Food = 'marginally ok food',
	retract(have(Food)),
	respond(['That ',Food,' was marginally ok']).
eat2(Thing):-
  edible(Thing),
  retract(have(Thing)),
  respond(['That ',Thing,' was great. It was so great.',
	' I mean it was just incredible. It was the greatest ',Thing]).
eat2(Person):-
	character(Person),
	respond(['I don''t think they''d appreciate that.']).
eat2(Thing):-
  respond(['You can''t eat a ',Thing]).
  
% make - use this to make milkshakes in the game!

make(milkshake):-
	here(union),
	have('milkshake ingredients'),
	asserta(have(milkshake)),
	respond(['Congrats! You now have a milkshake. ',
	'You hear faint pop music in the background']).
make(milkshake):-
	here(union),
	respond(['You don''t have the required ingredients.']).
make(milkshake):-
	respond(['How are you supposed to do that?']).
make(_):-
	here(Here),
	location(Character,Here),
	character(Character),
	tab(2),write('That might cost a lot of money.'),nl,
	tab(2),write('You could try to make '),
	write(Character),
	write(' pay for it, '),nl,
	tab(2),write('but that probably won''t work.'),nl.
make(_):-
	write('That might cost a lot of money, '),nl,
	write('and there''s no one around to pay for it. '),nl,
	write('It''s certainly not coming out of your pocket!'),nl.

% get_in - you can't get into a car.. you're too big for anything else

get_In(Thing):-
	driveableObject(Thing),
	respond(['What? You''re just going to take it out for a spin?',
		' I don''t think so. It''s locked anyway']).
get_In(Person):-
	character(Person),
	respond(['I don''t think we''re allowed to implement that..']).
get_In(Thing):-
	respond(['I think you''re too big to fit in a ',Thing]).

% solve the quiz (make something up)

solve('phillippy quiz'):-
	retract(have('phillippy quiz')),
	asserta(have('finished phillippy quiz')),
	asserta(location('even worse gibberish','finished phillippy quiz')),
	respond(['You make something up and write it down']).
solve(_):-
	respond(['What''s there to solve?']).

% inventory list your possesions

inventory:-
  have(_),                         % make sure you have at least one thing
  write('You have: '),nl,
  list_possessions.
inventory:-
  write('You have nothing'),nl.

list_possessions:-
  have(X),
  tab(2),write(X),nl,
  fail.
list_possessions.

% The final speech is the last step of the game.

make_speech:-
	here(union),
	full_voter_sheet(1),
	make_speech2.
make_speech2:-
	respond(['some words']).

% respond simplifies writing a mixture of literals and variables
 
respond([]):-
  write('.'),nl,nl.
respond([H|T]):-
  write(H),
  respond(T).

% Simple English command listener.  It does some semantic checking
% and allows for various synonyms.  Within a restricted subset of
% English, a command can be phrased many ways.  Also non grammatical
% constructs are understood, for example just giving a room name
% is interpreted as the command to goto that room.

% Some interpretation is based on the situation.  Notice that when
% the player says turn on the light it is ambiguous.  It could mean
% the room light (which can't be turned on in the game) or the
% flash light.  If the player has the flash light it is interpreted
% as flash light, otherwise it is interpreted as room light.

get_command(C):-
  readlist(L),        % reads a sentence and puts [it,in,list,form]
  command(X,L,[]),    % call the grammar for command
  C =.. X,!.          % make the command list a structure
get_command(_):-
  respond(['I don''t understand, try again or type help']),fail.

% The grammar doesn't have to be real English.  There are two
% types of commands in Nani Search, those with and without a 
% single argument.  A special case is also made for the command
% goto which can be activated by simply giving a room name.

command([Pred,Arg]) --> verb(Type,Pred),nounphrase(Type,Arg).
command([Pred]) --> verb(intran,Pred).
command([goto,Arg]) --> noun(go_place,Arg).

% Recognize three types of verbs.  Each verb corresponds to a command,
% but there are many synonyms allowed.  For example the command
% turn_on will be triggered by either "turn on" or "switch on".

verb(go_place,goto) --> go_verb.
verb(person,V) --> tran_verb(V).
verb(thing,V) --> tran_verb(V).
verb(intran,V) --> intran_verb(V).

go_verb --> [go].
go_verb --> [go,to].
go_verb --> [go,into].
go_verb --> [walk,to].
go_verb --> [run,to].
go_verb --> [g].

tran_verb(talk) --> [talk].
tran_verb(talk) --> [talk,to].
tran_verb(talk) --> [hi].
tran_verb(talk) --> [go,up,to].
tran_verb(talk) --> [say,hello,to].
tran_verb(talk) --> [greet].
tran_verb(talk) --> [hello].
tran_verb(talk) --> [say,hi,to].
tran_verb(get_In) --> [get,in].
tran_verb(get_In) --> [get,inside].
tran_verb(get_In) --> [get,into].
tran_verb(get_In) --> [jump,in].
tran_verb(get_In) --> [jump,into].
tran_verb(get_In) --> [drive].
tran_verb(make) --> [make].
tran_verb(solve) --> [solve].
tran_verb(solve) --> [finish].
tran_verb(give) --> [give].
tran_verb(take) --> [take].
tran_verb(take) --> [pick,up].
tran_verb(drop) --> [drop].
tran_verb(drop) --> [put].
tran_verb(drop) --> [put,down].
tran_verb(eat) --> [eat].
tran_verb(look_in) --> [look,in].
tran_verb(look_in) --> [look,at].
tran_verb(look_in) --> [look].
tran_verb(look_in) --> [check].
tran_verb(look_in) --> [review].
tran_verb(look_in) --> [read].
tran_verb(look_in) --> [open].

intran_verb(make_speech) --> [make,speech].
intran_verb(make_speech) --> [give,speech].
intran_verb(make_speech) --> [speech].
intran_verb(inventory) --> [inventory].
intran_verb(inventory) --> [i].
intran_verb(look) --> [look].
intran_verb(look) --> [look,around].
intran_verb(look) --> [l].
intran_verb(quit) --> [quit].
intran_verb(quit) --> [exit].
intran_verb(quit) --> [end].
intran_verb(quit) --> [bye].
intran_verb(nshelp) --> [help].
intran_verb(hint) --> [hint].

% a noun phrase is just a noun with an optional determiner in front.

nounphrase(Type,Noun) --> det,noun(Type,Noun).
nounphrase(Type,Noun) --> noun(Type,Noun).

det --> [the].
det --> [a].

% Nouns are defined as rooms, or things located somewhere.  We define
% special cases for those things represented in Nani Search by two
% words.  We can't expect the user to type the name in quotes.

noun(go_place,R) --> [R], {place(R)}.
noun(go_place,'construction site') --> [construction,site].
noun(go_place,'construction site') --> [construction].
noun(go_place,'construction site') --> [site].
noun(go_place,'d lot') --> [d,lot].
noun(go_place,'entrance to campus') --> [entrance,to,campus].
noun(go_place,'entrance to campus') --> [entrance].
noun(go_place,'rohrbaughs office') --> [rohrbaughs,office].
noun(go_place,'rohrbaughs office') --> [office].
noun(go_place,'naugle lobby') --> [naugle, lobby].
noun(go_place,'naugle lobby') --> [naugle].
noun(go_place,'naugle lobby') --> [lobby].
noun(go_place,'high center') --> [high,center].
noun(go_place,'engle center') --> [engle,center].
noun(go_place,'back forty') --> [back,forty].
noun(go_place,'starry athletic field') --> [starry,athletic,field].
noun(go_place,'starry athletic field') --> [starry,field].
noun(go_place,'starry athletic field') --> [starry].

noun(thing,T) --> [T], {location(T,_)}.
noun(thing,T) --> [T], {have(T)}.
noun(thing,'small loan of a million dollars') --> [small,loan,of,a,million,dollars].
noun(thing,'small loan of a million dollars') --> [small,loan].
noun(thing,'small loan of a million dollars') --> [loan].
noun(thing,'small loan of a million dollars') --> [a,million,dollars].
noun(thing,'small loan of a million dollars') --> [million,dollars].
noun(thing,'small loan of a million dollars') --> [dollars].
noun(thing,'voter sheet') --> [voter,sheet].
noun(thing,'voter sheet') --> [sheet].
noun(thing,'back hoe') --> [back,hoe].
noun(thing,'dr rohrbaugh''s desk') --> [dr,rohrbaughs,desk].
noun(thing,'dr rohrbaugh''s desk') --> [desk].
noun(thing,'drumpf hat') --> [drumpf,hat].
noun(thing,'drumpf hat') --> [hat].
noun(thing,'milkshake machine') --> [milkshake,machine].
noun(thing,'milkshake machine') --> [machine].
noun(thing,'milkshake') --> [milkshake].
noun(thing,'milkshake ingredients') --> [milkshake,ingredients].
noun(thing,'milkshake ingredients') --> [ingredients].
noun(thing,'phillippy quiz') --> [phillippy,quiz].
noun(thing,'phillippy quiz') --> [quiz].
noun(thing,'finished phillippy quiz') --> [finished,phillippy,quiz].
noun(thing,'brady miller') --> [brady,miller].
noun(thing,'brady miller') --> [brady].
noun(thing,'white car') --> [white,car].
noun(thing,'red car') --> [red,car].
noun(thing,'blue car') --> [blue,car].
noun(thing,'silver car') --> [silver,car].
noun(thing,'lots of other cars') --> [lots,of,other,cars].
noun(thing,'construction plans') --> [construction,plans].
noun(thing,'marginally ok food') --> [marginally,ok,food].
noun(thing,'marginally ok food') --> [food].
noun(thing,'construction plans') --> [construction,plans].
noun(thing,'construction plans') --> [plans].
noun(thing,'forged psychological health verification') --> [forged,psychological,health,verification].
noun(thing,'forged psychological health verification') --> [health,verification].
noun(thing,'forged psychological health verification') --> [verification].
noun(thing,'ccc book') --> [ccc,book].
noun(thing,'ccc book') --> [book].


noun(person,P) --> [P], {character(P)}.
noun(person,'dr rohrbaugh') --> [dr,rohrbaugh].
noun(person,'dr rohrbaugh') --> [rohrbaugh].
noun(person,'lobby josh') --> [lobby,josh].
noun(person,'lobby josh') --> [josh].
noun(person,'construction worker') --> [construction,worker].
noun(person,'construction worker') --> [worker].
noun(person,'dr phillippy') --> [dr,phillippy].
noun(person,'dr phillippy') --> [phillippy].
noun(person,'ping pong players') --> [ping,pong,players].
noun(person,'ping pong players') --> [players].
noun(person,'dr miller') --> [dr,miller].
noun(person,'dr miller') --> [miller].
noun(person,'p safety officer') --> [p,safety,officer].
noun(person,'p safety officer') --> [officer].
noun(person,'man behind cars') --> [man,behind,cars].
noun(person,'man behind cars') --> [man].
noun(person,'jake and julie') --> [jake,and,julie].
noun(person,'athlete') --> [athlete].
noun(person,'someone sketchy') --> [someone,sketchy].
noun(person,'someone sketchy') --> [someone].
noun(person,'campus nurse') --> [campus,nurse].
noun(person,'campus nurse') --> [nurse].
noun(person,'business student') --> [business,student].
noun(person,'business student') --> [student].

% readlist - read a list of words, based on a Clocksin & Mellish
% example.

readlist(L):-
  write('> '),
  read_word_list(L).

read_word_list([W|Ws]) :-
  get0(C),
  readword(C, W, C1),       % Read word starting with C, C1 is first new
  restsent(C1, Ws), !.      % character - use it to get rest of sentence

restsent(C,[]) :- lastword(C), !. % Nothing left if hit last-word marker
restsent(C,[W1|Ws]) :-
  readword(C,W1,C1),        % Else read next word and rest of sentence
  restsent(C1,Ws).

readword(C,W,C1) :-         % Some words are single characters
  single_char(C),           % i.e. punctuation
  !, 
  name(W, [C]),             % get as an atom
  get0(C1).
readword(C, W, C1) :-
  is_num(C),                % if we have a number --
  !,
  number_word(C, W, C1, _). % convert it to a genuine number
readword(C,W,C2) :-         % otherwise if character does not
  in_word(C, NewC),         % delineate end of word - keep
  get0(C1),                 % accumulating them until 
  restword(C1,Cs,C2),       % we have all the word     
  name(W, [NewC|Cs]).       % then make it an atom
readword(_,W,C2) :-         % otherwise
  get0(C1),       
  readword(C1,W,C2).        % start a new word

restword(C, [NewC|Cs], C2) :-
  in_word(C, NewC),
  get0(C1),
  restword(C1, Cs, C2).
restword(C, [], C).


single_char(0',).
single_char(0';).
single_char(0':).
single_char(0'?).
single_char(0'!).
single_char(0'.).


in_word(C, C) :- C >= 0'a, C =< 0'z.
in_word(C, L) :- C >= 0'A, C =< 0'Z, L is C + 32.
in_word(0'',0'').
in_word(0'-,0'-).

% Have character C (known integer) - keep reading integers and build
% up the number until we hit a non-integer. Return this in C1,
% and return the computed number in W.

number_word(C, W, C1, Pow10) :- 
  is_num(C),
  !,
  get0(C2),
  number_word(C2, W1, C1, P10),
  Pow10 is P10 * 10,
  W is integer(((C - 0'0) * Pow10) + W1).
number_word(C, 0, C, 0.1).


is_num(C) :-
  C =< 0'9,
  C >= 0'0.

% These symbols delineate end of sentence

lastword(10).   % end if new line entered
lastword(0'.).
lastword(0'!).
lastword(0'?).