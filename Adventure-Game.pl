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
  write('election. You must collect all *INSERT NUM* votes to win.'),nl,
  nl,
  write('You control the game by using simple English commands'),nl,
  write('expressing the action you wish to take.  You can go to'),nl,
  write('other locations, look at your surroundings, look in things'),nl,
  write('take things, drop things, eat things, inventory the'),nl,
  write('things you have, and talk to people and give items to people.'),nl,
  nl,
  write('Hit any key to continue.'),get0(_),
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
  (nanifound; X == quit).

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
do(talk(X)):-talk(X),!.
do(eat(X)):-eat(X),!.
do(look):-look,!.
do(talk(X)):-talk(X),!.
do(look_in(X)):-look_in(X),!.
do(quit):-quit,!.

% These are the predicates which control exit from the game.  If
% the player has taken the nani, then the call to "(nani)" will
% succeed and the command_loop will complete.  Otherwise it fails
% and command_loop will repeat.

nanifound:-
  have(nani),        
  write('Congratulations, you saved the Nani.'),nl,
  write('Now you can rest secure.'),nl,nl.

quit:-
  write('Giving up?  It''s going to be a scary night'),nl,
  write('and when you get the Nani it''s not going'),nl,
  write('to smell right.'),nl,nl.

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
  nl,
  write('The examples are verbose, terser commands and synonyms'),nl,
  write('are usually accepted.'),nl,nl,
  write('Hit any key to continue.'),nl,
  get0(_),
  look.

hint:-
  write('You need to get to the cellar, and you can''t unless'),nl,
  write('you get some light.  You can''t turn on the cellar'),nl,
  write('light, but there is a flash light in the desk in the'),nl,
  write('office you might use.'),nl,nl,
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
place('dr rohrbaughs office').
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
  assertz(location(milkshake,union)),
  assertz(location('drumpf hat',sewers)),
  assertz(location('phillippy quiz','dr phillippy')),
  assertz(location('brady miller','back forty')),
  assertz(location('dr rohrbaugh desk','dr rohrbaughs office')),
  assertz(location('back hoe','construction site')),
  assertz(location('dr rohrbaugh','rohrbaughs office')),
  assertz(location('construction worker','construction site')),
  assertz(location('backhoe worker','back hoe')),
  assertz(location('dr phillippy',frey)),
  assertz(location('ping-pong players',fishbowl)),
  assertz(location('hobo','d lot')),
  assertz(location('dr miller',boyer)),
  assertz(location('lobby josh','naugle lobby')),
  assertz(have('small loan of a million dollars')),
  assertz(have('voter sheet')),
  assertz(here('entrance to campus')),
  assertz(josh_count(1)),
  dynamic(has_vote/1).

puzzleItem(milkshake).
puzzleItem('phillippy Quiz').
puzzleItem('brady Miller').

lookableObject('dr rohrbaugh desk').
lookableObject('back hoe').

starting('small loan of a million dollars').
starting('voter sheet').

character('dr rohrbaugh').
character('construction worker').
character('backhoe worker').
character('dr phillippy').
character('ping-pong players').
character('hobo').
character('dr miller').
character('lobby josh').

%%%%%%%% COMMANDS %%%%%%%%%%%%%%%%%%%%%%%%%%

% goto moves the player from room to room.

goto(Place):-
  can_go(Place),                 % check for legal move
  puzzle(goto(Place)),           % check for special conditions
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
look_in(Thing):-
  respond(['There is nothing in the ',Thing]).
  
% talk allows the player to talk to other characters

talk(Character):-
	is_present(Character),
	dialog(Character).
talk(Character):-
	is_present(Character),
	respond(['Their speech is incoherent. They should speak more english','']).
talk(Character):-
	respond(['That person is not here','']).
	
% dialog allows different characters to respond in different ways

dialog(Character):-
	has_vote(Character),
	respond(['You already have his vote! Why would you want to talk to him?']).
dialog(Character):-
	Character = 'dr rohrbaugh',
	location('drumpf hat', 'dr rohrbaugh'),
	asserta(has_vote('dr rohrbaugh')),
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



% take allows the player to take something.  As long as the thing is
% contained in the room it can be taken, even if the adventurer hasn't
% looked in the the container which contains it.  Also the thing
% must not be furniture.

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
	here(Here),
	contains(Person,Here).

contains(Thing,Here):-             % recursive definition to find
  location(Thing,Here).            % things contained in things etc.
contains(Thing,Here):-
  location(Thing,X),
  contains(X,Here).

is_takable(Thing):-                % you can't take the furniture
  lookableObject(Thing),
  respond(['You can''t pick up a ',Thing]),
  !,fail.
is_takable(_).                     % not furniture, ok to take

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
  
eat2(Thing):-
  edible(Thing),
  retract(have(Thing)),
  respond(['That ',Thing,' was good']).
eat2(Thing):-
  tastes_yuchy(Thing),
  respond(['Three year olds don''t eat ',Thing]).
eat2(Thing):-
  respond(['You can''t eat a ',Thing]).

% inventory list your possesions

inventory:-
  have(X),                         % make sure you have at least one thing
  write('You have: '),nl,
  list_possessions.
inventory:-
  write('You have nothing'),nl.

list_possessions:-
  have(X),
  tab(2),write(X),nl,
  fail.
list_possessions.


% The only special puzzle in Nani Search has to do with going to the
% cellar.  Puzzle is only called from goto for this reason.  Other
% puzzles pertaining to other commands could easily be added.

puzzle(goto(cellar)):-
  have(flashlight),
  turned_on(flashlight),!.
puzzle(goto(cellar)):-
  write('You can''t go to the cellar because it''s dark in the'),nl,
  write('cellar, and you''re afraid of the dark.'),nl,
  !,fail.
puzzle(_).

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
command([talk,Arg]) --> noun(person,Arg).

% Recognize three types of verbs.  Each verb corresponds to a command,
% but there are many synonyms allowed.  For example the command
% turn_on will be triggered by either "turn on" or "switch on".

verb(go_place,goto) --> go_verb.
verb(person,talk) --> talk_verb.
verb(thing,V) --> tran_verb(V).
verb(intran,V) --> intran_verb(V).

go_verb --> [go].
go_verb --> [go,to].
go_verb --> [g].

talk_verb --> [talk].
talk_verb --> [talk,to].
talk_verb --> [hi].
talk_verb --> [go,up,to].
talk_verb --> [say,hello,to].
talk_verb --> [greet].
talk_verb --> [hello].

tran_verb(give) --> [give].
tran_verb(take) --> [take].
tran_verb(take) --> [pick,up].
tran_verb(drop) --> [drop].
tran_verb(drop) --> [put].
tran_verb(drop) --> [put,down].
tran_verb(eat) --> [eat].
tran_verb(look_in) --> [look,in].
tram_verb(look_in) --> [look,at].
tran_verb(look_in) --> [look].
tran_verb(look_in) --> [open].

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
noun(go_place,'d lot') --> [d,lot].
noun(go_place,'entrance to campus') --> [entrance,to,campus].
noun(go_place,'rohrbaughs office') --> [rohrbaughs,office].
noun(go_place,'naugle lobby') --> [naugle, lobby].
noun(go_place,'starry field') --> [starry,field].
noun(go_place,'high center') --> [high,center].
noun(go_place,'engle center') --> [engle,center].
noun(go_place,'back forty') --> [back,forty].
noun(go_place,'starry athletic field') --> [starry,athletic,field].

noun(thing,T) --> [T], {location(T,_)}.
noun(thing,T) --> [T], {have(T)}.
noun(thing,'small loan of a million dollars') --> [small,loan,of,a,million,dollars].
noun(thing,'voter sheet') --> [voter,sheet].
noun(thing,'back hoe') --> [back,hoe].
noun(thing,'dr rohrbaughs desk') --> [dr,rohrbaughs,desk].
noun(thing,'drumpf hat') --> [drumpf,hat].

noun(person,P) --> [P], {character(P)}.
noun(person,'dr rohrbaugh') --> [dr,rohrbaugh].
noun(person,'lobby josh') --> [lobby,josh].

% If the player has just typed light, it can be interpreted three ways.
% If a room name is before it, it must be a room light.  If the
% player has the flash light, assume it means the flash light.  Otherwise
% assume it is the room light.

noun(thing,light) --> [X,light], {place(X)}.
noun(thing,flashlight) --> [light], {have(flashlight)}.
noun(thing,light) --> [light].

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
readword(C,W,C2) :-         % otherwise
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