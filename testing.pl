
:- dynamic([
    health/3,
    agent_health/2,
    game_score/1
]).

world_position(small_enemy, (2, 9)).
world_position(small_enemy, (10, 2)).
world_position(small_enemy, (10, 9)).

world_position(large_enemy, (5, 6)).
world_position(large_enemy, (11, 8)).

%
% Health Tracking
% ------
% Agent's initial energy: 100
% Enemies' initial energy: 100
% Energy filled by power-ups: 20

% initial_health/2
% Initializes health for agent and enemies --> all 100 HP
initial_health(agent, 100).
initial_health(small_enemy, 100).
initial_health(large_enemy, 100).

% get_health/3
% Given a character (excluding agent) and their position on the map, get said character's health
% If character's health hasn't been tracked yet, initialize new health
get_health(Pos, Character, Health) :-
    health(Pos, Character, Health),
    !.
get_health(Pos, Character, Health) :-
    world_position(Character, Pos),
    initial_health(Character, Health),
    assertz(health(Pos, Character, Health)),
    !.

% update_health/3
% Given a character (excluding agent) and their position on the map, update said character's health
update_health(Pos, Character, NewHealth) :-
    get_health(Pos, Character, OldHealth),
    retractall(health(Pos, Character, _)),
    assertz(health(Pos, Character, NewHealth)),
    !.

% get_agent_health/2
% Get agent's health
% If agent's health hasn't been tracked yet, initialize new health
get_agent_health(Character, Health) :-
    agent_health(Character, Health),
    !.
get_agent_health(Character, Health) :-
    initial_health(Character, Health),
    assertz(agent_health(Character, Health)),
    !.

% update_agent_health/2
% Update agent's health
update_agent_health(Character, NewHealth) :-
    get_agent_health(Character, OldHealth),
    retractall(agent_health(Character, _)),
    assertz(agent_health(Character, NewHealth)),
    !.


% 
% Score System: Costs and Rewards
% ------
% 1. Pick up: +1000
% 2. Falling in a pit: -1000
% 3. Getting killed by an enemy: -1000
% 4. Being attacked by an enemy: -{dammage}
% 5. Shooting: -10

% initial_game_score/1
% Inicialize game score
initial_game_score(0).

% get_game_score/1
% Get the game's score
get_game_score(Score) :-
    game_score(Score),
    !.
get_game_score(Score) :-
    initial_game_score(Score),
    assertz(game_score(Score)),
    !.

% update_game_score/1
% Update the game's score
update_game_score(NewScore) :-
    get_game_score(OldScore),
    retractall(game_score(_)),
    assertz(game_score(NewScore)),
    !.

% pick_up/0
% Pick Up Reward -> +1000 points
pick_up_score :-
    get_game_score(OldScore),
    (NewScore is 1000+integer(OldScore)),
    update_game_score(NewScore).

% pit_fall/0
% Fall into Pit -> -1000 points
pit_fall_score :-
    get_game_score(OldScore),
    (NewScore is integer(OldScore)-1000),
    update_game_score(NewScore).

% killed/0
% Killed by enemy -> -1000 points
killed_score :-
    get_game_score(OldScore),
    (NewScore is integer(OldScore)-1000),
    update_game_score(NewScore).

% attacked/1
% Attacked by enemy -> -{damage}
attacked_score(Damage) :-
    get_game_score(OldScore),
    (NewScore is integer(OldScore)-integer(Damage)),
    update_game_score(NewScore).

% shooting/0
% Shooting an arrow -> -10 points
shooting_score :-
    get_game_score(OldScore),
    (NewScore is integer(OldScore)-10),
    update_game_score(NewScore).


%
% TODO: Attack System
% ----

% Ammo damage: random between 20 and 50
% Ammo count: 5
