
:- dynamic([
    health/3,
    agent_health/2
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


% initial_health(small_enemy, 100).
% initial_health(large_enemy, 100).

% health((2, 3), 70).

% get_health(Pos, Health) :-
%     health(Pos, Health),
%     !.
% get_health(Pos, Health) :-
%     world_position(Character, Pos),
%     initial_health(Character, Health),
%     assertz(health(Pos, Health)),
%     !.

% update_health(Pos, NewHealth) :-
%     get_health(Pos, OldHealth),
%     retractall(health(Pos, _)),
%     assertz(health(Pos, NewHealth)),
%     !.
