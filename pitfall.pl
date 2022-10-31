% Cave size: 12x12
% Initial position: (1,1)
% Cave exit: (1,1)
% Agent's initial energy: 100
% Ammo damage: random between 20 and 50
% Ammo count: 5
% Enemies' initial energy: 100
% Energy filled by ower-ups: 20

% Actions
% -----
% 1. Move forward
% 2. Turn 90 deg clockwise
% 3. Pick up object (gold)
% 4. Shoot on the current facing direction (deals random damage between 20 and 50
%    to any enemy in the adjacent cell in the direction the agent is facing)
% 5. Climb out of the cave (only at the start position)

% Costs and rewards
% -----
% 1. Pick up: +1000
% 2. Falling in a pit: -1000
% 3. Getting killed by an enemy: -1000
% 4. Being attacked by an enemy: -{dammage}
% 5. Shooting: -10

% Sensors
% -----
% 1. Steps (adjacent cells to damage-inflicting enemies)
% 2. Breeze (adjacent cells to pits)
% 3. Flash (adjacent cells to teleporting enemies)
% 4. Glow (cells where gold is present)
% 5. Impact (when walking into a wall)
% 6. Scream (when an enemy dies)

:- dynamic([
    position/2,
    hit_wall/0,
    killed_enemy/0
]).

%
% World information
% -----------------

dir(north).
dir(east).
dir(south).
dir(west).

facing(east).

minX(1). maxX(12).
minY(1). maxY(12).

adjacent((X, Y1), (X, Y2), south) :-
    Y2 is Y1 - 1,
    valid_position((X, Y2)).
adjacent((X, Y1), (X, Y2), north) :-
    Y2 is Y1 + 1,
    valid_position((X, Y2)).
adjacent((X1, Y), (X2, Y), west) :-
    X2 is X1 - 1,
    valid_position((X2, Y)).
adjacent((X1, Y), (X2, Y), east) :-
    X2 is X1 + 1,
    valid_position((X2, Y)).

valid_position((X, Y)) :-
    minX(MinX), maxX(MaxX), between(MinX, MaxX, X),
    minY(MinY), maxY(MaxY), between(MinY, MaxY, Y).


% Cave elements
% -----
% 1. 2 two small enemies (damage 20)
% 2. 2 large enemies (damage 50)
% 3. 8 pits
% 4. 3 gold
% 5. 4 teletransporting enemies
% 6. 3 power ups
% 7. Walls around the whole cave

position(agent, (1, 1)).

position(small_enemy, (2, 10)).
position(small_enemy, (10, 2)).

position(large_enemy, (5, 6)).
position(large_enemy, (11, 8)).

position(pit, (2, 11)).
position(pit, (3, 3)).
position(pit, (4, 8)).
position(pit, (5, 2)).
position(pit, (7, 9)).
position(pit, (10, 6)).
position(pit, (10, 10)).
position(pit, (11, 3)).

position(gold, (3, 5)).
position(gold, (9, 11)).
position(gold, (11, 2)).

position(teletransporter, (1, 7)).
position(teletransporter, (4, 11)).
position(teletransporter, (7, 3)).
position(teletransporter, (10, 1)).

position(power_up, (1, 12)).
position(power_up, (2, 2)).
position(power_up, (7, 7)).


% Sensors

sense_environment(Sensors) :-
    Sensors = (Steps, Breeze, Flash, Glow, Impact, Scream),
    sense_steps(Steps),
    sense_breeze(Breeze),
    sense_flash(Flash),
    sense_glow(Glow),
    sense_impact(Impact),
    sense_scream(Scream),
    !.

% sense_steps/1
% steps if large or small enemy in adjacent cell
sense_steps(steps) :-
    position(agent, AP),
    (
        position(small_enemy, SEP),
        adjacent(AP, SEP, _)
    ;
        position(large_enemy, LEP),
        adjacent(AP, LEP, _)
    ).
sense_steps(no_steps).

% sense_breeze/1
% breeze if pit in adjacent cell
sense_breeze(breeze) :-
    position(agent, AP),
    position(pit, PP),
    adjacent(AP, PP, _).
sense_breeze(no_breeze).

% sense_flash/1
% flash if teletransporter in adjacent cell
sense_flash(flash) :-
    position(agent, AP),
    position(teletransporter, TP),
    adjacent(AP, TP, _).
sense_flash(no_flash).

% sense_glow/1
% glow if gold in agent's cell
sense_glow(glow) :-
    position(agent, AP),
    position(gold, AP).
sense_glow(no_glow).

% sense_impact/1
% impact if hit wall in previous step
% TODO
sense_impact(impact) :-
    hit_wall.
sense_impact(no_impact).

% sense_scream/1
% scream if killed enemy in previous step
% TODO
sense_scream(scream) :-
    killed_enemy.
sense_scream(no_scream).

update_position(NP) :-
    retractall(position(agent, _)),
    assertz(position(agent, NP)).

step :-
    sense_environment(Sensors),
    update_knowledge(Sensors),
    !.

update_knowledge(Sensors) :-
    Sensors = (Steps, Breeze, Flash, Glow, _, Scream),
    update_steps(Steps),
    update_breeze(Breeze),
    update_flash(Flash),
    update_glow(Glow),
    update_scream(Scream).

% update_steps/1
update_steps(Steps) :-
    position(agent, AP),
    assertz(certain(Steps, AP)),
    update_enemies(Steps).

% update_enemies/1
update_enemies(no_steps) :-
    position(agent, AP),
    assertz(certain(no_enemy, AP)),
    adjacent(AP, P, _),
    retractall(possible_position(small_enemy, P)),
    retractall(possible_position(large_enemy, P)),
    assertz(certain(no_enemy, P)),
    fail.
update_enemies(no_steps).

update_enemies(steps) :-
    update_small_enemies(steps),
    update_large_enemies(steps).
    % Take into account the maximum number of enemies and certainties to infer possibilities

update_small_enemies(steps) :-
    position(agent, AP),
    adjacent(AP, P, _),
    \+ certain(no_enemy, P),
    assertz(possible_position(small_enemy, P)),
    fail.
update_small_enemies(steps).

update_large_enemies(steps) :-
    position(agent, AP),
    adjacent(AP, P, _),
    \+ certain(no_enemy, P),
    assertz(possible_position(large_enemy, P)),
    fail.
update_large_enemies(steps).

% update breeze/1
update_breeze(Breeze) :-
    position(agent, AP),
    assertz(certain(Breeze, AP)),
    update_pits(Breeze).

update_pits(no_breeze) :-
    position(agent, AP),
    assertz(certain(no_pit, AP)),
    adjacent(AP, P, _),
    retractall(possible_position(pit, P)),
    assertz(certain(no_pit, P)),
    fail.
update_pits(no_breeze).

update_pits(_) :-
    position(agent, AP),
    adjacent(AP, P, _),
    \+ certain(no_pit, P),
    assertz(possible_position(pit, P)),
    fail.
update_pits(_).

% update flash/1
update_flash(Flash) :-
    position(agent, AP),
    assertz(certain(Flash, AP)),
    update_teleporter(Flash).

update_teleporter(no_flash) :-
    position(agent, AP),
    assertz(certain(no_teleporter, AP)),
    adjacent(AP, P, _),
    retractall(possible_position(teleporter, P)),
    assertz(certain(no_teleporter, P)),
    fail.
update_teleporter(no_flash).

update_teleporter(_) :-
    position(agent, AP),
    adjacent(AP, P, _),
    \+ certain(no_teleporter, P),
    assertz(possible_position(teleporter, P)),
    fail.
update_teleporter(_).

% update_gold/1
update_glow(Glow) :-
    position(agent, AP),
    assertz(certain(Glow, AP)),
    update_gold(Glow).

update_gold(glow) :-
    position(agent, AP),
    assertz(certain(gold, AP)).
update_gold(_) :-
    position(agent, AP),
    assertz(certain(no_gold, AP)).

% update_scream/1
update_scream(no_scream).
update_scream(scream) :-
    position(agent, AP),
    facing(Dir),
    adjacent(AP, P, Dir),
    retractall(possible_position(small_enemy, P)),
    retractall(possible_position(large_enemy, P)),
    retractall(possible_position(teleporter, P)),
    assertz(certain(no_enemy, P)).
    
