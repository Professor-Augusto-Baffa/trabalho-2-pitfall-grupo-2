% Cave size: 12x12
% Initial position: (1,1)
% Cave exit: (1,1)
% Agent's initial energy: 100
% Ammo damage: random between 20 and 50
% Ammo count: 5
% Enemies' initial energy: 100
% Energy filled by ower-ups: 20

% Costs and rewards
% -----
% 1. Pick up: +1000
% 2. Falling in a pit: -1000
% 3. Getting killed by an enemy: -1000
% 4. Being attacked by an enemy: -{dammage}
% 5. Shooting: -10

:- dynamic([
    world_position/2,
    agent_position/1,
    facing/1,
    hit_wall/0,
    killed_enemy/0,
    goal/1,
    certain/2
]).

agent_position((1,1)).

%
% World information
% -----------------

facing(east).

dir(north).
dir(east).
dir(south).
dir(west).

clockwise(north, east).
clockwise(east, south).
clockwise(south, west).
clockwise(west, north).

% adjacent/2
% adjacent(+Pos, ?NextPos, ?Direction)
adjacent((X, Y1), (X, Y2), south) :-
    Y2 is Y1 - 1.
adjacent((X, Y1), (X, Y2), north) :-
    Y2 is Y1 + 1.
adjacent((X1, Y), (X2, Y), west) :-
    X2 is X1 - 1.
adjacent((X1, Y), (X2, Y), east) :-
    X2 is X1 + 1.

minX(1). maxX(12).
minY(1). maxY(12).

% valid_position/1
% valid_position(+Pos)
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

% world_position/2
% world_position contains absolute world knowledge and should not be queried for decision making

world_position(agent, (1, 1)).

world_position(small_enemy, (2, 9)).
world_position(small_enemy, (10, 2)).

world_position(large_enemy, (5, 6)).
world_position(large_enemy, (11, 8)).

world_position(pit, (2, 11)).
world_position(pit, (3, 3)).
world_position(pit, (4, 8)).
world_position(pit, (5, 2)).
world_position(pit, (7, 9)).
world_position(pit, (10, 6)).
world_position(pit, (10, 10)).
world_position(pit, (11, 3)).

world_position(gold, (3, 5)).
world_position(gold, (9, 11)).
world_position(gold, (11, 2)).

world_position(teletransporter, (1, 7)).
world_position(teletransporter, (4, 11)).
world_position(teletransporter, (7, 3)).
world_position(teletransporter, (10, 1)).

world_position(power_up, (1, 12)).
world_position(power_up, (2, 2)).
world_position(power_up, (7, 7)).

% Print cave for debugging

print_cave :-
    minY(MinY), maxY(MaxY),
    between(MinY, MaxY, Y),
    Line is MaxY - Y + 1,
    print_cave_line(Line),
    fail.
print_cave.
print_cave_line(Y) :-
    minX(MinX), maxX(MaxX),
    between(MinX, MaxX, X),
    print_cave_cell(X, Y),
    write(' '),
    fail.
print_cave_line(_) :- nl.
print_cave_cell(X, Y) :-
    world_position(Element, (X, Y)),
    print_cave_element(Element),
    !.
print_cave_cell(_, _) :-
    print_cave_element(empty).

print_cave_element(empty) :- write('.').
print_cave_element(agent) :- write('X').
print_cave_element(small_enemy) :- write('d').
print_cave_element(large_enemy) :- write('D').
print_cave_element(pit) :- write('P').
print_cave_element(gold) :- write('O').
print_cave_element(teletransporter) :- write('T').
print_cave_element(power_up) :- write('U').

%
% Observation and decision making
% ----

% sense_learn_act/0
% Learns from the environment and acts accordingly
sense_learn_act :-
    sense_environment(Sensors),
    clear_transient_flags,
    update_knowledge(Sensors),
    update_goal,
    next_action(Action),
    perform_action(Action),
    !.


% Sensors
% -----
% 1. Steps (adjacent cells to damage-inflicting enemies)
% 2. Breeze (adjacent cells to pits)
% 3. Flash (adjacent cells to teleporting enemies)
% 4. Glow (cells where gold is present)
% 5. Impact (when walking into a wall)
% 6. Scream (when an enemy dies)

% sense_environment/1
% sense_environment(-Sensors)
% Sensing should use absolute world knowledge because it does not depend on
% where the agent thinks it is, but on where it actually is
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
% sense_steps(-Steps)
% steps if large or small enemy in adjacent cell
sense_steps(steps) :-
    world_position(agent, AP),
    (
        world_position(small_enemy, SEP),
        adjacent(AP, SEP, _),
        valid_position(SEP)
    ;
        world_position(large_enemy, LEP),
        adjacent(AP, LEP, _),
        valid_position(LEP)
    ).
sense_steps(no_steps).

% sense_breeze/1
% sense_breeze(-Breeze)
% breeze if pit in adjacent cell
sense_breeze(breeze) :-
    world_position(agent, AP),
    world_position(pit, PP),
    adjacent(AP, PP, _),
    valid_position(PP). %%%%
sense_breeze(no_breeze).

% sense_flash/1
% sense_flash(-Flash)
% flash if teletransporter in adjacent cell
sense_flash(flash) :-
    world_position(agent, AP),
    world_position(teletransporter, TP),
    adjacent(AP, TP, _),
    valid_position(TP). %%%%
sense_flash(no_flash).

% sense_glow/1
% sense_glow(-Glow)
% glow if gold in agent's cell
sense_glow(glow) :-
    world_position(agent, AP),
    world_position(gold, AP).
sense_glow(no_glow).

% sense_impact/1
% sense_impact(-Impact)
% impact if hit wall in previous step
sense_impact(impact) :-
    hit_wall.
sense_impact(no_impact).

% sense_scream/1
% sense_scream(-Scream)
% scream if killed enemy in previous step
% TODO
sense_scream(scream) :-
    killed_enemy.
sense_scream(no_scream).


% For testing only
force_update_position(NP) :-
    retractall(world_position(agent, _)),
    retractall(agent_position(_)),
    assertz(world_position(agent, NP)),
    assertz(agent_position(NP)).

world_step :-
    % If valid step, update position
    world_position(agent, AP),
    facing(Direction),
    adjacent(AP, NP, Direction),
    valid_position(NP),
    retractall(world_position(agent, _)),
    assertz(world_position(agent, NP)),
    !.
world_step :-
    % If invalid step, hit wall
    assertz(hit_wall).


clear_transient_flags :-
    retractall(hit_wall),
    retractall(killed_enemy).


%
% Update knowledge
% ------
% Updating knowledge should not use absolute world position, but agent perception,
% as it should reflect where the agent thinks it is

% update_knowledge/1
% update_knowledge(+Sensors)
update_knowledge(Sensors) :-
    Sensors = (Steps, Breeze, Flash, Glow, Impact, Scream),
    % Update impact first in case the wall was hit in the previous step
    update_impact(Impact),
    update_steps(Steps),
    update_breeze(Breeze),
    update_flash(Flash),
    update_glow(Glow),
    update_scream(Scream).

% update_steps/1
% update_steps(+Steps)
update_steps(Steps) :-
    agent_position(AP),
    assertz(certain(Steps, AP)),
    update_enemies(Steps).

% update_enemies/1
% update_enemies(+Steps)
update_enemies(no_steps) :-
    agent_position(AP),
    learn(no_enemy, AP),
    adjacent(AP, P, _), % Not checking valid_position because it is world knowledge
    learn(no_enemy, P),
    fail.
update_enemies(no_steps).

update_enemies(steps) :-
    agent_position(AP),
    adjacent(AP, P, _), % Not checking valid_position because it is world knowledge
    \+ certain(no_enemy, P),
    assertz(possible_position(enemy, P)),
    fail.
update_enemies(steps).

% update_breeze/1
% update_breeze(+Breeze)
update_breeze(Breeze) :-
    agent_position(AP),
    assertz(certain(Breeze, AP)),
    update_pits(Breeze).

% update_pits/1
% update_pits(+Breeze)
update_pits(no_breeze) :-
    agent_position(AP),
    learn(no_pit, AP),
    adjacent(AP, P, _), % Not checking valid_position because it is world knowledge
    learn(no_pit, P),
    fail.
update_pits(no_breeze).

update_pits(breeze) :-
    agent_position(AP),
    adjacent(AP, P, _), % Not checking valid_position because it is world knowledge
    \+ certain(no_pit, P),
    assertz(possible_position(pit, P)),
    fail.
update_pits(breeze).

% update_flash/1
% update_flash(+Flash)
update_flash(Flash) :-
    agent_position(AP),
    assertz(certain(Flash, AP)),
    update_teleporter(Flash).

% update_teleporter/1
% update_teleporter(+Flash)
update_teleporter(no_flash) :-
    agent_position(AP),
    assertz(certain(no_teleporter, AP)),
    adjacent(AP, P, _), % Not checking valid_position because it is world knowledge
    learn(no_teleporter, P),
    fail.
update_teleporter(no_flash) :- !.

update_teleporter(flash) :-
    agent_position(AP),
    adjacent(AP, P, _), % Not checking valid_position because it is world knowledge
    \+ certain(no_teleporter, P),
    assertz(possible_position(teleporter, P)),
    fail.
update_teleporter(flash).

% update_glow/1
% update_glow(+Glow)
update_glow(Glow) :-
    agent_position(AP),
    assertz(certain(Glow, AP)),
    update_gold(Glow).

% update_gold/1
% update_gold(+Glow)
update_gold(glow) :-
    agent_position(AP),
    assertz(certain(gold, AP)).
update_gold(no_glow) :-
    agent_position(AP),
    assertz(certain(no_gold, AP)).

% update_impact/1
% update_impact(+Impact)
update_impact(no_impact).
update_impact(impact) :-
    % If impact, a wall was hit on last move, so "go back" to fix the agent_position
    agent_position(AP),
    facing(Direction),
    % Turn twice to get the backwards direction
    clockwise(Direction, D),
    clockwise(D, Backwards),
    adjacent(AP, PrevPos, Backwards),
    retractall(agent_position(_)),
    assertz(agent_position(PrevPos)),
    % Learn cave bounds
    learn_cave_bounds.

% update_scream/1
% update_scream(+Scream)
update_scream(no_scream).
update_scream(scream) :-
    agent_position(AP),
    facing(Dir),
    adjacent(AP, P, Dir), % Not checking valid_position because it is world knowledge
    learn(no_enemy, P),
    learn(no_teleporter, P).

% learn/2
% learn(+Item, +Location)
learn(no_enemy, P) :-
    retractall(possible_position(enemy, P)),
    assertz(certain(no_enemy, P)).
learn(enemy, P) :-
    retractall(possible_position(_, P)),
    assertz(certain(enemy, P)).
learn(no_teleporter, P) :-
    retractall(possible_position(teleporter, P)),
    assertz(certain(no_teleporter, P)).
learn(teleporter, P) :-
    retractall(possible_position(_, P)),
    assertz(certain(teleporter, P)).
learn(no_pit, P) :-
    retractall(possible_position(pit, P)),
    assertz(certain(no_pit, P)).
learn(pit, P) :-
    retractall(possible_position(_, P)),
    assertz(certain(pit, P)).


learn_cave_bounds :-
    facing(east),
    % Hit wall when walking east, so learn maxX
    agent_position((MaxX, _)),
    assertz(certain(maxX, MaxX)),
    review_gt_max_x_assumptions,
    !.
learn_cave_bounds :-
    facing(west),
    % Hit wall when walking west, so learn minX
    agent_position((MinX, _)),
    assertz(certain(minX, MinX)),
    review_lt_min_x_assumptions,
    !.
learn_cave_bounds :-
    facing(north),
    % Hit wall when walking north, so learn maxY
    agent_position((_, MaxY)),
    assertz(certain(maxY, MaxY)),
    review_gt_max_y_assumptions,
    !.
learn_cave_bounds :-
    facing(south),
    % Hit wall when walking south, so learn minY
    agent_position((_, MinY)),
    assertz(certain(minY, MinY)),
    review_lt_min_y_assumptions,
    !.

% Review assumptions
% Remove possible_positions that are out of bounds

review_lt_min_x_assumptions :-
    certain(minX, MinX),
    possible_position(Element, Pos),
    Pos = (X, _),
    X < MinX,
    retractall(possible_position(Element, Pos)),
    fail.
review_lt_min_x_assumptions.

review_lt_min_y_assumptions :-
    certain(minY, MinY),
    possible_position(Element, Pos),
    Pos = (_, Y),
    Y < MinY,
    retractall(possible_position(Element, Pos)),
    fail.
review_lt_min_y_assumptions.

review_gt_max_x_assumptions :-
    certain(maxX, MaxX),
    possible_position(Element, Pos),
    Pos = (X, _),
    X > MaxX,
    retractall(possible_position(Element, Pos)),
    fail.
review_lt_max_x_assumptions.

review_gt_max_y_assumptions :-
    certain(maxY, MaxY),
    possible_position(Element, Pos),
    Pos = (_, Y),
    Y > MaxY,
    retractall(possible_position(Element, Pos)),
    fail.
review_gt_max_y_assumptions.

% infer_dangerous_positions/0
% Use current knowledge to consolidate possible positions of dangers into certainties.
% Used for enemies, teleporters and pits.
% Eg. If there were steps at one cell with 4 neighbors and the agent is certain that 3
% of those have no enemies, than the enemy has to be on the fourth one.
% TODO: Take into account the maximum number of enemies and certainties to infer possibilities
infer_dangerous_positions :-
    % For each danger trio (Hint, NotThere, There)
    % e.g. (steps, no_enemy, enemy) or (breeze, no_pit, pit)
    Dangers = [(steps, no_enemy, enemy), (breeze, no_pit, pit), (flash, no_teleporter, teleporter)],
    member((Hint, NotThere, There), Dangers),
    % For each known Hint (e.g. steps) location
    certain(Hint, Pos),
    % Get valid neighboring cells
    findall(MaybeDangerPos, (
        adjacent(Pos, MaybeDangerPos, _),
        maybe_valid_position(MaybeDangerPos)
    ), MaybeDangerPositions),
    % Get the number of cells
    length(MaybeDangerPositions, CellCount),
    % Filter the ones that are known not to have dangers (e.g. enemies) in them
    findall(NotADangerousPos, (
        member(NotADangerousPos, MaybeDangerPositions),
        certain(NotThere, NotADangerousPos)
    ), NotDangerousPositions),
    % If we know the danger (e.g. enemy) not to be in N-1 of them, then it has only one possible location
    CertainSize is CellCount - 1,
    length(NotDangerousPositions, CertainSize),
    % Get the position
    findall(Cell, (
        member(Cell, MaybeDangerPositions),
        \+ memberchk(Cell, NotDangerousPositions)
    ), [DangerPos|_]),
    % Learn that there is a danger (e.g. enemy) there
    learn(There, DangerPos),
    % Backtrack
    fail.
infer_dangerous_positions.

% maybe_valid_position/1
% maybe_valid_position(+Pos)
% Checks if a position is possibly valid, or fails if known to be invalid from learned bounds
maybe_valid_position((X, Y)) :-
    maybe_valid_min_x(X),
    maybe_valid_min_y(Y),
    maybe_valid_max_x(X),
    maybe_valid_max_y(Y).

% maybe_valid_min_x/1
% maybe_valid_min_x(+X)
maybe_valid_min_x(X) :-
    % If known to be invalid, fail
    certain(minX, MinX),
    X < MinX,
    !, fail.
maybe_valid_min_x(_).

% maybe_valid_max_x/1
% maybe_valid_max_x(+X)
maybe_valid_max_x(X) :-
    % If known to be invalid, fail
    certain(maxX, MaxX),
    X > MaxX,
    !, fail.
maybe_valid_max_x(_).

% maybe_valid_min_y/1
% maybe_valid_min_y(+Y)
maybe_valid_min_y(Y) :-
    % If known to be invalid, fail
    certain(minY, MinY),
    Y < MinY,
    !, fail.
maybe_valid_min_y(_).

% maybe_valid_max_y/1
% maybe_valid_max_y(+Y)
maybe_valid_max_y(Y) :-
    % If known to be invalid, fail
    certain(maxY, MaxY),
    Y > MaxY,
    !, fail.
maybe_valid_max_y(_).
    

%
% Update goal
% -----------

update_goal :-
    % If no goal, set a new one
    \+ goal(_),
    ask_goal_KB(Goal),
    set_goal(Goal),
    !.
update_goal :-
    % If goal is reach, and position is reached, set to explore
    goal(reach(P)),
    agent_position(P),
    ask_goal_KB(Goal),
    set_goal(Goal),
    !.
update_goal :-
    % If goal is reach, and haven't reached yet, don't change goal
    goal(reach(_)).
% For now, only `reach` goals. Later, if no cells are available and we are certain of enemy positions,
% we could set a goal to kill an enemy at a given position to unblock new areas

% set_goal/1
% set_goal(+Goal)
set_goal(Goal) :-
    retractall(goal(_)),
    assertz(goal(Goal)).

% TODO: implement goal choice
ask_goal_KB(reach(5, 5)).


%
% Actions
% -------
% 1. Move forward
% 2. Turn 90 deg clockwise
% 3. Pick up object (gold)
% 4. Shoot on the current facing direction (deals random damage between 20 and 50
%    to any enemy in the adjacent celin the direction the agent is facing)
% 5. Climb out of the cave (only at the start )

% TODO: implement next action choice based on current goal
next_action(move_forward).

% perform_action/1
% perform_action(+Action)
perform_action(move_forward) :-
    facing(Direction),
    agent_position(AP),
    adjacent(AP, NP, Direction),
    retractall(agent_position(_)),
    assertz(agent_position(NP)),
    world_step.
