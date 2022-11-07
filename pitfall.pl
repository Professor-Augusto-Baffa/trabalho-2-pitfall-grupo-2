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
    certain/2,
    collected/2
]).

% assert_new/1
% assert_new(+Term)
% Assertz Term if not already known. Avoids duplicate entries in the KB.
assert_new(Term) :-
    \+ Term,
    assertz(Term),
    !.
assert_new(_).

agent_position((1,1)).

collected(gold, 0).
collected(power_up, 0).

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
    world_position(agent, (X, Y)),
    facing(north),
    write('\033[48;5;35m^\033[0m'),
    !.
print_cave_cell(X, Y) :-
    world_position(agent, (X, Y)),
    facing(east),
    write('\033[48;5;35m>\033[0m'),
    !.
print_cave_cell(X, Y) :-
    world_position(agent, (X, Y)),
    facing(west),
    write('\033[48;5;35m<\033[0m'),
    !.
print_cave_cell(X, Y) :-
    world_position(agent, (X, Y)),
    facing(south),
    write('\033[48;5;35mv\033[0m'),
    !.
print_cave_cell(X, Y) :-
    certain(visited, (X,Y)),
    write('\033[48;5;231m\033[38;5;0m.\033[0m'),
    !.
print_cave_cell(X, Y) :-
    certain(safe, (X,Y)),
    write('\033[48;5;231m\033[38;5;0mS\033[0m'),
    !.
print_cave_cell(X, Y) :-
    certain(pit, (X,Y)),
    write('\033[48;5;238mP\033[0m'),
    !.
print_cave_cell(X, Y) :-
    certain(enemy, (X,Y)),
    write('\033[48;5;208mD\033[0m'),
    !.
print_cave_cell(X, Y) :-
    certain(teleporter, (X,Y)),
    write('\033[48;5;26mT\033[0m'),
    !.
print_cave_cell(X, Y) :-
    certain(gold, (X,Y)),
    write('\033[48;5;220mO\033[0m'),
    !.
print_cave_cell(X, Y) :-
    possible_position(pit, (X,Y)),
    possible_position(enemy, (X,Y)),
    possible_position(teleporter, (X,Y)),
    write('!'),
    !.
print_cave_cell(X, Y) :-
    possible_position(pit, (X,Y)),
    possible_position(enemy, (X,Y)),
    write('ç'),
    !.
print_cave_cell(X, Y) :-
    possible_position(pit, (X,Y)),
    possible_position(teleporter, (X,Y)),
    write('+'),
    !.
print_cave_cell(X, Y) :-
    possible_position(enemy, (X,Y)),
    possible_position(teleporter, (X,Y)),
    write('#'),
    !.
print_cave_cell(X, Y) :-
    possible_position(pit, (X,Y)),
    write('p'),
    !.
print_cave_cell(X, Y) :-
    possible_position(enemy, (X,Y)),
    write('d'),
    !.
print_cave_cell(X, Y) :-
    possible_position(teleporter, (X,Y)),
    write('t'),
    !.
print_cave_cell(_, _) :-
    write('\033[48;5;0m\033[38;5;0m?\033[0m'),
    !.

%
% Observation and decision making
% ----

% sense_learn_act/2
% sense_learn_act(-Goal, -Action)
% Learns from the environment and acts accordingly, returning the current Goal and the Action it performed
sense_learn_act(Goal, Action) :-
    format('-----~n'),
    sense_environment(Sensors), write_sensors(Sensors),
    clear_transient_flags,
    format('Learning:~n'),
    update_knowledge(Sensors),
    update_goal,
    goal(Goal),
    next_action(Goal, Action),
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

write_sensors(Sensors) :-
    agent_position(AP),
    world_position(agent, ActualAP),
    facing(Dir),
    format('Agent~n~t~2|at: ~w~n~t~2|sensing: ~w~nWorld~n~t~2|agent at: ~w~n~t~2|facing: ~w~n', [AP, Sensors, ActualAP, Dir]),
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
% glow if gold in agents cell
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
    assert_new(hit_wall).

world_pick_up :-
    % If on the same position as gold, remove gold from the world
    world_position(agent, AP),
    world_position(gold, AP),
    retractall(world_position(gold, AP)).

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
    update_scream(Scream),
    set_visited_cell,
    infer_dangerous_positions,
    infer_safe_positions.

set_visited_cell :-
    agent_position(AP),
    assert_new(certain(visited, AP)),
    format('~t~2|visited: ~w~n', [AP]).

% update_steps/1
% update_steps(+Steps)
update_steps(Steps) :-
    agent_position(AP),
    % Retract in case the agent killed an enemy that previously generated steps here
    retractall(certain(steps, AP)),
    retractall(certain(no_steps, AP)),
    assertz(certain(Steps, AP)),
    update_enemies(Steps).

% update_enemies/1
% update_enemies(+Steps)
update_enemies(no_steps) :-
    agent_position(AP),
    retractall(certain(enemy, AP)),
    learn(no_enemy, AP),
    adjacent(AP, P, _),
    maybe_valid_position(P),
    learn(no_enemy, P),
    fail.
update_enemies(no_steps).

update_enemies(steps) :-
    agent_position(AP),
    adjacent(AP, P, _),
    maybe_valid_position(P),
    \+ certain(no_enemy, P),
    assert_new(possible_position(enemy, P)),
    fail.
update_enemies(steps).

% update_breeze/1
% update_breeze(+Breeze)
update_breeze(Breeze) :-
    agent_position(AP),
    assert_new(certain(Breeze, AP)),
    update_pits(Breeze).

% update_pits/1
% update_pits(+Breeze)
update_pits(no_breeze) :-
    agent_position(AP),
    learn(no_pit, AP),
    adjacent(AP, P, _),
    maybe_valid_position(P),
    learn(no_pit, P),
    fail.
update_pits(no_breeze).

update_pits(breeze) :-
    agent_position(AP),
    adjacent(AP, P, _),
    maybe_valid_position(P),
    \+ certain(no_pit, P),
    assert_new(possible_position(pit, P)),
    fail.
update_pits(breeze).

% update_flash/1
% update_flash(+Flash)
update_flash(Flash) :-
    agent_position(AP),
    % Retract in case there was flash generated by a now killed teleporter
    retractall(certain(flash, AP)),
    assert_new(certain(Flash, AP)),
    update_teleporter(Flash).

% update_teleporter/1
% update_teleporter(+Flash)
update_teleporter(no_flash) :-
    agent_position(AP),
    assert_new(certain(no_teleporter, AP)),
    adjacent(AP, P, _),
    maybe_valid_position(P),
    learn(no_teleporter, P),
    fail.
update_teleporter(no_flash) :- !.

update_teleporter(flash) :-
    agent_position(AP),
    adjacent(AP, P, _),
    maybe_valid_position(P),
    \+ certain(no_teleporter, P),
    assert_new(possible_position(teleporter, P)),
    fail.
update_teleporter(flash).

% update_glow/1
% update_glow(+Glow)
update_glow(Glow) :-
    agent_position(AP),
    % Retract in case we collected gold from this position
    retractall(certain(glow, AP)),
    assert_new(certain(Glow, AP)),
    update_gold(Glow).

% update_gold/1
% update_gold(+Glow)
update_gold(glow) :-
    agent_position(AP),
    assert_new(certain(gold, AP)),
    format('~t~2|gold position: ~w~n', [AP]).
update_gold(no_glow) :-
    agent_position(AP),
    % Retract in case we collected gold from this position
    retractall(certain(gold, AP)),
    assert_new(certain(no_gold, AP)).

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
    format('~t~2|agent position: ~w~n', [PrevPos]),
    % Learn cave bounds
    learn_cave_bounds.

% update_scream/1
% update_scream(+Scream)
update_scream(no_scream).
update_scream(scream) :-
    agent_position(AP),
    facing(Dir),
    adjacent(AP, P, Dir),
    maybe_valid_position(P),
    learn(no_enemy, P),
    learn(no_teleporter, P).


% learn/2
% learn(+Item, +Location)
learn(no_enemy, P) :-
    retractall(possible_position(enemy, P)),
    retractall(certain(enemy, P)),
    assert_new(certain(no_enemy, P)).
learn(enemy, P) :-
    retractall(possible_position(_, P)),
    assert_new(certain(enemy, P)).
learn(no_teleporter, P) :-
    retractall(possible_position(teleporter, P)),
    retractall(certain(teleporter, P)),
    assert_new(certain(no_teleporter, P)).
learn(teleporter, P) :-
    retractall(possible_position(_, P)),
    assert_new(certain(teleporter, P)).
learn(no_pit, P) :-
    retractall(possible_position(pit, P)),
    assert_new(certain(no_pit, P)).
learn(pit, P) :-
    retractall(possible_position(_, P)),
    assert_new(certain(pit, P)).
learn(safe, P) :-
    % Do nothing if already known to be safe
    certain(safe, P),
    !.
learn(safe, P) :-
    member(Danger, [enemy, pit, teleporter]),
    retractall(possible_position(Danger, P)),
    fail.
learn(safe, P) :-
    assert_new(certain(safe, P)),
    format('~t~2|safe: ~w~n', [P]).


learn_cave_bounds :-
    facing(east),
    % Hit wall when walking east, so learn maxX
    agent_position((MaxX, _)),
    assert_new(certain(maxX, MaxX)),
    review_gt_max_x_assumptions,
    !.
learn_cave_bounds :-
    facing(west),
    % Hit wall when walking west, so learn minX
    agent_position((MinX, _)),
    assert_new(certain(minX, MinX)),
    review_lt_min_x_assumptions,
    !.
learn_cave_bounds :-
    facing(north),
    % Hit wall when walking north, so learn maxY
    agent_position((_, MaxY)),
    assert_new(certain(maxY, MaxY)),
    review_gt_max_y_assumptions,
    !.
learn_cave_bounds :-
    facing(south),
    % Hit wall when walking south, so learn minY
    agent_position((_, MinY)),
    assert_new(certain(minY, MinY)),
    review_lt_min_y_assumptions,
    !.

% Review assumptions
% Remove possible_positions that are out of bounds

review_lt_min_x_assumptions :-
    certain(minX, MinX),
    (possible_position(Element, Pos) ; certain(Element, Pos)),
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

% infer_safe_positions/0
infer_safe_positions :-
    certain(no_pit, Pos),
    certain(no_enemy, Pos),
    certain(no_teleporter, Pos),
    learn(safe, Pos),
    fail.
infer_safe_positions.

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
    % If the goal is to reach an invalid position, remove goal and backtrack
    goal(reach(Pos)),
    \+ maybe_valid_position(Pos),
    retractall(goal(_)),
    fail.
update_goal :-
    % If no goal, set a new one
    \+ goal(_),
    ask_goal_KB(Goal),
    set_goal(Goal),
    !.
update_goal :-
    % If goal is reach, and position is reached, get new goal
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
ask_goal_KB(reach(Pos)) :-
    certain(safe, Pos),
    maybe_valid_position(Pos),
    \+certain(visited, Pos),
    !.


%
% Actions
% -------
% 1. Move forward (move_forward)
% 2. Turn 90 deg clockwise (turn_clockwise)
% 3. Pick up object (gold)
% 4. Shoot on the current facing direction (deals random damage between 20 and 50
%    to any enemy in the adjacent celin the direction the agent is facing)
% 5. Climb out of the cave (only at the start )

% next_action/2
% next_action(+Goal, -Action)
% Gets the next Action to perform in order to reach Goal
next_action(_, pick_up) :-
    agent_position(AP),
    certain(gold, AP),
    !.
next_action(reach(Pos), move_forward) :-
    % If goal is to reach a position
    % and the agent is next to the position and facing the right direction
    agent_position(AP),
    facing(Dir),
    adjacent(AP, Pos, Dir),
    % move forward
    !.
next_action(reach(Pos), turn_clockwise) :-
    % If goal is to reach a position
    % and the agent is next to the position, but facing the wrong direction
    agent_position(AP),
    adjacent(AP, Pos, _),
    % turn clockwise
    !.
next_action(reach(Pos), Action) :-
    % If goal is to reach a position and the agent is not next to the position
    % try to reach an adjacent position
    % TODO: Save path to avoid recalculation
    % TODO: Try to improve performance of BFS
    agent_position(AP),
    bfs(AP, Pos, [Next | _]),
    next_action(reach(Next), Action),
    !.

% perform_action/1
% perform_action(+Action)
perform_action(move_forward) :-
    facing(Direction),
    agent_position(AP),
    adjacent(AP, NP, Direction),
    retractall(agent_position(_)),
    assertz(agent_position(NP)),
    world_step.
perform_action(turn_clockwise) :-
    facing(Direction),
    clockwise(Direction, NewDirection),
    retractall(facing(_)),
    assertz(facing(NewDirection)).
perform_action(pick_up) :-
    collected(gold, Count),
    NewCount is Count + 1,
    retractall(collected(gold, _)),
    assertz(collected(gold, NewCount)),
    world_pick_up.

% bfs/3
% bfs(+Origin, +Goal, -Path)
% Runs a bfs from a safe node to a possibly unsafe node, using a visited path
bfs(Origin, Goal, Path) :-
    reverse_bfs(Goal, [[Origin]], S),
    reverse(S, [Origin | Path]),
    !.

% reverse_bfs/3
% reverse_bfs(+Goal, +PossiblePaths, -Path)
% Starts with PossiblePaths as [[Origin]] and expands a bfs. When done, Path
% is the path from Goal to Origin (the reverse path from origin to Goal).
reverse_bfs(Goal, [[Goal | Path] | _], [Goal | Path]).
reverse_bfs(Goal, [Path | Paths], Solution) :-
    bfs_extend(Path, Goal, NewPaths),
    append(Paths, NewPaths, AllPaths),
    reverse_bfs(Goal, AllPaths, Solution).
reverse_bfs(Goal, [_|Paths], Solution) :-
    reverse_bfs(Goal, Paths, Solution).

% bfs_extend/3
% bfs_extend(+Path, +Goal, -NewPaths)
% Extends a known Path with its valid neighbours. An adjacent cell is included
% in the path if it is visited or if it is the goal.
bfs_extend([Origin | Path], Goal, NewPaths)  :-
    findall(
        [Next, Origin | Path],
        (
            adjacent(Origin, Next, _),
            maybe_valid_position(Next),
            (Next = Goal ; certain(visited, Next)),
            \+ member(Next, [Origin | Path])
        ),
        NewPaths
    ),
    !.
