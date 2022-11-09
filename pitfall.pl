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

:- module(pitfall, [
    sense/1,
    learn/3,
    act/1,
    sense_learn_act/2,
    print_cave/0,
    disable_logging/0,
    enable_logging/0,
    force_update_position/1,
    certain/2,
    run_until_done/0,
    get_agent_health/1,
    get_game_score/1
]).

:- use_module(a_star).
:- use_module(logging).

:- dynamic([
    world_position/2,
    world_count/2,
    possible_position/3,
    agent_position/1,
    facing/1,
    hit_wall/0,
    killed_enemy/0,
    goal/1,
    certain/2,
    collected/2,
    game_score/1,
    health/3,
    agent_health/1,
    inventory/3
]).

:- enable_logging.

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

world_position(teleporter, (1, 7)).
world_position(teleporter, (4, 11)).
world_position(teleporter, (7, 3)).
world_position(teleporter, (10, 1)).

world_position(power_up, (1, 12)).
world_position(power_up, (2, 2)).
world_position(power_up, (7, 7)).

world_count(enemy, 4).
world_count(pit, 8).
world_count(gold, 3).
world_count(teleporter, 4).
world_count(power_up, 3).

% Print cave for debugging

print_cave :-
    minY(MinY), maxY(MaxY),
    between(MinY, MaxY, Y),
    Line is MaxY - Y + 1,
    print_cave_line(Line),
    fail.
print_cave :-
    get_agent_health(H),
    get_game_score(S),
    get_inventory(A, P),
    NP is 3 - P,
    collected(gold, G),
    log('Health: ~t~w~nScore: ~t~w~nAmmo: ~t~w~nGold: ~t~w~nPower ups: ~t~w~n',
        [H, S, A, G, NP]
    ).
print_cave_line(Y) :-
    minX(MinX), maxX(MaxX),
    between(MinX, MaxX, X),
    print_cave_cell(X, Y),
    log(' '),
    fail.
print_cave_line(_) :- log('~n').
print_cave_cell(X, Y) :-
    world_position(agent, (X, Y)),
    facing(north),
    log('\033[48;5;35m^\033[0m'),
    !.
print_cave_cell(X, Y) :-
    world_position(agent, (X, Y)),
    facing(east),
    log('\033[48;5;35m>\033[0m'),
    !.
print_cave_cell(X, Y) :-
    world_position(agent, (X, Y)),
    facing(west),
    log('\033[48;5;35m<\033[0m'),
    !.
print_cave_cell(X, Y) :-
    world_position(agent, (X, Y)),
    facing(south),
    log('\033[48;5;35mv\033[0m'),
    !.
print_cave_cell(X, Y) :-
    certain(visited, (X,Y)),
    log('\033[48;5;231m\033[38;5;0m.\033[0m'),
    !.
print_cave_cell(X, Y) :-
    certain(safe, (X,Y)),
    log('\033[48;5;231m\033[38;5;0mS\033[0m'),
    !.
print_cave_cell(X, Y) :-
    certain(pit, (X,Y)),
    log('\033[48;5;238mP\033[0m'),
    !.
print_cave_cell(X, Y) :-
    certain(enemy, (X,Y)),
    log('\033[48;5;208mD\033[0m'),
    !.
print_cave_cell(X, Y) :-
    certain(teleporter, (X,Y)),
    log('\033[48;5;26mT\033[0m'),
    !.
print_cave_cell(X, Y) :-
    certain(gold, (X,Y)),
    log('\033[48;5;220mO\033[0m'),
    !.
print_cave_cell(X, Y) :-
    possible_position(pit, (X,Y), _),
    possible_position(enemy, (X,Y), _),
    possible_position(teleporter, (X,Y), _),
    log('+'),
    !.
print_cave_cell(X, Y) :-
    possible_position(pit, (X,Y), _),
    possible_position(enemy, (X,Y), _),
    log('+'),
    !.
print_cave_cell(X, Y) :-
    possible_position(pit, (X,Y), _),
    possible_position(teleporter, (X,Y), _),
    log('+'),
    !.
print_cave_cell(X, Y) :-
    possible_position(enemy, (X,Y), _),
    possible_position(teleporter, (X,Y), _),
    log('+'),
    !.
print_cave_cell(X, Y) :-
    possible_position(pit, (X,Y), _),
    log('\033[48;5;238mp\033[0m'),
    !.
print_cave_cell(X, Y) :-
    possible_position(enemy, (X,Y), _),
    log('\033[48;5;208md\033[0m'),
    !.
print_cave_cell(X, Y) :-
    possible_position(teleporter, (X,Y), _),
    log('\033[48;5;26mt\033[0m'),
    !.
print_cave_cell(_, _) :-
    log('\033[48;5;0m\033[38;5;0m?\033[0m'),
    !.


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
% If NewHealth <= 0, call character_killed rule
update_health(Pos, Character, NewHealth) :-
    (NewHealth > 0),
    retractall(health(Pos, Character, _)),
    assertz(health(Pos, Character, NewHealth)),
    !.
update_health(Pos, Character, _) :-
    retractall(health(Pos, Character, _)),
    assertz(health(Pos, Character, 0)),
    character_killed(Character, Pos),
    !.

% character_killed/1
% Called when character's (except agent) HP reaches 0
character_killed(EnemyType, EnemyPos) :-
    log('Character killed!~n'),
    retractall(world_position(EnemyType, EnemyPos)),
    (   EnemyType = teleporter
    ->  CountType = teleporter
    ;   CountType = enemy
    ),
    world_count(CountType, C),
    NC is C - 1,
    retractall(world_count(CountType, _)),
    assertz(world_count(CountType, NC)),
    assertz(killed_enemy),
    !.

% get_agent_health/1
% Get agent's health
% If agent's health hasn't been tracked yet, initialize new health
get_agent_health(Health) :-
    agent_health(Health),
    !.
get_agent_health(Health) :-
    initial_health(agent, Health),
    assertz(agent_health(Health)),
    !.

% update_agent_health/1
% Update agent's health
% When NewHealth <= 0, agent is killed, game over!
update_agent_health(NewHealth) :-
    (NewHealth > 0),
    retractall(agent_health(_)),
    assertz(agent_health(NewHealth)),
    !.
update_agent_health(_) :-
    retractall(agent_health(_)),
    assertz(agent_health(0)),
    agent_killed,
    !.

% agent_killed/0
% Called when agent's HP reaches zero, game over!
agent_killed :-
    killed_score,
    log('You died! Game over!~n'),
    fail.


% 
% Score System: Costs and Rewards
% ------
% Assumes game score cannot be negative
% 1. Pick up: +1000
% 2. Falling in a pit: -1000
% 3. Getting killed by an enemy: -1000
% 4. Being attacked by an enemy: -{dammage}
% 5. Shooting: -10
% 6. Other Actions (moving, turning, etc): -1

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
    (NewScore >= 0),
    retractall(game_score(_)),
    assertz(game_score(NewScore)),
    !.
update_game_score(_).

% pick_up/0
% Pick Up Reward -> +1000 points
pick_up_score :-
    get_game_score(OldScore),
    (NewScore is 1000+integer(OldScore)),
    update_game_score(NewScore),
    !.

% killed/0
% Killed by enemy -> -1000 points
killed_score :-
    get_game_score(OldScore),
    (NewScore is integer(OldScore)-1000),
    update_game_score(NewScore),
    !.

% attacked/1
% Attacked by enemy -> -{damage}
attacked_score(Damage) :-
    get_game_score(OldScore),
    (NewScore is integer(OldScore)-integer(Damage)),
    update_game_score(NewScore),
    !.

% shooting/0
% Shooting an arrow -> -10 points
shooting_score :-
    get_game_score(OldScore),
    (NewScore is integer(OldScore)-10),
    update_game_score(NewScore),
    !.

% gen_action_score/0
% For general actions such as moving, etc -> -1 point
gen_action_score :-
    get_game_score(OldScore),
    (NewScore is integer(OldScore)-1),
    update_game_score(NewScore),
    !.



%
% Inventory
% ----
% Power ups count: 3
% Ammo count: 5

% initial_inventory/3
initial_inventory(agent, 5, 3).

% get_inventory/2
% Get agent's inventory
get_inventory(Ammo, PowerUps) :-
    inventory(agent, Ammo, PowerUps),
    !.
get_inventory(Ammo, PowerUps) :-
    initial_inventory(agent, Ammo, PowerUps),
    assertz(inventory(agent, Ammo, PowerUps)),
    !.


% update_inventory/2
% Called whenever agent uses a power up or ammo
update_inventory(NewAmmo, NewPowerUps) :-
    (NewAmmo >= 0, NewPowerUps >= 0),
    retractall(inventory(agent,_,_)),
    assertz(inventory(agent, NewAmmo, NewPowerUps)),
    !.
update_inventory(_,_).


% use_ammo/0
% Agent uses 1 ammo
use_ammo :-
    get_inventory(OldAmmo, PowerUps),
    (NewAmmo is integer(OldAmmo)-1),
    update_inventory(NewAmmo, PowerUps),
    !.

% use_power_up/0
% Agent uses 1 power up
use_power_up :-
    get_inventory(Ammo, OldPowerUps),
    (NewPowerUps is integer(OldPowerUps)-1),
    update_inventory(Ammo, NewPowerUps),
    world_position(agent, AP),
    retractall(world_position(power_up, AP)),
    !.



%
% Attack System
% ----
% Small Enemy Attack -> -20 HP for agent
% Big Enemy Attack -> -50 HP for agent
% Ammo damage: random between 20 and 50
% Ammo count: 5

% receive_effects/0
receive_effects :-
    log('Effects:~n'),
    agent_power_up,
    small_enemy_attacks,
    big_enemy_attacks,
    teleport_attack,
    fall_in_pit.


% agent_power_up/0
% Rule for updating health with +20 HP when agent collects power up
% Only 3 available + has to match agent's position
agent_power_up :-
    world_position(agent, AP),
    world_position(power_up, AP),
    use_power_up,
    get_agent_health(OldHealth),
    (NewHealth is integer(OldHealth)+20),
    !,
    update_agent_health(NewHealth).
agent_power_up :-
    log('~t~2|No power up available!~n').

% small_enemy_attacks/0
%
% 1. game score -> -20 points
% 2. agent health -> -20 points
small_enemy_attacks :-
    % If the agent is at an enemy's position
    world_position(agent, AP),
    world_position(small_enemy, AP),
    % Decrease the score
    attacked_score(20),
    % Decrease health
    get_agent_health(OldHealth),
    (NewHealth is integer(OldHealth)-20),
    !,
    update_agent_health(NewHealth),
    log('~t~2|Attacked by a small enemy!~n').
small_enemy_attacks.

% big_enemy_attacks/0
% 
% 1. game score -> -50 points
% 2. agent health = -50 points
big_enemy_attacks :-
    % If the agent is at an enemy's position
    world_position(agent, AP),
    world_position(large_enemy, AP),
    % Decrease the score
    attacked_score(50),
    % Decrease health
    get_agent_health(OldHealth),
    (NewHealth is integer(OldHealth)-50),
    !,
    update_agent_health(NewHealth),
    log('~t~2|Attacked by a big enemy!~n').
big_enemy_attacks.

% teleport_attack/0
% Enemy attacks with teleport
teleport_attack :-
    world_position(agent, AP),
    world_position(teleporter, AP),
    % Find new position
    minX(MinX), maxX(MaxX), minY(MinY), maxY(MaxY),
    random_between(MinX, MaxX, X),
    random_between(MinY, MaxY, Y),
    % Update world position
    retractall(world_position(agent, _)),
    assertz(world_position(agent, (X, Y))),
    retractall(agent_position(_)),
    log('~t~2|Attacked by a teleporter!~n'),
    !.
teleport_attack.

fall_in_pit :-
    world_position(agent, AP),
    world_position(pit, AP),
    !,
    update_agent_health(0).
fall_in_pit.

% agent_attacks/2
% Damage is a random number between 20 and 50
%
% 1. game score -> -10 points
% 2. ammo -> -1
% 3. enemy health -> -{damage}
agent_attacks(EnemyPos, Enemy) :-
    get_inventory(Ammo, _),
    (Ammo > 0), 
    use_ammo,
    shooting_score,
    random_between(20, 50, Damage),
    get_health(EnemyPos, Enemy, OldHealth),
    (NewHealth is integer(OldHealth)-integer(Damage)),
    update_health(EnemyPos, Enemy, NewHealth),
    !.
agent_attacks(_,_) :-
    log('Agent attack on enemy failed!~n'),
    !.

%
% Observation and decision making
% ----

% sense_learn_act/2
% sense_learn_act(-Goal, -Action)
% Learns from the environment and acts accordingly, returning the current Goal and the Action it performed
sense_learn_act(Goal, Action) :-
    log('-----~n'),
    sense(Sensors),
    log('Learning:~n'),
    learn(Sensors, Goal, Action),
    print_cave,
    log('Goal: ~w~nAction: ~w~n', [Goal, Action]),
    act(Action).

% sense/1
% sense(-Sensors)
sense(Sensors) :-
    receive_effects,
    sense_environment(Sensors),
    write_sensors(Sensors).

% learn/3
% learn(+Sensors, -Goal, -Action)
learn(Sensors, Goal, Action) :-
    update_knowledge(Sensors),
    update_goal(Goal),
    next_action(Goal, Action).

% act/1
% act(+Action)
act(Action) :-
    clear_transient_flags,
    perform_action(Action).


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
    log('Agent~n~t~2|at: ~w~n~t~2|sensing: ~w~nWorld~n~t~2|agent at: ~w~n~t~2|facing: ~w~n', [AP, Sensors, ActualAP, Dir]),
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
    valid_position(PP).
sense_breeze(no_breeze).

% sense_flash/1
% sense_flash(-Flash)
% flash if teleporter in adjacent cell
sense_flash(flash) :-
    world_position(agent, AP),
    world_position(teleporter, TP),
    adjacent(AP, TP, _),
    valid_position(TP).
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

world_shoot :-
    world_position(agent, AP),
    facing(Dir),
    adjacent(AP, EnemyPos, Dir),
    world_position(EnemyType, EnemyPos),
    agent_attacks(EnemyPos, EnemyType),
    !.

world_pick_up :-
    % If on the same position as gold, remove gold from the world
    world_position(agent, AP),
    world_position(gold, AP),
    pick_up_score,
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
    update_scream(Scream),
    update_steps(Steps),
    update_breeze(Breeze),
    update_flash(Flash),
    update_glow(Glow),
    set_visited_cell,
    infer_dangerous_positions,
    infer_safe_positions.

set_visited_cell :-
    agent_position(AP),
    assert_new(certain(visited, AP)),
    log('~t~2|visited: ~w~n', [AP]).

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
    % If already found all enemies
    world_count(enemy, EC),
    aggregate_all(count, certain(enemy, _), EC),
    !.
update_enemies(steps) :-
    agent_position(AP),
    adjacent(AP, P, _),
    maybe_valid_position(P),
    \+ certain(no_enemy, P),
    assert_new(possible_position(enemy, P, AP)),
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
    % If already found all pits
    world_count(pit, PC),
    aggregate_all(count, certain(pit, _), PC),
    !.
update_pits(breeze) :-
    agent_position(AP),
    adjacent(AP, P, _),
    maybe_valid_position(P),
    \+ certain(no_pit, P),
    assert_new(possible_position(pit, P, AP)),
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
    % If already found all pits
    world_count(teleporter, TC),
    aggregate_all(count, certain(teleporter, _), TC),
    !.
update_teleporter(flash) :-
    agent_position(AP),
    adjacent(AP, P, _),
    maybe_valid_position(P),
    \+ certain(no_teleporter, P),
    assert_new(possible_position(teleporter, P, AP)),
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
    log('~t~2|gold position: ~w~n', [AP]).
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
    log('~t~2|agent position: ~w~n', [PrevPos]),
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
    learn(killed_enemy, P).


% learn/2
% learn(+Item, +Location)
learn(no_enemy, P) :-
    retractall(possible_position(enemy, P, _)),
    retractall(certain(enemy, P)),
    assert_new(certain(no_enemy, P)).
learn(enemy, P) :-
    retractall(possible_position(_, P, _)),
    assert_new(certain(enemy, P)),
    check_count(enemy).
learn(no_teleporter, P) :-
    retractall(possible_position(teleporter, P, _)),
    retractall(certain(teleporter, P)),
    assert_new(certain(no_teleporter, P)).
learn(teleporter, P) :-
    retractall(possible_position(_, P, _)),
    assert_new(certain(teleporter, P)),
    check_count(teleporter).
learn(no_pit, P) :-
    retractall(possible_position(pit, P, _)),
    assert_new(certain(no_pit, P)).
learn(pit, P) :-
    retractall(possible_position(_, P, _)),
    assert_new(certain(pit, P)),
    check_count(pit).
learn(safe, P) :-
    % Do nothing if already known to be safe
    certain(safe, P),
    !.
learn(safe, P) :-
    member(Danger, [enemy, pit, teleporter]),
    retractall(possible_position(Danger, P, _)),
    fail.
learn(safe, P) :-
    assert_new(certain(safe, P)),
    log('~t~2|safe: ~w~n', [P]).
learn(killed_enemy, P) :-
    % If killed normal enemy
    certain(enemy, P),
    adjacent(P, N, _),
    % Retract steps observations and mark as not visited, as steps could be
    % from another enemy
    retractall(certain(steps, N)),
    retractall(certain(visited, N)),
    % Retract possible enemy positions caused by these steps
    retractall(possible_position(enemy, _, N)),
    fail.
learn(killed_enemy, P) :-
    % If killed teleporter
    certain(teleporter, P),
    adjacent(P, N, _),
    % Retract flash observations and mark as not visited, as flash could be
    % from another teleporter
    retractall(certain(flash, N)),
    retractall(certain(visited, N)),
    % Retract possible teleporter positions caused by this flash
    retractall(possible_position(teleporter, _, N)),
    fail.
learn(killed_enemy, P) :-
    retractall(certain(enemy, P)),
    assertz(certain(no_enemy, P)),
    retractall(certain(teleporter, P)),
    assertz(certain(no_teleporter, P)),
    !.

learn_no_more_enemies :-
    possible_position(enemy, P, _),
    learn(no_enemy, P),
    fail.
learn_no_more_enemies.

learn_no_more_teleporters :-
    possible_position(teleporter, P, _),
    learn(no_teleporter, P),
    fail.
learn_no_more_teleporters.

learn_no_more_pits :-
    possible_position(pit, P, _),
    learn(no_pit, P),
    fail.
learn_no_more_pits.

check_count(enemy) :-
    world_count(enemy, ExistingEnemiesCount),
    aggregate_all(count, certain(enemy, _), ExistingEnemiesCount),
    learn_no_more_enemies.
check_count(enemy).

check_count(teleporter) :-
    world_count(teleporter, ExistingTeleporterCount),
    aggregate_all(count, certain(teleporter, _), ExistingTeleporterCount),
    learn_no_more_teleporters.
check_count(teleporter).

check_count(pit) :-
    world_count(pit, ExistingPitsCount),
    aggregate_all(count, certain(pit, _), ExistingPitsCount),
    learn_no_more_pits.
check_count(pit).

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
    log('~t~2|X >= ~w~n', [MinX]),
    (possible_position(_, (X, _), _) ; certain(_, (X, _))),
    X < MinX,
    retractall(possible_position(_, (X, _), _)),
    retractall(certain(_, (X, _))),
    fail.
review_lt_min_x_assumptions.

review_lt_min_y_assumptions :-
    certain(minY, MinY),
    log('~t~2|Y >= ~w~n', [MinY]),
    (possible_position(_, (_, Y), _) ; certain(_, (_, Y))),
    Y < MinY,
    retractall(possible_position(_, (_, Y), _)),
    retractall(certain(_, (_, Y))),
    fail.
review_lt_min_y_assumptions.

review_gt_max_x_assumptions :-
    certain(maxX, MaxX),
    log('~t~2|X <= ~w~n', [MaxX]),
    (possible_position(_, (X, _), _) ; certain(_, (X, _))),
    X > MaxX,
    retractall(possible_position(_, (X, _), _)),
    fail.
review_gt_max_x_assumptions.

review_gt_max_y_assumptions :-
    certain(maxY, MaxY),
    log('~t~2|Y <= ~w~n', [MaxY]),
    (possible_position(_, (_, Y), _) ; certain(_, (_, Y))),
    Y > MaxY,
    retractall(possible_position(_, (_, Y), _)),
    fail.
review_gt_max_y_assumptions.

% infer_dangerous_positions/0
% Use current knowledge to consolidate possible positions of dangers into certainties.
% Used for enemies, teleporters and pits.
% Eg. If there were steps at one cell with 4 neighbors and the agent is certain that 3
% of those have no enemies, than the enemy has to be on the fourth one.
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

% update_goal/1
% update_goal(-NewGoal)
update_goal(NewGoal) :-
    goal(Goal),
    update_goal(Goal, NewGoal),
    !.
update_goal(NewGoal) :-
    update_goal(none, NewGoal).

% update_goal/2
% update_goal(+CurrGoal, -NewGoal)
update_goal(leave, leave).
update_goal(reach(Pos), NewGoal) :-
    % If the goal is to reach an invalid position, remove goal and get a new one
    \+ maybe_valid_position(Pos),
    retractall(goal(_)),
    update_goal(none, NewGoal).
update_goal(kill(Pos), NewGoal) :-
    % If the goal is to kill an enemy, and the enemy has been killed, remove goal and get a new one
    certain(no_enemy, Pos),
    certain(no_teleporter, Pos),
    retractall(goal(_)),
    update_goal(none, NewGoal).
update_goal(kill(_), NewGoal) :-
    % If the goal is to kill an enemy and there is no more ammo left, remove goal and get a new one
    get_inventory(0, _),
    retractall(goal(_)),
    update_goal(none, NewGoal).
update_goal(none, NewGoal) :-
    % If no goal, set a new one
    ask_goal_KB(NewGoal),
    set_goal(NewGoal),
    !.
update_goal(reach(Pos), NewGoal) :-
    % If goal is reach, and position is reached, get new goal
    agent_position(Pos),
    retractall(goal(_)),
    update_goal(none, NewGoal),
    !.
update_goal(reach(Pos), reach(Pos)) :-
    % If goal is reach, and haven't reached yet, don't change goal
    !.
update_goal(kill(Pos), kill(Pos)) :-
    % If the goal is to kill an enemy that hasn't been killed, keep it
    !.

% set_goal/1
% set_goal(+Goal)
set_goal(Goal) :-
    retractall(goal(_)),
    assertz(goal(Goal)).


ask_goal_KB(leave) :-
    % If collected all gold, leave the cave
    world_count(gold, GC),
    collected(gold, GC),
    !.

ask_goal_KB(reach(Pos)) :-
    next_position_to_explore(Pos),
    !.

ask_goal_KB(kill(EnemyPos)) :-
    % If unable to explore new positions, find an enemy in the frontier and kill it
    % Check if there is still ammo
    get_inventory(Ammo, _),
    Ammo \= 0,
    bagof(
        (Count, Distance, OneEnemyPos),
        AP^(
            % Get enemy in the known frontier
            enemy_in_frontier(OneEnemyPos, Dir),
            % Get the number of cells that will unblock in up to three movements
            frontier_count(OneEnemyPos, Dir, 3, Count),
            % Get the distance in case there's two equally good enemy options
            agent_position(AP),
            a_star_heuristic(AP, OneEnemyPos, Distance)
        ),
        EnemyChoices
    ),
    % Sort according to the number of unblocked cells and the distance
    sort(0, @>, EnemyChoices, [(_, _, EnemyPos) | _]),
    !.

% next_position_to_explore/1
% next_position_to_explore(-Pos)
% Gets the next position to try to reach when exploring the cave
next_position_to_explore(Pos) :-
    agent_position(AP),
    next_position_to_explore([AP], [], Pos).

% next_position_to_explore/3
% next_position_to_explore(+Queue, +Explored, -Pos)
% Expands neighbours in a BFS fashion until a safe unexplored position is found
next_position_to_explore([], _, _) :- !, fail.
next_position_to_explore([Pos | _], _, Pos) :-
    % If the next in the queue has not been visited, pick it to explore
    \+ certain(visited, Pos),
    !.
next_position_to_explore([Next | QueueTail], Explored, Pos) :-
    setof(
        Neighbour,
        Dir^(
            adjacent(Next, Neighbour, Dir),
            maybe_valid_position(Neighbour),
            certain(safe, Neighbour),
            \+ member(Neighbour, Explored),
            \+ member(Neighbour, QueueTail)
        ),
        QueueAdd
    ),
    append(QueueTail, QueueAdd, Queue),
    !,
    next_position_to_explore(Queue, [Next | Explored], Pos).
next_position_to_explore([Failed | QueueTail], Explored, Pos) :-
    !,
    next_position_to_explore(QueueTail, [Failed | Explored], Pos).

% enemy_in_frontier/2
% enemy_in_frontier(-EnemyPos, -Direction)
% Returns the position of an enemy that is on the frontier of known cells,
% meaning that the agents knows nothing about cells "after" the enemy. 
% Direction is the direction from the EnemyPos to an unknown position.
enemy_in_frontier(EnemyPos, Backwards) :-
    % Get a position where there is surely an enemy
    (certain(enemy, EnemyPos) ; certain(teleporter, EnemyPos)),
    % With an adjacent safe position
    adjacent(EnemyPos, SafePos, Dir),
    certain(safe, SafePos),
    % And with an opposite side...
    clockwise(Dir, Perp),
    clockwise(Perp, Backwards),
    adjacent(EnemyPos, UnexploredPos, Backwards),
    % ...that the agent knows nothing about
    unknown(UnexploredPos).

unknown(Pos) :-
    maybe_valid_position(Pos),
    \+certain(visited, Pos),
    \+certain(no_enemy, Pos),
    \+certain(no_teleporter, Pos),
    \+certain(no_pit, Pos),
    \+certain(enemy, Pos),
    \+certain(teleporter, Pos),
    \+certain(pit, Pos),
    \+possible_position(enemy, Pos, _),
    \+possible_position(teleporter, Pos, _),
    \+possible_position(pit, Pos, _),
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
    agent_position(AP),
    a_star(AP, Pos, a_star_heuristic, a_star_extend, [Next | _]),
    next_action(reach(Next), Action),
    !.
next_action(leave, step_out) :-
    % If goal is to leave, and the agent is at (1,1), step out of the cave
    agent_position((1,1)),
    !.
next_action(leave, Action) :-
    % If goal is to leave and the agent is not at (1,1), act to reach (1,1)
    next_action(reach((1,1)), Action),
    !.

next_action(kill(EnemyPos), shoot) :-
    % If goal is to kill, and the agent is adjacent to the enemy in the right direction,
    % shoot
    agent_position(AP),
    facing(Dir),
    adjacent(AP, EnemyPos, Dir),
    !.
next_action(kill(EnemyPos), turn_clockwise) :-
    % If goal is to kill, and the agent is adjacent to the enemy in the wrong direction,
    % turn
    agent_position(AP),
    adjacent(AP, EnemyPos, _),
    !.
next_action(kill(EnemyPos), Action) :-
    % If goal is to kill, and the agent is not next to the enemy,
    % act as if going to the enemy position
    next_action(reach(EnemyPos), Action),
    !.

% perform_action/1
% perform_action(+Action)
perform_action(_) :-
    gen_action_score,
    fail.
perform_action(move_forward) :-
    facing(Direction),
    agent_position(AP),
    adjacent(AP, NP, Direction),
    retractall(agent_position(_)),
    assertz(agent_position(NP)),
    world_step,
    !.
perform_action(turn_clockwise) :-
    facing(Direction),
    clockwise(Direction, NewDirection),
    retractall(facing(_)),
    assertz(facing(NewDirection)),
    !.
perform_action(pick_up) :-
    collected(gold, Count),
    NewCount is Count + 1,
    retractall(collected(gold, _)),
    assertz(collected(gold, NewCount)),
    world_pick_up,
    !.
perform_action(step_out). % Nothing to do
perform_action(shoot) :-
    world_shoot,
    !.


% a_star_heuristic/3
% a_star_heuristic(+Origin, +Goal, -EstCost)
% Estimates the cost from Origin to Goal
a_star_heuristic((X0, Y0), (X1, Y1), H) :-
    H is abs(Y1 - Y0) + abs(X1 - X0).

a_star_extend(Goal, Origin, Next) :-
    adjacent(Origin, Next, _),
    maybe_valid_position(Next),
    (Next = Goal ; certain(safe, Next)).


% frontier_count/4
% frontier_count(+Origin, +Dir, +Rounds, -Count)
% Counts how many new cells can be explored from Origin in up to Rounds moves
frontier_count(Origin, Dir, Rounds, Count) :-
    frontier_extend(Origin, Dir, Rounds, ExtendedFrontier),
    length(ExtendedFrontier, Count),
    !.

% frontier_extend/4
% frontier_extend(+Pos, +Dir, +Steps, -Reacheable)
% Gets a list of unknown cells that can be visited in up to Steps moves from an agent
% located at Pos and facing Dir
frontier_extend(_, _, 0, []) :- !.
frontier_extend(Pos, Dir, Steps, Reacheable) :-
    frontier_extend_(Pos, Dir, Steps, R1),
    delete(R1, Pos, Reacheable),
    !.

% frontier_extend_(+Pos, +Dir, +Steps, -Reacheable)
frontier_extend_(Pos, _, 0, R) :-
    (   maybe_valid_position(Pos)
    ->  R = [Pos]
    ;   R = []
    ), !.
frontier_extend_(Pos, Dir, Steps, Reacheable) :-
    adjacent(Pos, NextPos, Dir),
    clockwise(Dir, NextDir),
    NextSteps is Steps - 1,
    frontier_extend_(NextPos, Dir, NextSteps, R1),
    frontier_extend_(Pos, NextDir, NextSteps, R2),
    setof(
        P,
        (member(P, R1) ; member(P, R2)),
        Reacheable
    ),
    !.
frontier_extend_(_, _, _, []).

% For testing only
% Runs the algorithm until finding a gold position
run_until_done :-
    sense_learn_act(_, A),
    A \= shoot,
    A \= step_out,
    run_until_done,
    !.
run_until_done.
