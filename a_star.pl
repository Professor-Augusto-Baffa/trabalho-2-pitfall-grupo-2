%% Adapted from https://swish.swi-prolog.org/p/A*%20Planning.swinb

:- module(a_star, [a_star/5]).

:- meta_predicate a_star(+, +, 3, 2, -).
:- meta_predicate a_star(+, +, +, 3, 2, -).

:- use_module(library(heaps)).


% Add a heap_element(Cost,Position,Path) triple to the search heap
% of open nodes. Carrying Path for interest.
add_to_queue(AccCost, Queue, node(Position,Path), Goal, NewQueue) :-
    a_star_heuristic(Position, Goal, D),
    succ(AccCost, ActCost), % one action has been taken, so incr
    Priority is ActCost + D, % Priority cost
    add_to_heap(Queue, Priority, heap_element(ActCost,Position,Path), NewQueue).

% Add a list of node(Position,Path) Pairs
queue_add_nodes(_, Queue, _, [], _, Queue).
queue_add_nodes(AccCost, Queue, PositionsToAdd, [node(Pos,Path)|NodesTail], Goal, NewQueue) :-
    (   ord_memberchk(Pos, PositionsToAdd)
    ->  add_to_queue(AccCost, Queue, node(Pos,Path), Goal, Qd)
    ;   Qd = Queue
    ),
    queue_add_nodes(AccCost, Qd, PositionsToAdd, NodesTail, Goal, NewQueue).

% Convenience predicate to heap
get_from_queue(Queue, heap_element(AccCost,Position,Path), NewQueue) :-
    get_from_heap(Queue, _Priority, heap_element(AccCost,Position,Path), NewQueue).

nodes_positions(Nodes, Positions) :-
    findall(P, member(node(P,_), Nodes), Positions).


% a_star/5
% a_star(+Origin, +Goal, +Heuristic, +Expand, -Path)
a_star(Origin, Goal, Heuristic, Expand, Path) :-
    % Create heap of open search nodes
    call(Heuristic, Origin, Goal, H),
    %                   heap_element(AccCost,Pos,Path)
    singleton_heap(Queue, H, heap_element(0,Origin,[])),
    % Do the search
    a_star(Queue, Goal, [Origin], Heuristic, Expand, node(Goal,Answer)),
    reverse(Answer, Path),
    !.

% a_star/6
% a_star(+Origin, +Goal, +Visited, +Heuristic, +Expand, -Path)
a_star(Queue, Goal, Visited, Heuristic, Expand, Result) :-
    % Get the most promising node(Position,Path) pair
    get_from_queue(Queue, heap_element(AccCost,Position,Path), RemainingQueue),
    % If we've reached the goal, return the Result
    (   Goal == Position, Result = node(Position,Path)
    % Otherwise (or searching for other solutions), find the
    % reachable positions via some additional position NewP that is
    % recorded onto the path
    ;   setof(node(Next,[Next|Path]), call(Expand, Goal, Position, Next), Nodes),
        % Exclude visited nodes
        nodes_positions(Nodes, ExpandedPositions),
        ord_union(Visited, ExpandedPositions, NewVisited, NewPositions),
        % Add new open nodes (with tag-along processes) to search heap
        queue_add_nodes(AccCost, RemainingQueue, NewPositions, Nodes, Goal, NewQueue),
        % Carry on searching
        a_star(NewQueue, Goal, NewVisited, Heuristic, Expand, Result)
    ).
