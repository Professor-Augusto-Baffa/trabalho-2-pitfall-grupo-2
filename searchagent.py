from utils import MinHeap

import typing
from math import inf as infinity


W = typing.TypeVar('W')

class Node(typing.Generic[W]):
    """Node to be used for pathfinding.
    
    Stores an accumulated cost, a cost estimation, the previous node, and a wrapped value.
    """

    def __init__(self, element: W, cost: float, estimation: float) -> None:
        self.wrapped: W = element
        self.previous_node: typing.Optional[Node] = None
        self.accumulated_cost: float = cost
        self.estimated_cost: float = estimation
    
    def __repr__(self) -> str:
        prevWrapped = 'None'
        if self.previous_node is not None:
            prevWrapped = self.previous_node.wrapped.__repr__()
        return f'({self.wrapped}, {prevWrapped}, {self.accumulated_cost}, {self.estimated_cost})'


T = typing.TypeVar('T')

class SearchAgent(typing.Generic[T]):
    '''Implementation of a generic A* search algorithm.
    
    This implementation can be customized through dependency injection on the initializer.
    The `get_best_path` method will return the best path, while the interactive version
    `get_best_path_generator` will return a generator that will return the next potential
    neighbor under consideration by the algorithm. 
    Note: The cost to reach the first element is always zero. The cost to reach any 
    neighbor of a given node N is equal to the cost of N returned by the cost function.
    '''

    def __init__(self,
            cost_function: typing.Callable[[T], float], 
            heuristic_function: typing.Callable[[T, T], float], 
            expansion_function: typing.Callable[[T], typing.List[T]]
        ) -> None:
        '''Create a SearchAgent
        
        Parameters
        ----------
        cost_function
            Function that returns the individual cost of one given element
        heuristic_function
            Function that estimates the cost of the path between two elements.
            This funtion should never overestimate the cost of a path.
        expansion_function
            Funtion that returns all nodes that can be reached for a given node (its neighbors)
        '''
        self.get_cost = cost_function
        self.heuristic = heuristic_function
        self.expand_neighbors = expansion_function
        self.frontier: MinHeap[Node[T]] = MinHeap()
        self.current_node: typing.Optional[Node[T]] = None
        self.possible_next_node: typing.Optional[Node[T]] = None
    
    def reset(self):
        self.frontier = MinHeap()
        self.current_node = None
        self.possible_next_node = None

    def reconstruct_path(self, destination: Node[T]) -> typing.List[T]:
        path: typing.List[T] = [destination.wrapped]
        current = destination
        while current.previous_node is not None:
            current = current.previous_node
            path.append(current.wrapped)
        path.reverse()
        return path

    def _find_path(self, origin: T, goal: T, interactive: bool) -> typing.Generator[Node[T], None, typing.List[T]]:
        wrapped_values: typing.Dict[T, Node[T]] = dict()
        start_heuristic = self.heuristic(origin, goal)
        start_node = Node(origin, 0, start_heuristic)
        wrapped_values[origin] = start_node
        self.frontier.push(start_node, start_node.estimated_cost)

        while not self.frontier.is_empty():
            self.current_node = self.frontier.pop()
            if self.current_node.wrapped == goal:
                return self.reconstruct_path(self.current_node)
            for new_neighbor in self.expand_neighbors(self.current_node.wrapped):
                # The cost of a path is sum {[origin, end)} (not including the end node)
                cost_from_here = self.current_node.accumulated_cost + self.get_cost(self.current_node.wrapped)
                new_neighbor_node = wrapped_values.get(new_neighbor, None)

                if new_neighbor_node is None:
                    # Create a new node with infinity as the accumulated cost
                    # The heuristic does not matter as it will be recalculated
                    new_neighbor_node = Node(new_neighbor, infinity, 0)
                    wrapped_values[new_neighbor] = new_neighbor_node

                if cost_from_here < new_neighbor_node.accumulated_cost:
                    # Found a better path
                    new_neighbor_node.accumulated_cost = cost_from_here
                    new_estimation = self.heuristic(new_neighbor, goal)
                    new_neighbor_node.estimated_cost = cost_from_here + new_estimation
                    new_neighbor_node.previous_node = self.current_node
                    if new_neighbor_node not in self.frontier:
                        self.frontier.push(new_neighbor_node, new_neighbor_node.estimated_cost)
                    else:
                        # Changed cost of existing node in heap
                        self.frontier.update_priority(new_neighbor_node, new_neighbor_node.estimated_cost)
                        self.frontier.reorganize()
                
                if interactive:
                    # For generators that get one trial at a time instead of the best path
                    self.possible_next_node = new_neighbor_node
                    yield self.possible_next_node
        
        # Failed to reach goal
        return []

    def get_best_path(self, origin: T, goal: T) -> typing.List[T]:
        '''Returns the best path from origin to goal'''
        generator = self._find_path(origin, goal, interactive=False)
        # Generator will have a single return if not interactive
        try:
            next(generator)
        except StopIteration as e:
            return e.value
        raise RuntimeError('Unexpected generator state in `SearchAgent.get_best_path()`')
    
    def get_best_path_generator(self, origin: T, goal: T) -> typing.Generator[Node[T], None, typing.List[T]]:
        '''Returns a generator that will interactively find the best path from `origin` to `goal`.
        
        At each call to `next`, the generator will return a Node under consideration as
        the next one on the path. The current node that is being analyzed can be retrieved
        under the `current_node` property and the next possible node under `possible_next_node`. 
        To retrieve the path up to a given node, see `reconstruct_path`.
        Example::

            >>> sa = SearchAgent(
            ...     cost_function=lambda x: x,
            ...     heuristic_function=lambda x,y: y - x,
            ...     expansion_function=lambda x: [x + 1]
            ... )
            >>> gen = sa.get_best_path_generator(0, 5)
            >>> final_path = None
            >>> while True:
            ...     try:
            ...         possible_next_node = next(gen)
            ...         possible_path = sa.reconstruct_path(possible_next_node)
            ...         print(f'{possible_next_node.wrapped}: {possible_path}')
            ...     except StopIteration as e:
            ...         final_path = e.value
            ...         print(final_path)
            ...         break
            ... 
                1: [0, 1]
                2: [0, 1, 2]
                3: [0, 1, 2, 3]
                4: [0, 1, 2, 3, 4]
                5: [0, 1, 2, 3, 4, 5]
                [0, 1, 2, 3, 4 ,5]
        '''
        return self._find_path(origin, goal, interactive=True)

if __name__ == '__main__':
    # Example use
    sa = SearchAgent(
        cost_function=lambda x: x,
        heuristic_function=lambda x,y: y - x,
        expansion_function=lambda x: [x + 1]
    )
    gen = sa.get_best_path_generator(0, 5)
    final_path = None
    while True:
        try:
            possible_next_node = next(gen)
            possible_path = sa.reconstruct_path(possible_next_node)
            print(f'{possible_next_node.wrapped}: {possible_path}')
        except StopIteration as e:
            final_path = e.value
            print(final_path)
            break
