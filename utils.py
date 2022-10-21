
import heapq
import typing

U = typing.TypeVar('U')

class HeapElement(typing.Generic[U]):
    """Element inside the MinHeap
    
    All comparisons operations are performed using the priority only"""

    def __init__(self, element: U, priority: int):
        self.element = element
        self.priority = priority

    def __repr__(self) -> str:
        return f'({self.priority}, {self.element})'
    
    def __eq__(self, __o: object) -> bool:
       if not isinstance(__o, HeapElement):
           raise NotImplementedError
       return self.priority.__eq__(__o.priority)
    
    def __ne__(self, __o: object) -> bool:
        if not isinstance(__o, HeapElement):
            raise NotImplementedError
        return self.priority.__ne__(__o.priority)

    def __lt__(self, __o: object) -> bool:
        if not isinstance(__o, HeapElement):
            raise NotImplementedError
        return self.priority.__lt__(__o.priority)
    
    def __le__(self, __o: object) -> bool:
        if not isinstance(__o, HeapElement):
            raise NotImplementedError
        return self.priority.__le__(__o.priority)
    
    def __gt__(self, __o: object) -> bool:
        if not isinstance(__o, HeapElement):
            raise NotImplementedError
        return self.priority.__gt__(__o.priority)

    def __ge__(self, __o: object) -> bool:
        if not isinstance(__o, HeapElement):
            raise NotImplementedError
        return self.priority.__ge__(__o.priority)

T = typing.TypeVar('T')

class MinHeap(typing.Generic[T]):

    def __init__(self) -> None:
        self.values: typing.List[HeapElement[T]] = []

    def __contains__(self, key: T) -> bool:
        for he in self.values:
            if he.element == key: return True
        return False
    
    def is_empty(self) -> bool:
        return len(self.values) == 0
    
    def reorganize(self) -> None:
        heapq.heapify(self.values)

    def push(self, value: T, priority: int) -> None:
        heapq.heappush(self.values, HeapElement(value, priority))
    
    def pop(self) -> T:
        return heapq.heappop(self.values).element
    
    def peek(self) -> T:
        return self.values[0].element

    def push_then_pop(self, value: T, priority: int) -> T:
        return heapq.heappushpop(self.values, HeapElement(value, priority)).element
    
    def pop_then_push(self, value: T, priority: int) -> T:
        return heapq.heapreplace(self.values, HeapElement(value, priority)).element
    
    def update_priority(self, value: T, priority: int) -> T:
        for stored in self.values:
            if stored.element != value: continue
            stored.priority = priority
            self.reorganize()
            return
        raise ValueError(f'Value {value} is not in the heap')

