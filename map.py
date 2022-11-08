from copy import deepcopy
import typing

class Tile:

    VALID_TYPES = '.dDPTOUB'

    def __init__(self, letter: str) -> None:
        if letter not in Tile.VALID_TYPES:
            raise ValueError(f'Invalid character {letter}')
        self.terrain_type = letter
        self.is_in_path = False
        self.is_in_locked_path = False
        self.is_path_start = False
        self.is_path_end = False
    
    def get_cost(self) -> float:
        if self.terrain_type in self.VALID_TYPES:
            return 1
        #if self.terrain_type == 'R':
        #    return 5
        #if self.terrain_type == 'V':
        #    return 10
        #if self.terrain_type == 'M':
        #    return 200
        #if self.terrain_type == 'P' or self.terrain_type == '#':
        #    return 1e30
        #if self.terrain_type in Tile.VALID_TYPES:
        #    return 0
        raise ValueError(f'Invalid character {self.terrain_type}')
    
    def is_event_tile(self) -> bool:
        return self.terrain_type in '123456789ABCDEFGHI'

class Map:

    def __init__(
        self, matrix: typing.List[typing.List[Tile]], 
        event_tile_indeces: typing.Optional[typing.Dict[str, typing.Tuple[int, int]]] = None
    ) -> None:
        self.matrix = matrix
        self.event_tiles = event_tile_indeces
        self.n_lines = len(matrix)
        if self.n_lines > 0:
            self.n_cols = len(matrix[0])
        else:
            self.n_cols = 0
        return
    
    def _get_tile(self, line: int, col: int) -> typing.Optional[Tile]:
        if line < 0 or line >= self.n_lines:
            return None
        if col < 0 or col >= self.n_cols:
            return None
        return self.matrix[line][col]
    
    def get_tile(self, index: typing.Tuple[int, int]) -> typing.Optional[Tile]:
        line, col = index
        return self._get_tile(line, col)

    def change_tile(self, index: typing.Tuple[int, int], new_type) -> typing.Optional[Tile]:
        line, col = index
        tile = self._get_tile(line, col)
        tile.terrain_type = new_type

    def get_event_tile(self, event: str) -> typing.Optional[typing.Tuple[int, int]]:
        if self.event_tiles is None:
            return None
        return self.event_tiles.get(event, None)

    @staticmethod
    def read_from_file(file_name: str) -> 'Map':
        matrix_map = []
        event_tiles = dict()
        with open(file_name, 'r') as file:
            for line_n, line in enumerate(file):
                matrix_line = []
                line = line.strip()
                if len(line) == 0: continue
                for letter_n, letter in enumerate(line):
                    tile = Tile(letter)
                    matrix_line.append(tile)
                    if not tile.is_event_tile():
                        continue
                    event_tiles[letter] = (line_n, letter_n)
                matrix_map.append(deepcopy(matrix_line))
        
        return Map(matrix_map, event_tiles)