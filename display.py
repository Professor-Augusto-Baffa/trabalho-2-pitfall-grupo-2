import map

import enum
import typing
import pygame

PATH_INDICATORS_ENABLED = True

class Window:

    RendererType = typing.Callable[[pygame.Surface], None]
    EventHandlerType = typing.Callable[[pygame.event.Event], None]

    def __init__(self) -> None:
        self.background_color = pygame.Color("aliceblue")
        self.width = 1200
        self.height = 650
        self.surface = pygame.display.set_mode((self.width, self.height))
        pygame.display.set_caption("Jornada na Terra Media")
        self.renderers: typing.Dict[int, typing.Dict[str, 'Window.RendererType']] = dict()
        self.event_handlers: typing.Dict[int, typing.Dict[str, 'Window.RendererType']] = dict()
    
    def display(self) -> None:
        self.surface.fill(self.background_color)
        for z_pos in sorted(self.renderers.keys()):
            for render in self.renderers[z_pos].values():
                render(self.surface)
        pygame.display.update()

    def add_renderer(self, key: str, renderer: 'Window.RendererType', z_position: int = 0) -> None:
        if self.renderers.get(z_position, None) is None:
            self.renderers[z_position] = dict()
        self.renderers[z_position][key] = renderer
    
    def remove_renderer(self, key: str) -> None:
        for z_pos in self.renderers.keys():
            if self.renderers[z_pos].get(key, None) is None:
                continue
            del self.renderers[z_pos][key]

    def add_event_handler(self, key: str, event_type: int, handler: 'Window.EventHandlerType') -> None:
        handlers = self.event_handlers.get(event_type, None)
        if handlers is None:
            handlers = dict()
            self.event_handlers[event_type] = handlers
        handlers[key] = handler
    
    def remove_event_handler(self, key: str, event_type: typing.Optional[int] = None) -> None:
        if event_type is None:
            for et in self.event_handlers.keys():
                if self.event_handlers[et].get(key, None) is not None:
                    del self.event_handlers[et][key]
                if len(self.event_handlers[et].keys()) == 0:
                    del self.event_handlers[et]
            return
        handlers = self.event_handlers.get(event_type, None)
        if handlers is None:
            return
        if handlers.get(key, None) is not None:
            del handlers[key]
        if len(handlers.keys) > 0:
            return
        del self.event_handlers[event_type]
    
    def process_events(self):
        for event in pygame.event.get():
            handlers = self.event_handlers.get(event.type, None)
            if handlers is None:
                continue
            for handle in handlers.values():
                handle(event)
        return


class Depths(enum.IntEnum):
    MAP = -10
    DEFAULT = 0
    PATH = 10
    ONGOING_PATH = 20
    PLAYER = 20
    TEXT = 30
    BUTTON = 40


class Rendered:
    '''Base class for elements that are drawn to the screen'''

    __instance_seq = 0

    z_position: int = Depths.DEFAULT

    def __init__(self) -> None:
        self._renderer_key = f'{Rendered}.{Rendered.__instance_seq}'
        Rendered.__instance_seq += 1
    
    def set_window(self, window: Window) -> None:
        window.add_renderer(self._renderer_key, self.draw, z_position=self.z_position)

    def remove_from_window(self, window: Window) -> None:
        window.remove_renderer(self._renderer_key)
        window.remove_event_handler(self._renderer_key)
    
    def draw(self, surface: pygame.Surface) -> None:
        '''Override to draw this element on the given surface'''
        pass


class Text(Rendered):

    def __init__(self, text: str, pos: typing.Tuple[int, int], fontsize: int):
        super().__init__()
        self.z_position = Depths.TEXT
        self.text = text
        self.pos = pos

        self.fontname: typing.Optional[str] = None
        self.fontsize = fontsize
        self.fontcolor = pygame.Color('black')
        self.set_font()

    def set_font(self) -> None:
        self.font = pygame.font.Font(self.fontname, self.fontsize)

    def draw(self, surface: pygame.Surface) -> None:
        self.img = self.font.render(self.text, True, self.fontcolor)
        self.rect = self.img.get_rect()
        self.rect.topleft = self.pos
        surface.blit(self.img, self.rect)


class ButtonState(enum.IntEnum):
    NORMAL = enum.auto()
    HIGHLIGHTED = enum.auto()
    PRESSED = enum.auto()


class Button(Rendered):

    def __init__(
        self, text: str, pos: typing.Tuple[int, int], size: typing.Tuple[int, int], 
        on_click: typing.Optional[typing.Callable[[], None]] = None
    ) -> None:
        super().__init__()
        self.z_position = Depths.BUTTON

        self.text = text
        self.pos = pos
        self.size = size
        self.on_click = on_click
        self.state = ButtonState.NORMAL
        
        font_size = min(max(size[1] - 20, 14), 20)
        self.font = pygame.font.SysFont('Arial', font_size)
        self.size = (self.size[0], font_size + 20)
        self.fontcolor = pygame.Color(20, 20, 20)
        self.fillColors: typing.Dict[ButtonState, str] = {
            ButtonState.NORMAL: '#ffffff',
            ButtonState.HIGHLIGHTED: '#666666',
            ButtonState.PRESSED: '#333333',
        }

        self.buttonSurface = pygame.Surface(size)
        self.buttonRect = pygame.Rect(self.pos, self.size)

        self.alreadyPressed = False
    
    def _is_mouse_down(self) -> bool:
        return pygame.mouse.get_pressed()[0]

    def _process_button_down(self, event: pygame.event.Event) -> None:
        if event.type != pygame.MOUSEBUTTONDOWN:
            return
        mouse_pos = pygame.mouse.get_pos()
        if not self.buttonRect.collidepoint(mouse_pos):
            self.state = ButtonState.NORMAL
            return
        # Mouse is inside button
        self.state = ButtonState.PRESSED
    
    def _process_button_up(self, event: pygame.event.Event) -> None:
        if event.type != pygame.MOUSEBUTTONUP:
            return
        mouse_pos = pygame.mouse.get_pos()
        if not self.buttonRect.collidepoint(mouse_pos):
            self.state = ButtonState.NORMAL
            return
        # Mouse is inside button
        self.on_click()
        self.state = ButtonState.HIGHLIGHTED
    
    def _process_mouse_move(self, event: pygame.event.Event) -> None:
        if event.type != pygame.MOUSEMOTION:
            return
        mouse_pos = pygame.mouse.get_pos()
        if not self.buttonRect.collidepoint(mouse_pos):
            # Outside button
            self.state = ButtonState.NORMAL
            return
        # Mouse is inside button
        if self._is_mouse_down():
            self.state = ButtonState.PRESSED
        else:
            self.state = ButtonState.HIGHLIGHTED
    
    def set_window(self, window: Window) -> None:
        super().set_window(window)
        window.add_event_handler(self._renderer_key, pygame.MOUSEBUTTONDOWN, self._process_button_down)
        window.add_event_handler(self._renderer_key, pygame.MOUSEBUTTONUP, self._process_button_up)
        window.add_event_handler(self._renderer_key, pygame.MOUSEMOTION, self._process_mouse_move)

    def draw(self, surface: pygame.Surface) -> None:
        # Get bg color
        bg = self.fillColors.get(self.state, 'ff0000')
        # Get text surface
        img = self.font.render(self.text, True, self.fontcolor, bg)
        # Calculate position to center text on button
        text_size = img.get_size()
        dx = (self.size[0] - text_size[0]) / 2
        dy = (self.size[1] - text_size[1]) / 2
        text_pos = (self.pos[0] + dx, self.pos[1] + dy)
        pygame.draw.rect(surface, bg, self.buttonRect, border_radius=8)
        surface.blit(img, text_pos)


class MapDisplay(Rendered):

    def __init__(self, _map: map.Map, pos: typing.Tuple[float, float], size: typing.Tuple[float, float]) -> None:
        super().__init__()
        self.z_position = Depths.MAP
        self.map = _map
        self.pos = pos
        self.size = size
        tile_width = self.size[0] / self.map.n_cols
        tile_height = self.size[1] / self.map.n_lines
        self.tile_size = min(tile_width, tile_height)

    def draw_tile(self, surface: pygame.Surface, tile: map.Tile, pos_x: float, pos_y: float, size: float) -> None:
        letter = tile.terrain_type
        if letter in 'd':
            color_str = "darkorange"
        elif letter == 'D':
            color_str = "darkorange4"
        elif letter == '.':
            color_str = "palegreen2"
        elif letter == 'P':
            color_str = "gray19"
        elif letter == 'T':
            color_str = "aqua"
        elif letter == 'O':
            color_str = "darkgoldenrod2"
        elif letter == 'U':
            color_str = "firebrick"
        else:
            color_str = "darkgoldenrod1"
        pygame.draw.rect(surface, pygame.Color(color_str), pygame.Rect(pos_x,pos_y,size,size))
    
    def draw(self, surface: pygame.Surface) -> None:
       # tile_width = self.size[0] / self.map.n_cols
       # tile_height = self.size[1] / self.map.n_lines
       # size = min(tile_width, tile_height)
        pos_x = self.pos[0]
        pos_y = self.pos[1]
        for line in self.map.matrix:
            for tile in line:
                # Add 1 to all sides to avoid spaces between squares
                self.draw_tile(surface, tile, pos_x-1, pos_y-1, self.tile_size+1)
                pos_x += self.tile_size
            pos_y += self.tile_size
            pos_x = self.pos[0]

class PlayerDisplay(Rendered):
    
    def __init__(self, _map: map.Map, pos: typing.Tuple[float, float], size: typing.Tuple[float, float]) -> None:
        super().__init__()
        self.z_position = Depths.PLAYER
        self.map = _map
        self.pos = pos
        self.size = size
        self.player_color = "antiquewhite4"
        img = pygame.image.load("guerreiro.png")
        self.img = pygame.transform.scale(img,(self.size*2,self.size*2))

    def draw(self, surface: pygame.Surface) -> None:
        pos_x = self.pos[0]
        pos_y = self.pos[1]
        surface.blit(self.img,(pos_x,pos_y-self.size*2))
        #pygame.draw.circle(surface,self.player_color,(pos_x + self.size,pos_y - self.size),self.size)
        return

class PathDisplay(Rendered):

    def __init__(
        self, _map: map.Map, pos: typing.Tuple[float, float], size: typing.Tuple[float, float], 
        path: typing.List[typing.Tuple[int, int]], is_done: bool = False) -> None:
        super().__init__()
        self.z_position = Depths.PATH if is_done else Depths.ONGOING_PATH
        self.map = _map
        self.pos = pos
        self.size = size
        self.path = path
        self.is_done = is_done
        self._indicate_direction: bool = False
        self._n_dir_indicators: int = 0
        self._dir_indicators: typing.List[int] = []
        if not PATH_INDICATORS_ENABLED: return
        if not is_done: return
        n = len(path)
        if n < 3:
            return
        self._indicate_direction = True
        self._n_dir_indicators = min(int(n / 3), 5)
        self._dir_indicators = [ i + 1 for i in range(self._n_dir_indicators) ]
    
    def _update_dir_indicators(self) -> None:
        for i in range(self._n_dir_indicators):
            self._dir_indicators[i] += 1
            if self._dir_indicators[i] >= len(self.path) - 1:
                self._dir_indicators[i] = 1

    def draw_tile(self, index: typing.Tuple[int, int], color: str, surface: pygame.Surface) -> None:
        tile_width = self.size[0] / self.map.n_cols
        tile_height = self.size[1] / self.map.n_lines
        # Add 1 to all sides to avoid spaces between squares
        size = min(tile_width, tile_height)
        # pos = [x (horizontal), y (vertical)]
        # index = [i (vertical), j (horizontal)]
        pos_x = self.pos[0] + index[1] * size - 1
        pos_y = self.pos[1] + index[0] * size - 1
        pygame.draw.rect(surface, pygame.Color(color), pygame.Rect(pos_x,pos_y,size+1,size+1))

    def _draw_dir_indicators(self, surface: pygame.Surface) -> None:
        min_alpha = 51
        max_alpha = 179
        indicators_surface = pygame.Surface(surface.get_size())
        indicators_surface.set_alpha(255)
        step = (max_alpha - min_alpha) / self._n_dir_indicators
        for i, path_i in enumerate(self._dir_indicators):
            color = pygame.Color(200, 0, 200, int(min_alpha + i * step))
            self.draw_tile(self.path[path_i], color, indicators_surface)
        surface.blit(indicators_surface, (0, 0))

    def draw(self, surface: pygame.Surface) -> None:
        start_color = '#FF0000' if not self.is_done else 'darkgoldenrod1'
        middle_color = '#CC00CC' if not self.is_done else '#222222'
        end_color = '#FFAAFF' if not self.is_done else 'darkgoldenrod1'
        n = len(self.path)
        if n == 0: return
        self.draw_tile(self.path[0], start_color, surface)
        if n == 1: return
        if n > 2:
            for i in range(1, n - 1):
                self.draw_tile(self.path[i], middle_color, surface)
        self.draw_tile(self.path[-1], end_color, surface)
        if self._indicate_direction:
            self._draw_dir_indicators(surface)
            self._update_dir_indicators()
