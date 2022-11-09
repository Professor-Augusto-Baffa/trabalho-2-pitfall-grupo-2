from turtle import onclick
import map, display, player
import searchagent as search

import builtins
import pygame
from pygame.locals import *
import typing

class PathFinder:

    TileIndex = typing.Tuple[int, int]

    def __init__(
        self, map_: map.Map,
        updated_path: typing.Optional[typing.Callable[[str, str, typing.List[TileIndex]], None]] = None,
        found_best_path: typing.Optional[typing.Callable[[str, str, typing.List[TileIndex]], None]] = None,
        finished: typing.Optional[typing.Callable[[], None]] = None
    ) -> None:
        # Map
        self.map = map_
        # Callbacks
        self.updated_path = updated_path
        self.found_best_path = found_best_path
        self.did_finish = finished


        self.goals: str = '123456789ABCDEFGHI'
        self.search_agent = search.SearchAgent(
            cost_function = self.get_cost,
            heuristic_function = self.estimate_cost,
            expansion_function = self.get_neighbours
        )

        # Resetable
        self.paths: typing.Dict[typing.Tuple[str, str], typing.List['self.TileIndex']]
        self.did_start: bool
        self.is_done: bool
        self.current_goal_index: int
        self.generator: typing.Optional[typing.Generator[search.Node[self.TileIndex], None, typing.List[self.TileIndex]]]
        self.reset()

    def reset(self) -> None:
        self.paths = dict()
        self.did_start = False
        self.is_done = False
        self.current_goal_index = 1
        self.generator = None
        self.search_agent.reset()
    
    def _start(self) -> None:
        self.did_start = True
        self.current_goal_index = 1
    
    def _current_goal_is_done(self) -> bool:
        current_start = self.goals[self.current_goal_index - 1]
        current_goal = self.goals[self.current_goal_index]
        current_path = self.paths.get((current_start, current_goal), None)
        return current_path is not None

    def _start_next_goal(self) -> bool:
        if self.current_goal_index == len(self.goals) - 1:
            self._finish()
            return False
        if self._current_goal_is_done():
            # When the first goal is started, it won't be done yet
            self.current_goal_index += 1
        current_start = self.goals[self.current_goal_index - 1]
        current_goal = self.goals[self.current_goal_index]
        start_tile = self.map.get_event_tile(current_start)
        end_tile = self.map.get_event_tile(current_goal)
        self.generator = self.search_agent.get_best_path_generator(start_tile, end_tile)
        return True
    
    def _finish(self) -> None:
        # Set done
        self.is_done = True
        # Use callback
        if self.did_finish is None:
            return
        self.did_finish()

    def _found_best_path(self, path: typing.List[TileIndex]) -> None:
        # Remove generator
        self.generator = None
        # Update saved path
        current_start = self.goals[self.current_goal_index - 1]
        current_goal = self.goals[self.current_goal_index]
        self.paths[current_start, current_goal] = path
        self.search_agent.reset()
        # Use callback
        if self.found_best_path is None:
            return
        self.found_best_path(current_start, current_goal, path)

    def _update_ongoing_path(self, new_node: search.Node) -> None:
        # Update saved path
        path_to_current = self.search_agent.reconstruct_path(self.search_agent.current_node)
        new_path = path_to_current + [new_node.wrapped]
        current_start = self.goals[self.current_goal_index - 1]
        current_goal = self.goals[self.current_goal_index]
        self.paths[current_start, current_goal] = new_path
        # Use callback
        if self.updated_path is None:
            return
        self.updated_path(current_start, current_goal, new_path)


    def update(self) -> None:
        # Check if already done
        if self.is_done:
            return
        # Check if already started
        if not self.did_start:
            self._start()
        # Check if currently looking for a path
        if self.generator == None:
            pending_goal = self._start_next_goal()
            if not pending_goal:
                return
        # Try to get a new value
        try:
            last_visited_node = next(self.generator)
        except StopIteration as e:
            # Done
            found_path = e.value
            self._found_best_path(found_path)
            return
        # Visited a new node
        self._update_ongoing_path(last_visited_node)

    def get_path(self, start: str, end: str) -> typing.Optional[typing.List[TileIndex]]:
        return self.paths.get((start, end), None)

    def get_accumulated_cost(self) -> float:
        return sum((self.get_section_cost(self.goals[i-1], self.goals[i]) for i in range(1, len(self.goals))))

    def get_section_cost(self, start: str, end: str) -> float:
        path = self.paths.get((start, end), None)
        if path is None:
            return 0
        return sum(builtins.map(lambda tile_index: self.get_cost(tile_index), path))

    # Search Agent methods
    
    def get_cost(self, tile: TileIndex) -> float:
        return self.map.get_tile(tile).get_cost()

    def estimate_cost(self, start: TileIndex, end: TileIndex) -> float:
        if start == end: return 0
        return (end[0] - start[0]) + (end[1] - start[1]) + self.get_cost(start) - 1

    def get_neighbours(self, tile: TileIndex) -> typing.List[TileIndex]:
        l = []
        # Up
        if tile[0] > 0: l.append( (tile[0] - 1, tile[1]) )
        # Left
        if tile[1] > 1: l.append( (tile[0], tile[1] - 1) )
        # Right
        if tile[1] < self.map.n_cols - 1: l.append( (tile[0], tile[1] + 1) )
        # Down
        if tile[0] < self.map.n_lines - 1: l.append( (tile[0] + 1, tile[1]) )
        return l


class App:

    def __init__(self):
        pygame.init()

        self.mapa = map.Map.read_from_file("mapa.txt")
        self.visaoDoJogador = map.Map.read_from_file("visaoInicial.txt")
        self.running = True

        self.speed: float = 0.1#180 # Melhor entre 0.1 e 200
        self.retardo = 0
        self.tempoDeRetardo = 10

        self.window = display.Window()
        self.window.add_event_handler('main_quit_handler', pygame.QUIT, lambda e: self.stop_running())

        button_height = 34
        button_v_pos = self.window.height - 20 - button_height

        timer_font_size = 32
        timer_height = timer_font_size + 10
        timer_v_pos = button_v_pos - 10 - timer_height

        map_v_pos = 10
        map_height = timer_v_pos - 20
        map_width = self.window.width - 2 * 10

        self.mapDisplay = display.MapDisplay(self.mapa, (10, map_v_pos), (map_width, map_height))
        self.mapDisplay.set_window(self.window)

        self.player = player.Player(pos = [self.mapDisplay.pos[0],self.mapDisplay.pos[1]+self.mapDisplay.tile_size * self.mapa.n_lines ],size = self.mapDisplay.tile_size, mapa = self.mapa)
        self.player_display = display.PlayerDisplay(self.mapa,self.player.pos,self.mapDisplay.tile_size/2)
        self.player_display.set_window(self.window)

        self.text_to_display = "Sensores: Nenhum alerta"
        self.text_sensors = display.Text(self.text_to_display, [600,50],24)
        self.text_sensors.set_window(self.window)

        self.next_action = display.Text("Ação: Nenhuma", [600,100],24)
        self.next_action.set_window(self.window)
        self.score = 0
        self.ammo = 0
        self.gold = 0
        self.health = 0
        self.score_display = display.Text("Vida:{health}   Score:{score}   Ammo:{ammo}  Gold:{gold}".format(health=self.health,score=self.score,ammo=self.ammo,gold=self.gold), [600,150],24)
        self.score_display.set_window(self.window)

        self.go_button = display.Button('Iniciar', (25, button_v_pos), (100, button_height), on_click=self.change_auto_path)
        self.go_button.set_window(self.window)

        self.move_up_button = display.Button('Forward',(150,button_v_pos),(100,button_height),on_click=self.move_players)
        self.move_up_button.set_window(self.window)
        
        self.move_down_button = display.Button('Rotate',(275,button_v_pos),(100,button_height),on_click=self.rotate_players)
        self.move_down_button.set_window(self.window)

        self.auto_path = False
        self.move_down_button = display.Button('Executa Ação',(400,button_v_pos),(100,button_height),on_click=self.execute_querry)
        self.move_down_button.set_window(self.window)   

        self.path_finder = PathFinder(
            self.mapa, updated_path=self.path_finder_updated_path, found_best_path=self.path_finder_found_best_path,
            finished=self.path_finder_finished_running
        )

        self.path_displays: typing.Dict[typing.Tuple[str, str], display.PathDisplay] = dict()
        self.tempo_total = 0
        self.frame_count = 0
        self.is_path_finder_running = False
        self.is_done = False
        
    def stop_running(self):
        print('Quit')
        self.running = False

    def reset(self):
        self.tempo_total = 0
        self.frame_count = 0
        self.is_path_finder_running = False
        self.is_done = False
        for display in self.path_displays.values():
            display.remove_from_window(self.window)
        self.path_displays.clear()
        self.path_finder.reset()

    def move_players(self):
        self.player.move_forward()

    def rotate_players(self):
        self.player.rotate()
        self.player_display.img = pygame.transform.rotate(self.player_display.img,-90)
    
    def change_auto_path(self):
        self.auto_path =  not self.auto_path
    
    def execute_querry(self):
        #Resposta é um dicionario com Goal, Action e Sensors

        resposta = self.player.cerebro.faz_query("sense_learn_act(Goal,Action), sense_environment(Sensors), print_cave(), get_agent_health(agent, Health), get_game_score(Score), get_inventory(Ammo,PowerUps).")
        self.health = resposta["Health"]
        self.score = resposta["Score"]
        self.ammo = resposta["Ammo"]

        sensores = resposta["Sensors"].replace("(","")
        sensores = sensores.replace(")","")
        sensores = sensores.replace(" ","")
        sensores = sensores.replace(",,",",")
        sensores = sensores.split(',')
        #Checagem de sensores
        if sensores [1] == "steps":
            self.text_sensors.text = "Sensores: Steps"
        elif sensores [2] == "breeze":
            self.text_sensors.text = "Sensores: Breeze"
        elif sensores [3] == "flash":
            self.text_sensors.text = "Sensores: Flash"
        elif sensores [4] == "glow":
            self.text_sensors.text = "Sensores: Glow"
        elif sensores [5] == "impact":
            self.text_sensors.text = "Sensores: Impact"
        elif sensores [6] == "scream":
            self.text_sensors.text = "Sensores: Scream"
        else:
            self.text_sensors.text = "Sensores: Nenhum Alerta"

        self.score_display.text = "Vida:{health}   Score:{score}   Ammo:{ammo}  Gold:{gold}".format(health=self.health,score=self.score,ammo=self.ammo,gold=self.gold)

        if resposta['Health'] <= 0:
            self.auto_path = False
            self.next_action = "Jogador morto"
            return
        #Checagem da acao
        if resposta['Action'] == "turn_clockwise":
            self.rotate_players()
            self.next_action.text = "Ação: Rodar para direita"
            
        elif resposta['Action'] == "move_forward":
            if sensores[5] == "no_impact":
                self.move_players()
                self.next_action.text = "Ação: Mover para frente"

        elif resposta['Action'] == "pick_up":
            self.next_action.text = "Ação: Pegar ouro"
            self.gold += 1

        elif resposta['Action'] == "shoot":
            self.next_action.text = "Ação: Atacar"
        
        elif resposta['Action'] == "step_out":
            self.next_action.text = "Ação: Sair da caverna"
            self.auto_path = False
        else:
            print("Ação não compreendida")

    ###############
    # Path Finder #
    ###############

    def path_finder_updated_path(self, start: str, end: str, path: typing.List[PathFinder.TileIndex]) -> None:
        path_display = self.path_displays.get((start, end), None)
        if path_display is not None:
            path_display.path = path
            return
        path_display = display.PathDisplay(self.mapa, self.mapDisplay.pos, self.mapDisplay.size, path)
        path_display.set_window(self.window)
        self.path_displays[start, end] = path_display

    def path_finder_found_best_path(self, start: str, end: str, path: typing.List[PathFinder.TileIndex]) -> None:
        old_display = self.path_displays.get((start, end), None)
        if old_display is not None:
            self.window.remove_renderer(old_display._renderer_key)
        path_display = display.PathDisplay(self.mapa, self.mapDisplay.pos, self.mapDisplay.size, path, is_done=True)
        path_display.set_window(self.window)
        self.path_displays[start, end] = path_display

    def path_finder_finished_running(self) -> None:
        self.is_path_finder_running = False

    def run(self):
        while self.running:
            if self.is_path_finder_running:
                self.frame_count += 1
                if self.speed < 1.0:
                    n = int(1.0 / self.speed)
                    if self.frame_count % n == 0:
                        self.path_finder.update()
                else:
                    n = int(self.speed)
                    for _ in range(n):
                        if not self.is_path_finder_running:
                            break
                        self.path_finder.update()
            self.window.process_events()
            self.window.display()
            if self.auto_path and self.retardo >= self.tempoDeRetardo:
                self.execute_querry()
                #self.update_map()
                self.retardo = 0
            elif self.retardo < self.tempoDeRetardo:
                self.retardo+=1

        pygame.quit()

    def start_search(self):
        self.is_path_finder_running = True
        return

    def reset_search(self):
        self.reset()

    def stop_search(self):
        self.is_path_finder_running = False
        return
    
    def step_search(self):
        if self.is_path_finder_running:
            return
        if self.is_done:
            return
        self.path_finder.update()

if __name__ == '__main__':
    app = App()
    app.run()