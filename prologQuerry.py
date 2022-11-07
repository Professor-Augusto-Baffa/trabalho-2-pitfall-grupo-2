from pyswip import Prolog

class PrologQuery():

    def __init__(self):
        self.prolog = Prolog()
        self.prolog.consult("pitfall.pl")

    def faz_query(self, querry):
        for i,action in enumerate(self.prolog.query(querry)):
            #print (i)
            return action

    def olha_mapa(self):
        for i,dicionario in enumerate(self.prolog.query("print_cave.")):
            return dicionario

if __name__ == "__main__":
    prolog = PrologQuery()
    while True:
        resposta = prolog.faz_query("sense_learn_act(Goal,Action), facing(Direction), world_position(agent, Position).")
        print("Action = " + resposta["Action"])