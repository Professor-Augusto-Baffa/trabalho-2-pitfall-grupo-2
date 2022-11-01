from pyswip import Prolog

class PrologQuery():

    def __init__(self):
        self.prolog = Prolog()
        self.prolog.consult("pitfall.pl")

    def faz_querry(self):
        for i,action in enumerate(self.prolog.query("sense_learn_act(Goal,Action).")):
            #print (i)
            return action

    def olha_mapa(self):
        for i,dicionario in enumerate(self.prolog.query("print_cave.")):
            return dicionario