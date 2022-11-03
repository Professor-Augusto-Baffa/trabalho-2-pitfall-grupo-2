from pyswip import Prolog

class PrologQuery():

    def __init__(self):
        self.prolog = Prolog()
        self.prolog.consult("pitfall.pl")

    def faz_querry(self, querry):
        for i,action in enumerate(self.prolog.query(querry)):
            #print (i)
            return action

    def olha_mapa(self):
        for i,dicionario in enumerate(self.prolog.query("print_cave.")):
            return dicionario