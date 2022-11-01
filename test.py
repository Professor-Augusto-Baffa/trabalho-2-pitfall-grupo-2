from pyswip import Prolog

class PrologQuerry():

    def __init__(self):
        self.prolog = Prolog()
        self.prolog.consult("pitfall.pl")

    def faz_querry(self):
        for i,action in enumerate(self.prolog.query("sense_learn_act(Goal,Action).")):
            #print (i)
            return action