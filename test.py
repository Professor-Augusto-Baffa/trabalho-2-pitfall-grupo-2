from pyswip import Prolog

prolog = Prolog()
prolog.consult("pitfall.pl")

for i,action in enumerate(prolog.query("sense_learn_act(Goal,Action).")):
    print (i)
    print (action)