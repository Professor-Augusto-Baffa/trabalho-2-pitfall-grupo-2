import random

class Player ():

    life = 100

    def __init__(self,pos,size,mapa):
        self.pos = pos
        self.pos_matriz = [0,mapa.n_lines - 1]#[0,0] é a pos encima e na esqueda
        self.size = size
        self.dir = 0 # 0 = direita, 1 = cima, 2 = esquerda, 3 = baixo
        self.mapa = mapa

    def move_forward(self):
        if self.dir == 0:
            self.pos[0]+=self.size
            self.pos_matriz[0] += 1
        elif self.dir == 1:
            self.pos[1]-=self.size
            self.pos_matriz[1] -= 1    
        elif self.dir == 2:
            self.pos[0]-=self.size
            self.pos_matriz[0] -= 1
        elif self.dir == 3:
            self.pos[1]+=self.size
            self.pos_matriz[1] += 1
        else:
            print("Erro na direcao do movimento")
        print(self.pos_matriz)
    
    def rotate(self):
        self.dir -= 1
        if self.dir < 0:
            self.dir = 3 

    def attack(self,enemy):
        try:
            enemy.takeDamage(random.randint(20,50))
        except all:
            print("Inimigo não possui atributo vida")