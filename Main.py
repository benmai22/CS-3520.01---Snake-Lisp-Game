from Snake import *
import numpy as np
import operator
from openpyxl import Workbook


# gene, 1x6   *   6x4   *   4x3  =  1x3
#      sens_v   gene[0]   gene[1]  move
def randomChromosome():
    return np.random.rand(6, 3)*2-1  # , np.random.rand(4, 3)]


def activation(x):
    return np.tanh(x)


def decide_move(gene, s):
    inputs = np.array(s.sensor_vector())
    mat1 = np.array(gene)
    # mat2 = np.array(gene[1])
    out = activation(inputs.dot(mat1))
    direction = Move(out.argmax())

    return direction


def decide_not_die(gene, s):
    inputs = np.array(s.sensor_vector())
    mat1 = np.array(gene)
    # mat2 = np.array(gene[1])
    out = activation(inputs.dot(mat1))
    direction = Move(out.argmax())
    if snake.check_move(direction):
        possible_moves = [0, 1, 2]
        possible_moves.remove(out.argmax())
        possible_moves.remove(out.argmin())
        direction = Move(possible_moves[0])
    if snake.check_move(direction):
        direction = Move(out.argmin())
    return direction


def crossover(genes):
    new_gene = []
    rows, cols = genes[0].shape
    for i in range(0, rows):
        chrom = []
        for j in range(0, cols):
            chrom.append(genes[np.random.randint(0, len(genes))][i][j])
        new_gene.append(chrom)

    return np.array(new_gene)


def mutate(gene):
    gene[np.random.randint(0, len(gene))][np.random.randint(0, len(gene[0]))] = np.random.rand()
    return gene


def generate_gene(champs):
    genes = [g[2] for g in champs]
    gene = crossover(genes)
    if np.random.rand() < mutation_rate:
        return mutate(gene)
    return gene


game_size = 50
dot_size = 4
delay = 0
distance_penalty = -1.5
food_reward = 10
game_count = 100
number_champs = 3
epochs = 50
data = []
mutation_rate = .001
if input("Use default values [y/n]: ").startswith("n"):
    game_size = int(input("Enter grid size of the play area: "))
    dot_size = int(input("Enter dot size for the pixels: "))
    delay = float(input("Enter render delay in seconds: "))
    distance_penalty = float(input("Enter distance penalty for moving away from food: "))
    food_reward = float(input("Enter the reward for eating food: "))

wb = Workbook()
ws = wb.active
ws.append(['Epoch', 'Champ score', 'Champ moves', 'Champ fitness', 'Average score', 'Average moves', 'Average fitness'])

games = []
for i in range(0, game_count*2):
    snake = Snake(game_size, dot_size, delay, distance_penalty, food_reward)
    gene = randomChromosome()
    while not snake.crashed and snake.moves_since_score < game_size:
        snake.move_snake(decide_not_die(gene, snake))
        time.sleep(delay / 2)
    games.append((snake.fitness(), snake.score, gene))
    snake.win.close()


games.sort(key=operator.itemgetter(0))
games.reverse()
champs = games[0:number_champs]
print(games[0][1])


for epoch in range(0, epochs):
    games = []
    average_score = 0
    average_moves = 0
    average_fitness = 0
    for i in range(0, game_count):
        snake = Snake(game_size, dot_size, delay, distance_penalty, food_reward)
        if i < number_champs:
            gene = champs[i][2]
        else:
            gene = generate_gene(champs)
        while not snake.crashed and snake.moves_since_score < game_size*2:
            if snake.win.checkKey() == 'q':
                if delay > .0000001:
                    delay = 0
                else:
                    delay = .1
            snake.move_snake(decide_not_die(gene, snake))
            time.sleep(delay / 2)
        snake.win.close()
        average_score += snake.score
        average_moves += snake.moves
        average_fitness += snake.fitness()
        games.append((snake.fitness(), snake.score, gene, snake.moves))
    average_score = average_score / game_count
    average_moves = average_moves / game_count
    average_fitness = average_fitness / game_count
    games.sort(key = operator.itemgetter(0))
    games.reverse()
    champs = games[0:number_champs]

    ws.append([epoch, games[0][1], games[0][3], games[0][0], average_score, average_moves, average_fitness])
    wb.save('data.xlsx')
    print("epoch: "+str(epoch)+" top score: "+str(games[0][1])+" average score: "+str(average_score) +
          " average moves: "+str(average_moves))

print(champs[0])
print(champs[1])



