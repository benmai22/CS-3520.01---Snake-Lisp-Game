from graphics import *
import random
import math
from enum import Enum

game_size = 50
dot_size = 10
delay = .2

win = GraphWin('Snake', game_size*dot_size, game_size*dot_size)

snake_color = "red"
food_color = "green"

food_pos = (0, 0)
food_rect = Rectangle(Point(0, 0), Point(0, 0))

snake_pos = []
snake_rects = []

score = 0
moves = 0

crashed = False

distance_penalty = -1.5
food_reward = 10


class Move(Enum):
    FORWARD = 1
    LEFT = 2
    RIGHT = 3
    UP = 4
    DOWN = 5


facing = Move.UP


def initFood():
    global food_pos
    global food_rect

    radius = dot_size / 2
    food_pos = (random.randint(0, game_size), random.randint(0, game_size))
    food_rect = Rectangle(Point(dot_size*food_pos[0]-radius, dot_size*food_pos[1]-radius),
                          Point(dot_size*food_pos[0]+radius, dot_size*food_pos[1]+radius))
    food_rect.setFill(food_color)
    food_rect.draw(win)


def moveFood():
    global food_pos
    og = food_pos
    while food_pos in snake_pos:
        food_pos = (random.randint(0, game_size), random.randint(0, game_size))
    dx = food_pos[0] - og[0]
    dy = food_pos[1] - og[1]
    food_rect.move(dx * dot_size, dy * dot_size)


def initSnake():
    radius = dot_size/2
    for point in snake_pos:
        rect = Rectangle(Point(dot_size*point[0]-radius, dot_size*point[1]-radius),
                         Point(dot_size*point[0]+radius, dot_size*point[1]+radius))
        rect.setFill(snake_color)
        rect.setWidth(1)
        rect.draw(win)
        snake_rects.append(rect)


def updateSnake(hdx, hdy):
    global moves, crashed
    moves += 1
    for i in range(len(snake_pos)-1, 0, -1):
        dx = snake_pos[i - 1][0] - snake_pos[i][0]
        dy = snake_pos[i - 1][1] - snake_pos[i][1]
        snake_pos[i] = snake_pos[i-1]
        snake_rects[i].move(dx * dot_size, dy * dot_size)
    snake_pos[0] = (snake_pos[0][0] + hdx, snake_pos[0][1] + hdy)
    snake_rects[0].move(hdx * dot_size, hdy * dot_size)
    if snake_pos[0] == food_pos:
        increaseScore()
    if checkCrash(snake_pos[0]):
        crashed = True
        print("CRASH!!!")

    print(fitness())


def angleToFood():
    global facing, food_pos
    if facing is Move.UP:
        dx = food_pos[0] - snake_pos[0][0]
        dy = food_pos[1] - snake_pos[0][1]
        return math.copysign(math.atan2(dy, math.fabs(dx)) + math.pi / 2, dx)
    elif facing is Move.DOWN:
        dx = snake_pos[0][0] - food_pos[0]
        dy = snake_pos[0][1] - food_pos[1]
        return math.copysign(math.atan2(dy, math.fabs(dx)) + math.pi / 2, dx)
    elif facing is Move.RIGHT:
        dy = snake_pos[0][0] - food_pos[0]
        dx = snake_pos[0][1] - food_pos[1]
        return math.copysign(math.atan2(dy, math.fabs(dx)) + math.pi / 2, -dx)
    elif facing is Move.LEFT:
        dy = food_pos[0] - snake_pos[0][0]
        dx = food_pos[1] - snake_pos[0][1]
        return math.copysign(math.atan2(dy, math.fabs(dx)) + math.pi / 2, -dx)


def checkCrash(coor):
    if coor in snake_pos[1:]:
        return True
    if coor[0] < 0 or coor[1] < 0:
        return True
    if coor[0] > game_size or coor[1] > game_size:
        return True
    return False


def moveSnake(direct):
    global facing
    if direct is Move.LEFT:
        if facing is Move.UP:
            facing = Move.LEFT
        elif facing is Move.LEFT:
            facing = Move.DOWN
        elif facing is Move.DOWN:
            facing = Move.RIGHT
        elif facing is Move.RIGHT:
            facing = Move.UP
    elif direct is Move.RIGHT:
        if facing is Move.UP:
            facing = Move.RIGHT
        elif facing is Move.LEFT:
            facing = Move.UP
        elif facing is Move.DOWN:
            facing = Move.LEFT
        elif facing is Move.RIGHT:
            facing = Move.DOWN

    if facing is Move.UP:
        updateSnake(0, -1)
    elif facing is Move.LEFT:
        updateSnake(-1, 0)
    elif facing is Move.DOWN:
        updateSnake(0, 1)
    elif facing is Move.RIGHT:
        updateSnake(1, 0)


def increaseScore():
    global score
    score += 1
    print("Score: "+str(score)+" in "+str(moves)+" moves")
    moveFood()
    addBody()

# [clear ahead, clear left, clear right, distance to food, angle to food]
def sensorVector():
    in_vector = []
    in_vector.append(int(checkClear(Move.FORWARD)))
    in_vector.append(int(checkClear(Move.LEFT)))
    in_vector.append(int(checkClear(Move.RIGHT)))
    in_vector.append(distanceToFood())
    in_vector.append(angleToFood()/(2*math.pi)+.5)
    return in_vector

def fitness():
    return distanceToFood()*distance_penalty + score*food_reward


def checkClear(direct):
    global facing
    if facing is Move.UP:
        if direct is Move.FORWARD:
            return not checkCrash((snake_pos[0][0], snake_pos[0][1]-1))
        elif direct is Move.LEFT:
            return not checkCrash((snake_pos[0][0] - 1, snake_pos[0][1]))
        elif direct is Move.RIGHT:
            return not checkCrash((snake_pos[0][0] + 1, snake_pos[0][1]))
    elif facing is Move.LEFT:
        if direct is Move.FORWARD:
            return not checkCrash((snake_pos[0][0] - 1, snake_pos[0][1]))
        elif direct is Move.LEFT:
            return not checkCrash((snake_pos[0][0], snake_pos[0][1] + 1))
        elif direct is Move.RIGHT:
            return not checkCrash((snake_pos[0][0], snake_pos[0][1] - 1))
    elif facing is Move.DOWN:
        if direct is Move.FORWARD:
            return not checkCrash((snake_pos[0][0], snake_pos[0][1] + 1))
        elif direct is Move.LEFT:
            return not checkCrash((snake_pos[0][0] + 1, snake_pos[0][1]))
        elif direct is Move.RIGHT:
            return not checkCrash((snake_pos[0][0] - 1, snake_pos[0][1]))
    elif facing is Move.RIGHT:
        if direct is Move.FORWARD:
            return not checkCrash((snake_pos[0][0] + 1, snake_pos[0][1]))
        elif direct is Move.LEFT:
            return not checkCrash((snake_pos[0][0], snake_pos[0][1] - 1))
        elif direct is Move.RIGHT:
            return not checkCrash((snake_pos[0][0], snake_pos[0][1] + 1))


def distanceToFood():
    return math.hypot(snake_pos[0][0]-food_pos[0], snake_pos[0][1]-food_pos[1])


def addBody():
    point = snake_pos[-1]
    radius = dot_size/2
    snake_pos.append(point)
    rect = Rectangle(Point(dot_size * point[0] - radius, dot_size * point[1] - radius),
                     Point(dot_size * point[0] + radius, dot_size * point[1] + radius))
    rect.setFill(snake_color)
    rect.setWidth(1)
    rect.draw(win)
    snake_rects.append(rect)


# Add the head to the snake
snake_pos.append((25, 25))

initSnake()
addBody()
addBody()

initFood()
time.sleep(1)


while not crashed:
    key = win.checkKey()
    direction = Move.FORWARD
    if key == 'Left':
        direction = Move.LEFT
    elif key == 'Right':
        direction = Move.RIGHT
    elif key == 'q':
        break
    moveSnake(direction)
    time.sleep(delay)
