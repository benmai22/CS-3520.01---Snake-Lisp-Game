from graphics import *
import random
import math
from enum import Enum
import numpy as np


# distance_penalty = -1.5
# food_reward = 10


class Move(Enum):
    FORWARD = 0
    LEFT = 1
    RIGHT = 2
    UP = 3
    DOWN = 4


class Snake:
    snake_color = "red"
    food_color = "green"

    random.seed(123)

    def __init__(self, game_size, dot_size, delay, distance_penalty, food_reward):
        self.game_size = game_size
        self.dot_size = dot_size
        self.delay = delay
        self.distance_penalty = distance_penalty
        self.food_reward = food_reward
        self.win = GraphWin('Snake', game_size * dot_size, game_size * dot_size)
        self.food_pos = (0, 0)
        self.food_rect = Rectangle(Point(0, 0), Point(0, 0))
        self.snake_pos = []
        self.snake_rects = []
        self.score = 0
        self.moves = 0
        self.crashed = False
        self.facing = Move.UP
        self.moves_since_score = 0

        self.snake_pos.append((25, 25))
        self.initSnake()
        self.add_body()
        self.add_body()
        self.initFood()

    def initFood(self):
        radius = self.dot_size / 2
        self.food_pos = (random.randint(0, self.game_size), random.randint(0, self.game_size))
        self.food_rect = Rectangle(
            Point(self.dot_size * self.food_pos[0] - radius, self.dot_size * self.food_pos[1] - radius),
            Point(self.dot_size * self.food_pos[0] + radius, self.dot_size * self.food_pos[1] + radius))
        self.food_rect.setFill(self.food_color)
        self.food_rect.draw(self.win)

    def initSnake(self):
        radius = self.dot_size / 2
        for point in self.snake_pos:
            rect = Rectangle(Point(self.dot_size * point[0] - radius, self.dot_size * point[1] - radius),
                             Point(self.dot_size * point[0] + radius, self.dot_size * point[1] + radius))
            rect.setFill(self.snake_color)
            rect.setWidth(1)
            rect.draw(self.win)
            self.snake_rects.append(rect)

    def moveFood(self):
        og = self.food_pos
        while self.food_pos in self.snake_pos:
            self.food_pos = (random.randint(1, self.game_size-1), random.randint(1, self.game_size-1))
        dx = self.food_pos[0] - og[0]
        dy = self.food_pos[1] - og[1]
        self.food_rect.move(dx * self.dot_size, dy * self.dot_size)

    def updateSnake(self, hdx, hdy):
        self.moves += 1
        self.moves_since_score += 1
        for i in range(len(self.snake_pos) - 1, 0, -1):
            dx = self.snake_pos[i - 1][0] - self.snake_pos[i][0]
            dy = self.snake_pos[i - 1][1] - self.snake_pos[i][1]
            self.snake_pos[i] = self.snake_pos[i - 1]
            self.snake_rects[i].move(dx * self.dot_size, dy * self.dot_size)
        self.snake_pos[0] = (self.snake_pos[0][0] + hdx, self.snake_pos[0][1] + hdy)
        self.snake_rects[0].move(hdx * self.dot_size, hdy * self.dot_size)
        if self.snake_pos[0] == self.food_pos:
            self.increase_score()
        if self.check_crash(self.snake_pos[0]):
            self.crashed = True

    def angleToFood(self):
        if self.facing is Move.UP:
            dx = self.food_pos[0] - self.snake_pos[0][0]
            dy = self.food_pos[1] - self.snake_pos[0][1]
            return math.copysign(math.atan2(dy, math.fabs(dx)) + math.pi / 2, dx)
        elif self.facing is Move.DOWN:
            dx = self.snake_pos[0][0] - self.food_pos[0]
            dy = self.snake_pos[0][1] - self.food_pos[1]
            return math.copysign(math.atan2(dy, math.fabs(dx)) + math.pi / 2, dx)
        elif self.facing is Move.RIGHT:
            dy = self.snake_pos[0][0] - self.food_pos[0]
            dx = self.snake_pos[0][1] - self.food_pos[1]
            return math.copysign(math.atan2(dy, math.fabs(dx)) + math.pi / 2, -dx)
        elif self.facing is Move.LEFT:
            dy = self.food_pos[0] - self.snake_pos[0][0]
            dx = self.food_pos[1] - self.snake_pos[0][1]
            return math.copysign(math.atan2(dy, math.fabs(dx)) + math.pi / 2, -dx)

    def check_crash(self, coor):
        if coor in self.snake_pos[1:]:
            return True
        if coor[0] < 0 or coor[1] < 0:
            return True
        if coor[0] > self.game_size or coor[1] > self.game_size:
            return True
        return False

    def check_move(self, direct):
        f = self.facing
        if direct is Move.LEFT:
            if self.facing is Move.UP:
                f = Move.LEFT
            elif self.facing is Move.LEFT:
                f = Move.DOWN
            elif self.facing is Move.DOWN:
                f = Move.RIGHT
            elif self.facing is Move.RIGHT:
                f = Move.UP
        elif direct is Move.RIGHT:
            if self.facing is Move.UP:
                f = Move.RIGHT
            elif self.facing is Move.LEFT:
                f = Move.UP
            elif self.facing is Move.DOWN:
                f = Move.LEFT
            elif self.facing is Move.RIGHT:
                f = Move.DOWN

        if f is Move.UP:
            return self.check_crash((self.snake_pos[0][0], self.snake_pos[0][1]-1))
        elif f is Move.LEFT:
            return self.check_crash((self.snake_pos[0][0] - 1, self.snake_pos[0][1]))
        elif f is Move.DOWN:
            return self.check_crash((self.snake_pos[0][0], self.snake_pos[0][1] + 1))
        elif f is Move.RIGHT:
            return self.check_crash((self.snake_pos[0][0] + 1, self.snake_pos[0][1]))

    def move_snake(self, direct):
        if direct is Move.LEFT:
            if self.facing is Move.UP:
                self.facing = Move.LEFT
            elif self.facing is Move.LEFT:
                self.facing = Move.DOWN
            elif self.facing is Move.DOWN:
                self.facing = Move.RIGHT
            elif self.facing is Move.RIGHT:
                self.facing = Move.UP
        elif direct is Move.RIGHT:
            if self.facing is Move.UP:
                self.facing = Move.RIGHT
            elif self.facing is Move.LEFT:
                self.facing = Move.UP
            elif self.facing is Move.DOWN:
                self.facing = Move.LEFT
            elif self.facing is Move.RIGHT:
                self.facing = Move.DOWN

        if self.facing is Move.UP:
            self.updateSnake(0, -1)
        elif self.facing is Move.LEFT:
            self.updateSnake(-1, 0)
        elif self.facing is Move.DOWN:
            self.updateSnake(0, 1)
        elif self.facing is Move.RIGHT:
            self.updateSnake(1, 0)

    def increase_score(self):
        self.score += 1
        self.moves_since_score = 0
        #print("Score: " + str(self.score) + " in " + str(self.moves) + " moves")
        self.moveFood()
        self.add_body()

    # [clear ahead, clear left, clear right, distance to food, angle to food]
    def sensor_vector(self):
        return [int(self.check_clear(Move.FORWARD)), int(self.check_clear(Move.LEFT)),
                int(self.check_clear(Move.RIGHT)),
                self.distance_to_food() / self.game_size, self.angleToFood() / math.pi, 1]

    def fitness(self):
        return self.moves_since_score*self.distance_penalty + self.score * self.food_reward

    def check_clear(self, direct):
        if self.facing is Move.UP:
            if direct is Move.FORWARD:
                return not self.check_crash((self.snake_pos[0][0], self.snake_pos[0][1] - 1))
            elif direct is Move.LEFT:
                return not self.check_crash((self.snake_pos[0][0] - 1, self.snake_pos[0][1]))
            elif direct is Move.RIGHT:
                return not self.check_crash((self.snake_pos[0][0] + 1, self.snake_pos[0][1]))
        elif self.facing is Move.LEFT:
            if direct is Move.FORWARD:
                return not self.check_crash((self.snake_pos[0][0] - 1, self.snake_pos[0][1]))
            elif direct is Move.LEFT:
                return not self.check_crash((self.snake_pos[0][0], self.snake_pos[0][1] + 1))
            elif direct is Move.RIGHT:
                return not self.check_crash((self.snake_pos[0][0], self.snake_pos[0][1] - 1))
        elif self.facing is Move.DOWN:
            if direct is Move.FORWARD:
                return not self.check_crash((self.snake_pos[0][0], self.snake_pos[0][1] + 1))
            elif direct is Move.LEFT:
                return not self.check_crash((self.snake_pos[0][0] + 1, self.snake_pos[0][1]))
            elif direct is Move.RIGHT:
                return not self.check_crash((self.snake_pos[0][0] - 1, self.snake_pos[0][1]))
        elif self.facing is Move.RIGHT:
            if direct is Move.FORWARD:
                return not self.check_crash((self.snake_pos[0][0] + 1, self.snake_pos[0][1]))
            elif direct is Move.LEFT:
                return not self.check_crash((self.snake_pos[0][0], self.snake_pos[0][1] - 1))
            elif direct is Move.RIGHT:
                return not self.check_crash((self.snake_pos[0][0], self.snake_pos[0][1] + 1))

    def distance_to_food(self):
        return math.hypot(self.snake_pos[0][0] - self.food_pos[0], self.snake_pos[0][1] - self.food_pos[1])

    def add_body(self):
        point = self.snake_pos[-1]
        radius = self.dot_size / 2
        self.snake_pos.append(point)
        rect = Rectangle(Point(self.dot_size * point[0] - radius, self.dot_size * point[1] - radius),
                         Point(self.dot_size * point[0] + radius, self.dot_size * point[1] + radius))
        rect.setFill(self.snake_color)
        rect.setWidth(1)
        rect.draw(self.win)
        self.snake_rects.append(rect)

    def run_snake(self):
        # Add the head to the snake

        time.sleep(1)

        while not self.crashed:
            key = self.win.checkKey()
            direction = Move.FORWARD
            if key == 'Left':
                direction = Move.LEFT
            elif key == 'Right':
                direction = Move.RIGHT
            elif key == 'q':
                break
            self.move_snake(direction)
            time.sleep(self.delay)

