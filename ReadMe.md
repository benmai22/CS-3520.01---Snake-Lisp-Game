pip# Python and LISP Snake
To run the LISP version of snake it is recommended to be running on Linux and follow the instructions for LISP Install.

## LISP
### Install
Install the prerequisites by running `apt-get install sbcl libsdl2-dev libffi-dev libffi6 libsdl2-ttf-2.0`.

Next install quick LISP with `curl -0 https://beta.quicklisp.org/quicklisp.lisp` and run the file with sbcl using the 
command `sbcl --load quicklisp.lisp`. Once quicklisp is installed it is recommended to enable it at startup by running 
`(ql:add-to-init-file)` while in sbcl (enter sbcl by typing `sbcl`). Next use quicklisp to install sdl2 and opengl with
with the following commands: `(ql:quickload "sdl2")`, `(ql:quickload "sdl2-ttf")`, and `(ql:quickload "cl-opengl")`. 

### Running Snake
To run the snake game run `sbcl --load snake.lsp`.

IF this command fails, try doing the following.

Navigate to directory of "snakege.lsp" in terminal.
Open sbcl with `sbcl`.
Inside sbcl run `(load "~/quicklisp/setup.lisp")`.
Then, still in sbcl, run `(load "snakege.lsp")`.

## Python
### Install
All the following dependencies must be installed.
Install Python3 with `sudo apt-get install python3`.
Install Tkinter with `sudo apt-get install python3.6-tk`.
Install pip for Python3 with `sudo apt-get install python3-pip`.
Install Numpy with `sudo pip3 install -U numpy`.
Install Setup Tools for Python3 with `pip3 install -U setuptools`.
Install Openpyxl with `pip3 install openpyxl`.

### Running Snake
To run Snake with genetic algorithm control, run `python3 Main.py`. You will be asked if you want to enter custom values or use default (answer with `y` or `n`). The program will output a data.xlsx file that logs the progress over time. By default the program will run for 50 generations with a population of 100. You can quit anytime and retain the data up until that point.
