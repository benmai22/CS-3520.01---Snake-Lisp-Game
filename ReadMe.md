#Python and LISP Snake
To run the LISP version of snake it is recommended to be running on Linux and follow the instructions for LISP Install.

##LISP
###Install
Install the prerequisites by running `apt-get install sbcl libsdl2-dev libffi-dev libffi6 libsdl2-ttf-2.0`.

Next install quick LISP with `curl -0 https://beta.quicklisp.org/quicklisp.lisp` and run the file with sbcl using the 
command `sbcl --load quicklisp.lisp`. Once quicklisp is installed it is recommended to enable it at startup by running 
`(ql:add-to-init-file)` while in sbcl (enter sbcl by typing `sbcl`). Next use quicklisp to install sdl2 and opengl with
with the following commands: `(ql:quickload "sdl2")`, `(ql:quickload "sdl2-ttf")`, and `(ql:quickload "cl-opengl")`. 

###Running Snake
To run the snake game run `sbcl --load snake.lsp`.

##Python
###Running Snake
To run a normal game of snake in python run `python Snake.py`.