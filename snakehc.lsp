(require :sdl2)
(require :sdl2-ttf)
(require :cl-opengl)

(defparameter *screen-width* 800)
(defparameter *screen-height* 600)
(defparameter *move-step* 10)
(defparameter *food_size* 7)
(defparameter *max_snake_length* 50)
(defparameter *frame-delay* 30)
(defparameter *close-delay* 1000)
(defparameter *font* nil)
(defparameter *pop-size* 10)
(defparameter *learning-steps* 10000)

(defvar *running* nil)
(defvar *ccrash* nil)
(defvar *renderer* nil)
(defvar *runnno* nil)
(defvar population (append '() '()))
(defvar uppopulation (append '() '()))
(defvar moves '() )
(defvar idx 0)

(defclass tex ()
	((renderer
    	:initarg :renderer
    	:initform (error "No renderer provided"))
   	(width
    	:accessor tex-width
    	:initform 0 )
   	(height
	    :accessor tex-height
	    :initform 0)
   	(texture
	    :accessor tex-texture
	    :initform nil)))

(defun render (tex x y &key clip)
  (with-slots (renderer texture width height) tex
    (sdl2:render-copy-ex renderer
                         texture
                         :source-rect clip
                         :dest-rect (sdl2:make-rect x
                                                    y
                                                    (if clip (sdl2:rect-width clip) width)
                                                    (if clip (sdl2:rect-height clip) height))
    ))
)

(defun show-message(text x y renderer)
	(let ((tex (make-instance 'tex :renderer renderer)))
    	(with-slots (renderer texture  width height) tex
      		(let ((surface (sdl2-ttf:render-text-solid *font* text 255 255 255 0)))
        		(setf width (sdl2:surface-width surface))
        		(setf height (sdl2:surface-height surface))
        		(setf texture (sdl2:create-texture-from-surface renderer surface))))
        (render tex x y))
)


(defun clear (renderer)
	(sdl2:set-render-draw-color renderer 0 0 0 255)
  	(sdl2:render-clear renderer))


; worm
(defclass worm ()
  	((head-x :initarg :x :initform (/ *screen-width* 2) :accessor head-x)
   	(head-y :initarg :y :initform (/ *screen-width* 2) :accessor head-y)
   	(max-length :initform *max_snake_length* :accessor max-length)
   	(horizontal-speed :initform 0 :accessor horizontal-speed)
   	(vertical-speed :initform *move-step* :accessor vertical-speed)
   	(body :initform nil :accessor body)
   	(crashed :initform nil :accessor crashed-p))
)

	(defmethod crashed-p :around ((worm worm))
	  	(with-accessors ((x head-x) (y head-y)) worm
	    	(or (call-next-method)
	        	(<= x 0)
	        	(>= x *screen-width*)
	        	(<= y 0)
	        	(>= y *screen-height*)))
  	)

	(defmethod move ((worm worm))
	  	(with-accessors ((x head-x) (y head-y)) worm
	    	(incf x (horizontal-speed worm))
	    	(incf y (vertical-speed worm))
	    	(when (member (cons x y) (body worm) :test #'equal)
	      		(setf (crashed-p worm) t))
	    	(setf (body worm) (append (body worm) (list (cons x y))))
	    	(when (> (length (body worm))
	            	(max-length worm))
	      (pop (body worm))))
	)

  (defmethod cancrash (key (worm worm))
    (with-accessors ((x head-x) (y head-y)) worm
      (with-accessors ((hs horizontal-speed) (vs vertical-speed)) worm
    (setf hs 0)
    (setf vs 0)
    (case key
        (:scancode-up
            (setf vs (* -1 *move-step*)))
        (:scancode-down
            (setf vs *move-step*))
        (:scancode-left
            (setf hs (* -1 *move-step*)))
        (:scancode-right
            (setf hs *move-step*)))  
    (incf x (horizontal-speed worm))
    (incf y (vertical-speed worm))
    (when (member (cons x y) (body worm) :test #'equal)
        (setf *ccrash* t))
    (setf (body worm) (append (body worm) (list (cons x y))))
    (when (> (length (body worm))
            (max-length worm))
    (pop (body worm)))

    (setf hs (* -1 hs))
    (setf vs (* -1 vs))
    (incf x (horizontal-speed worm))
    (incf y (vertical-speed worm))
    (setf (body worm) (append (body worm) (list (cons x y))))
    (when (> (length (body worm))
            (max-length worm))
    (pop (body worm)))))
  )

	(defmethod draw (renderer (worm worm))
 		(multiple-value-bind (rects num)
      		(apply #'sdl2:rects*
            	(loop :for (x . y) in (body worm)
                   		:collect (sdl2:make-rect x y 8 8)))
    		(sdl2:set-render-draw-color renderer 255 0 255 255)
    		(sdl2:render-fill-rects renderer rects num))
 	)


	(defmethod turn-worm (key (worm worm))
  	(with-accessors ((hs horizontal-speed) (vs vertical-speed)) worm
  		(setf hs 0)
  		(setf vs 0)
    	(case key
      		(:scancode-up
       			(unless (= vs *move-step*)
         			(setf vs (* -1 *move-step*))))
      		(:scancode-down
       			(unless (= vs -1)
        			(setf vs *move-step*)))
      		(:scancode-left
       			(unless (= hs *move-step*)
        			(setf hs (* -1 *move-step*))))
      		(:scancode-right
       			(unless (= hs -1)
        			(setf hs *move-step*))))
    	)
  	)

(defmacro with-window-renderer ((window renderer) &body body)
  	`(sdl2:with-init (:everything)
     	(sdl2:with-window (,window
            :title "Snake"
            :w *screen-width*
            :h *screen-height*
            :flags '(:shown))
       		(sdl2:with-renderer (,renderer ,window :index -1 :flags '(:accelerated))
         	,@body)))
)

; food
(defclass food ()
	((x :initarg :x :initform (*(random (/ *screen-width* 10)) 10) :accessor x-loc)
   	(y :initarg :y :initform (*(random (/ *screen-height* 10)) 10) :accessor y-loc)
   	(size :initarg :size :initform *food_size* :accessor size)
   	(red :initarg :red :initform 255 :accessor red)
   	(green :initarg :green :initform 255 :accessor green)
   	(blue :initarg :blue :initform 255 :accessor blue))
)

(defclass good (food)
	((red :initform 255)
  	(green :initform 255)
  	(blue :initform 0)
   	(bonus :initform 25 :accessor bonus))
)

(defun make-random-food ()
      (make-instance 'good))

(defmethod draw (renderer (food food))
  	(with-accessors ((x x-loc)
                   (y y-loc)
                   (size size)
                   (red red)
                   (green green)
                   (blue blue))
  	food
    (sdl2:set-render-draw-color renderer red green blue 255)  
    (sdl2:render-fill-rect renderer (sdl2:make-rect x y size size))))

(defmethod eat ((worm worm) (good good))
  	(incf (max-length worm) (bonus good)))

(defmethod collided ((worm worm) (food food))
  	(with-accessors ((worm-x head-x)
                   (worm-y head-y))
    	worm
    (with-accessors ((food-x x-loc)
                     (food-y y-loc)
                     (size size))
        food
      	(not (or (< worm-x food-x)
            (> worm-x (+ food-x size))
            (< worm-y food-y)
            (> worm-y (+ food-y size))))))
)

(defun turn_worm (worm moves)
  (let ((choice (random 4)))
      (setf *ccrash* nil)
      (case choice
          (0 (turn-worm :scancode-up worm) )
          (1 (turn-worm :scancode-down worm) )
          (2 (turn-worm :scancode-left worm) )
          (3 (turn-worm :scancode-right worm) ))
      (setf moves (append moves 'choice))
  )
)

(defun turn_worm_n (worm moves choice)
  (setf moves (append moves 'choice))
  (case choice
      (0 (turn-worm :scancode-up worm))
      (1 (turn-worm :scancode-down worm))
      (2 (turn-worm :scancode-left worm))
      (3 (turn-worm :scancode-right worm)))
)

(defun snake_run (renderer mov)
  (let ((worm (make-instance 'worm))
    (food (make-random-food))
    (score 0))
  (sdl2:with-event-loop (:method :poll)
      (:keyup
        (:keysym keysym)
            (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
              (sdl2:push-event :quit)
          )
        )

      (:idle
        ()
        (when (> (+ idx 1) (list-length mov))
          ;(turn_worm worm moves)
	  (setf idx 0)
        )
        (turn_worm_n worm moves (nth idx mov))  
	(when *running*
	  (print idx)
	  (write (nth idx mov))
	)
        (setf idx (+ 1 idx))
        ;(turn_worm worm moves)
        (move worm)
	(when *running*
          (clear renderer)
	)
        (show-message (format nil "Score ~a" score) 0 0 renderer)
        (when *running*
          (show-message (format nil "Learning no ~a" *runnno*) 0 15 renderer)
          (draw renderer food)
          (draw renderer worm) 
        ) 
        (when (crashed-p worm)
          (show-message (format nil "Crashed") (/ *screen-width* 2) (/ *screen-height* 2) renderer)
            (setf *running* nil))
        (when (collided worm food)
                (incf score)
                (eat worm food)
                (setf food (make-random-food)))
        (sdl2:render-present renderer)
        (sdl2:delay *frame-delay*)
        ;(when (not *running*)
        ;  (clear renderer)
        ;  (sdl2:push-event :quit)
        ;)
      )

      (:quit () t)
    )
  )
)


; path generation
;(defvar tmpPath)

(defun calc_hamiltonian_cycles_part (grid path targetRow targetCol curRow curCol)
  ;(reduce '* (array-dimensions grid))  ; Path should be this big when we're done

  ; When called, the current space could be T, NIL, or out of bounds.
  ;
  ; If it's NIL, we'll make it T so it's not considered until we back out of the function.
  ; Then we'll consider the 4 adjacent spaces.
  ;
  ; Once we generate a full path, we can check if we ended up adjacent to the
  ; target space.


  (cond
    
    ; Check if the space is in bounds...
    ((or
       (< curRow 0) 
       (< curCol 0)
       (>= curRow (car (array-dimensions grid)) )
       (>= curCol (car (cdr (array-dimensions grid)) ) )
     )
    
      ; This space is out of bounds... reject
      ;(terpri)
      ;(princ "Out of bounds")
      '()
    )

    ; This space is inside bounds... is it T or NIL?
    ((aref grid curRow curCol)

      ; This space has a T -- it's in our current path
      ; Normally we don't care, but if our path is full length AND
      ; we're back at the start, this is good.
      (cond
	((and
	  (= targetRow curRow) 
          (= targetCol curCol) 
          (= (list-length path) (reduce '* (array-dimensions grid)))
         )
          path ; We have a full-length path which returned to the start
	)

	(T
          '()  ; This is not one of those paths... reject it.
	)
      )
      ;(princ "In bounds")
    )
        
    (T
      ; This space has a NIL -- it's not in our current path
      (setf (aref grid curRow curCol) T)

      (terpri)
      (print grid)
      (print path)

      ; We'll store the results of each of the recursive calls
      ; inside tmpPath. If any returns something other than NIL,
      ; that means it was a successful call!
      (let ((tmpPath NIL))
	; Check up
        (setq tmpPath
          (calc_hamiltonian_cycles_part
            grid
            (append path '(0))
            targetRow targetCol 
            (- curRow 1) curCol
          )
        )

	; The following checks all work because (not x) is only true when
	; x is EXACTLY a value of NIL. And NIL is returned only if the
	; given path does not yeild a valid output.

	(when (not tmpPath)
	  ; Check down
          (setq tmpPath
            (calc_hamiltonian_cycles_part
              grid
              (append path '(1))
              targetRow targetCol 
              (+ curRow 1) curCol
            )
          )
	)

	(when (not tmpPath)
	  ; Check left
          (setq tmpPath
            (calc_hamiltonian_cycles_part
              grid
              (append path '(2))
              targetRow targetCol 
              curRow (- curCol 1)
            )
          )
	)

	(when (not tmpPath)
	  ; Check right
          (setq tmpPath
            (calc_hamiltonian_cycles_part
              grid
              (append path '(3))
              targetRow targetCol 
              curRow (+ curCol 1)
            )
          )
	)

	(print tmpPath)

	tmpPath ; Return whatever we've gotten
      )

;     (calc_hamiltonian_cycles_part
;       grid
;       (append path '(0))
;       targetRow targetCol 
;       (- curRow 1) curCol
;     )


      ;(print curRow)
      ;(write curCol)
      ;'()
    )

  ) ; end of cond

  ;print(
	;(loop for row in spaces
	      ;for col in row
	      ;collect col)
	;)
)

(defun calc_hamiltonian_cycles (numRows numCols initRow initCol)

  (calc_hamiltonian_cycles_part
    (make-array (list numRows numCols) :initial-element 'NIL) 
    '()
    initRow initCol ; target
    initRow initCol ; current
  )
)

(defun snake_init ()
  (setf *running* t)
  (with-window-renderer (window renderer)
	   (sdl2-ttf:init)
	   (setf *font* (sdl2-ttf:open-font "5ceta_mono.ttf" 10))
     (setf *renderer* renderer)
     (setf *runnno* 1)
     (setf *running* t)

     ;(calc_hamiltonian_cycles 2 2)

     (setf moves (list 0 2))

     (snake_run *renderer* moves) 
  )
  (sdl2-ttf:quit)
)

;;run
;(snake_init)
(terpri)
(print(calc_hamiltonian_cycles 5 5 4 4))
(terpri)
(terpri)
