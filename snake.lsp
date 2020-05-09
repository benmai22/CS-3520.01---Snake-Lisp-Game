(require :sdl2)
(require :sdl2-ttf)
(require :cl-opengl)

(defparameter *screen-width* 800)
(defparameter *screen-height* 600)
(defparameter *move-step* 10)
(defparameter *food_size* 7)
(defparameter *max_snake_length* 1000)
(defparameter *frame-delay* 60)
(defparameter *close-delay* 1000)
(defparameter *font* nil)

(defvar *running* nil)
(defvar moves 0)


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
            (incf moves)
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

(defclass poison (food)
  	((red :initform 255)
  	(green :initform 0)
  	(blue :initform 0)
   	(penalty :initform 15 :accessor penalty))
)

(defun make-random-food ()
  (let ((choice (random 2)))
    (case choice
      (0 (make-instance 'good))
      (1 (make-instance 'poison)))))

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
(defmethod eat ((worm worm) (poison poison))
  	(decf (max-length worm) (penalty poison))
  	(setf (body worm)
        (subseq (body worm)
                (penalty poison)
                (length (body worm)))))

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


(defun snake ()
	(setf *running* t)
    (with-window-renderer (window renderer)
		(sdl2-ttf:init)
		(setf *font* (sdl2-ttf:open-font "5ceta_mono.ttf" 10))
			(let ((worm (make-instance 'worm))
				(food (make-random-food))
				(score 0))
			(sdl2:with-event-loop (:method :poll)
  				(:keyup
   					(:keysym keysym)
       					(when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-up)
         					(turn-worm :scancode-up worm))
       					(when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-down)
         					(turn-worm :scancode-down worm))
       					(when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-left)
         					(turn-worm :scancode-left worm))
       					(when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-right)
         					(turn-worm :scancode-right worm))
       					(when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
         					(sdl2:push-event :quit)
         			)
       			)

  				(:idle
   					()
   					(move worm)
   					(clear renderer)
   					(show-message (format nil "Score ~a" score) 0 0 renderer)
   					(draw renderer food)
   					(draw renderer worm)	
   					(when (crashed-p worm)
   						(show-message (format nil "Crashed") (/ *screen-width* 2) (/ *screen-height* 2) renderer)
        				(setf *running* nil))
   					(when (collided worm food)
                		(incf score)
                		(eat worm food)
                		(setf food (make-random-food)))
   					(when (>= 0 (max-length worm))
   						(show-message (format nil "Poisoned to death!") (/ *screen-width* 2) (/ *screen-height* 2) renderer)
                		(setf *running* nil))
   					(sdl2:render-present renderer)
   					(sdl2:delay *frame-delay*)
   					(when (not *running*)
                        (princ "Score: ")
                        (princ score)
                        (princ "  Moves: ")
                        (princ moves)
   						(sdl2:delay *close-delay*)
                		(sdl2:push-quit-event))
  				)
  				(:quit () t)
  			)
  		)
  	)
  	(sdl2-ttf:quit)
)
