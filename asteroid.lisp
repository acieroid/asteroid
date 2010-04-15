(defvar *width* 800)
(defvar *height* 600)
(defvar *max-speed* 10)

(defclass item ()
  ((x :accessor pos-x :initarg :x)
   (y :accessor pos-y :initarg :y)
   (vx :accessor vel-x :initarg :vx)
   (vy :accessor vel-y :initarg :vy)))

(defgeneric update (item))
(defgeneric draw (item))

(defclass asteroid (item)
  ((size :accessor size :initarg :size :initform 10)))

(defmethod draw ((asteroid asteroid))
  (sdl:draw-filled-circle-* (pos-x asteroid)
                            (pos-y asteroid)
                            (size asteroid)
                            :color sdl:*white*))

(defmethod update ((asteroid asteroid))
  (setf (pos-x asteroid)
        (mod (+ (pos-x asteroid) (vel-x asteroid)) *width*))
  (setf (pos-y asteroid)
        (mod (+ (pos-y asteroid) (vel-y asteroid)) *height*)))

(defun spawn-asteroid ()
  (make-instance 'asteroid
                 :x (random *width*)
                 :y (random *height*)
                 :vx (random *max-speed*)
                 :vy (random *max-speed*)))

(defun spawn-asteroids (n)
  (loop for i from 0 to n
       collect (spawn-asteroid)))

(defun start ()
  (let ((asteroids (spawn-asteroids 10)))
    (sdl:with-init ()
      (sdl:window *width* *height*)
      (sdl:with-events ()
        (:quit-event () t)
        (:idle ()
          (sdl:clear-display sdl:*black*)
          (mapcar #'update asteroids)
          (mapcar #'draw asteroids)
          (sdl:update-display))))))
             
      