(defvar *width* 800)
(defvar *height* 600)
(defvar *max-speed* 10)
(defvar *ship-size* 15)

(defclass item ()
  ((x :accessor pos-x :initarg :x)
   (y :accessor pos-y :initarg :y)
   (vx :accessor vel-x :initarg :vx)
   (vy :accessor vel-y :initarg :vy)))

(defgeneric update (item))
(defgeneric draw (item))

(defmethod update ((item item))
  (setf (pos-x item)
        (mod (+ (pos-x item) (vel-x item)) *width*))
  (setf (pos-y item)
        (mod (+ (pos-y item) (vel-y item)) *height*)))

(defclass asteroid (item)
  ((size :accessor size :initarg :size :initform 10)))

(defmethod draw ((asteroid asteroid))
  (sdl:draw-filled-circle-* (pos-x asteroid)
                            (pos-y asteroid)
                            (size asteroid)
                            :color sdl:*white*))

(defmethod update ((asteroid asteroid))
  (call-next-method))

(defun spawn-asteroid ()
  (make-instance 'asteroid
                 :x (random *width*)
                 :y (random *height*)
                 :vx (random *max-speed*)
                 :vy (random *max-speed*)))

(defun spawn-asteroids (n)
  (loop for i from 0 to n
       collect (spawn-asteroid)))

(defclass ship (item)
  ()) ; TODO: direction

(defmethod update ((ship ship))
  (when (sdl:key-held-p :SDL-KEY-UP)
    (incf (vel-x ship))
    (incf (vel-y ship)))
  (when (sdl:key-held-p :SDL-KEY-DOWN)
    (decf (vel-x ship))
    (decf (vel-y ship)))
  (call-next-method))

(defmethod draw ((ship ship))
  (sdl:draw-box-* (pos-x ship)
                  (pos-y ship)
                  *ship-size*
                  *ship-size*
                  :color sdl:*red*))
                 
(defun start ()
  (let ((asteroids (spawn-asteroids 10))
        (ship (make-instance 'ship
                             :x (/ *width* 2)
                             :y (/ *width* 2)
                             :vx 0
                             :vy 0)))
    (sdl:with-init ()
      (sdl:window *width* *height*)
      (sdl:with-events ()
        (:quit-event () t)
        (:idle ()
          (sdl:clear-display sdl:*black*)
          (mapcar #'update asteroids)
          (mapcar #'draw asteroids)
          (update ship)
          (draw ship)
          (sdl:update-display))))))
             
      