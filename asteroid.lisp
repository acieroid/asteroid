(defvar *width* 800)
(defvar *height* 600)
(defvar *max-speed* 10)
(defvar *ship-size* 15)
(defvar *accel* 0.1)
(defvar *angle-step* 0.1)

(defclass shape ()
  ((points :accessor points :initarg :points :initform nil)))

(defmethod translate ((shape shape) x y)
  (make-instance 'shape :points (mapcar (lambda (point)
                                          (cons (+ (car point) x)
                                                (+ (cdr point) y)))
                                        (points shape))))
(defmethod rotate ((shape shape) angle)
  (make-instance 'shape :points (mapcar (lambda (point)
                                          (cons (- (* (cos angle) (car point))
                                                   (* (sin angle) (cdr point)))
                                                (+ (* (sin angle) (car point))
                                                   (* (cos angle) (cdr point)))))
                                        (points shape))))

(defmethod draw ((shape shape))
  (sdl:draw-polygon
   (mapcar (lambda (point)
             (sdl:point :x (car point) :y (cdr point)))
           (cons (car (last (points shape))) (points shape)))))

(defclass item ()
  ((x :accessor pos-x :initarg :x)
   (y :accessor pos-y :initarg :y)
   (vx :accessor vel-x :initarg :vx)
   (vy :accessor vel-y :initarg :vy)))

(defgeneric update (item))
(defgeneric draw (item))

(defmethod update ((item item))
  (setf (pos-x item)
        (mod (round (+ (pos-x item) (vel-x item))) *width*))
  (setf (pos-y item)
        (mod (round (+ (pos-y item) (vel-y item))) *height*)))

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
  ((direction :accessor dir :initarg :dir :initform 0)))

(defmethod update ((ship ship))
  (when (sdl:key-held-p :SDL-KEY-UP)
    (incf (vel-x ship) (* *accel* (cos (dir ship))))
    (incf (vel-y ship) (* *accel* (sin (dir ship)))))
  (when (sdl:key-held-p :SDL-KEY-DOWN)
    (decf (vel-x ship) (* *accel* (cos (dir ship))))
    (decf (vel-y ship) (* *accel* (sin (dir ship)))))
  (when (sdl:key-held-p :SDL-KEY-RIGHT)
    (incf (dir ship) *angle-step*))
  (when (sdl:key-held-p :SDL-KEY-LEFT)
    (decf (dir ship) *angle-step*))
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
             

(defun test-shapes ()
  (let ((s (make-instance 'shape
                          :points '((0 . 0) (0 . 10)
                                    (10 . 10) (10 . 0)))))
    (sdl:with-init ()
      (sdl:window *width* *height*)
      (sdl:with-events ()
        (:quit-event () t)
        (:idle ()
               (sdl:clear-display sdl:*black*)
               (draw (translate (rotate s (/ pi 4)) 100 100))
               (sdl:update-display))))))