(defvar *width* 800)
(defvar *height* 600)
(defvar *max-speed* 10)
(defvar *ship-size* 25)
(defvar *accel* 0.5)
(defvar *angle-step* 0.1)
(defvar *asteroid-shapes*
  (list
   (make-shape (0 1)
               (0.5 0.8)
               (1 0)
               (0.8 0.7)
               (0 -1)
               (-0.4 -0.4)
               (-1 0)
               (-0.5 0.4))))

(defun random-elt (list)
  (elt list (random (length list))))

;;; Shapes
(defmacro make-shape (&rest points)
  `(list
    ,@(loop for p in points
          collect `(cons ,(car p)
                         ,(cadr p)))))
   
(defmacro deftrans (name args x y)
  (let ((shape (gensym))
        (point (gensym)))
  `(defmethod ,name (,@args (,shape cons))
     (mapcar (lambda (,point)
               (let ((p-x (car ,point))
                     (p-y (cdr ,point)))
                 (cons ,x ,y)))
             ,shape))))

(deftrans translate (x y)
  (+ p-x x)
  (+ p-y y))

(deftrans rotate (angle)
  (- (* (cos angle) p-x) (* (sin angle) p-y))
  (+ (* (sin angle) p-x) (* (cos angle) p-y)))

(deftrans scale (factor)
  (* p-x factor)
  (* p-y factor))
 
(defmethod draw ((shape cons))
  (sdl:draw-polygon
   (mapcar (lambda (point)
             (sdl:point :x (car point) :y (cdr point)))
           (cons (car (last shape)) shape))))

(defclass item ()
  ((x :accessor pos-x :initarg :x)
   (y :accessor pos-y :initarg :y)
   (vx :accessor vel-x :initarg :vx)
   (vy :accessor vel-y :initarg :vy)
   (shape :accessor shape :initform nil)))

(defgeneric update (item))
(defgeneric draw (item))

(defmethod update :after ((item item))
  (setf (pos-x item)
        (mod (round (+ (pos-x item) (vel-x item))) *width*))
  (setf (pos-y item)
        (mod (round (+ (pos-y item) (vel-y item))) *height*)))

(defclass asteroid (item)
  ((size :accessor size :initarg :size :initform 10)
   (direction :accessor dir)))

(defmethod initialize-instance :after ((asteroid asteroid) &rest initargs)
  (declare (ignore initargs))
  (setf (shape asteroid) (random-elt *asteroid-shapes*))
  (setf (dir asteroid) (random (* 2 pi))))

(defmethod draw ((asteroid asteroid))
  (draw (translate (pos-x asteroid) (pos-y asteroid)
                   (rotate (dir asteroid)
                           (scale (size asteroid) (shape asteroid))))))

(defmethod update ((asteroid asteroid))
  (incf (dir asteroid) 0.1))

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
  ((direction :accessor dir :initarg :dir :initform 0)
   (shape :initform (make-shape (0 (- (/ *ship-size*) 4))
                                (0 (/ *ship-size* 4))
                                (*ship-size* 0)))))

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
    (decf (dir ship) *angle-step*)))

(defmethod draw ((ship ship))
  (draw (translate (pos-x ship) (pos-y ship)
                   (rotate (dir ship)
                           (shape ship)))))
                 
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
