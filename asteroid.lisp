(defvar *width* 800)
(defvar *height* 600)
(defvar *max-speed* 10)
(defvar *ship-size* 25)
(defvar *accel* 0.5)
(defvar *decel* 0.02)
(defvar *angle-step* 0.1)
(defvar *bullet-size* 10)
(defvar *bullet-speed* 10)
(defvar *bullet-life* 50)
(defvar *asteroid-shapes*
  (list
   (make-shape
    (-0.5818636 -0.72732955) (-0.9697728 -0.34911817)
    (-1.0085635 0.05818636) (-0.7564226 0.45579323)
    (-0.32972267 0.30062956) (-0.50428176 0.6885387)
    (-0.09697727 0.8534) (0.31032723 0.72732955) (0.7564226 0.41700226)
    (0.7952135 0.038790904) (0.7952135 -0.3782113)
    (0.52367723 -0.7079341) (0.26183861 -0.8437025)
    (-0.17455909 -0.91158646))
   (make-shape
    (0.81693643 -1.0840118)
    (0.60484713 -1.3275216)
    (-0.58128196 -0.96618444)
    (-0.400613 -0.5262956)
    (0.039275795 0.03142063)
    (-0.5184403 0.64412296)
    (0.149248 0.9897498)
    (1.0054601 0.25136504)
    (0.78551596 -0.6284127)
    (0.19637899 -0.73838496)
    )
   (make-shape
    (0.2926612 -0.70504737)
    (0.19954172 -0.9711031)
    (-0.1463306 -0.9444976)
    (-0.5321113 -0.6518361)
    (-0.7316528 -0.10642224)
    (-0.61192787 0.4655973)
    (-0.23945007 0.7449558)
    (0.22614725 0.67844176)
    (0.7316528 0.4522945)
    (0.9711031 -0.039908342)
    (0.9844062 -0.61192787)
    (0.6252308 -0.9977087))))


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
  `(defmethod ,name (,@args (,shape list))
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
 
(defmethod draw ((shape list))
  (when shape
    (sdl:draw-polygon
     (mapcar (lambda (point)
               (sdl:point :x (car point) :y (cdr point)))
             (cons (car (last shape)) shape)))))

(defmethod print-shape ((shape list))
  (princ "(make-shape") (princ #\Newline)
  (mapcar (lambda (pt) 
            (format t " (~a ~a)~%" (car pt) (cdr pt)))
          shape)
  (princ ")") (princ #\Newline))

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

(defclass bullet (item)
  ((direction :accessor dir :initarg :dir)
   (shape :initform (make-shape (0 0) (*bullet-size* 0)))
   (life :accessor life :initform *bullet-life*)))

(defmethod initialize-instance :after ((bullet bullet) &rest initargs)
  (declare (ignore initargs))
  (setf (vel-x bullet) (* *bullet-speed* (cos (dir bullet))))
  (setf (vel-y bullet) (* *bullet-speed* (sin (dir bullet)))))

(defmethod update ((bullet bullet))
  (decf (life bullet)))

(defmethod draw ((bullet bullet))
  (draw (translate (pos-x bullet) (pos-y bullet)
                   (rotate (dir bullet) (shape bullet)))))

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
    (decf (dir ship) *angle-step*))
  (incf (vel-x ship) (- (* *decel* (vel-x ship))))
  (incf (vel-y ship) (- (* *decel* (vel-y ship)))))

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
                             :vy 0))
        bullets)
    (sdl:with-init ()
      (sdl:window *width* *height*)
      (sdl:with-events ()
        (:quit-event () t)
        (:key-down-event (:key key)
          (when (eq key :SDL-KEY-SPACE)
            (push (make-instance 'bullet
                                 :x (pos-x ship)
                                 :y (pos-y ship)
                                 :dir (dir ship))
                  bullets)))
        (:idle ()
          (sdl:clear-display sdl:*black*)
          (setf bullets (remove-if-not (lambda (bullet) (> (life bullet) 0))
                                       bullets))
          (mapcar #'update bullets)
          (mapcar #'draw bullets)
          (mapcar #'update asteroids)
          (mapcar #'draw asteroids)
          (update ship)
          (draw ship)
          (sdl:update-display))))))

(defun shape-creator ()
  (let* (s
         (ofx (/ *width* 2))
         (ofy (/ *height* 2))
         (axis-x (make-shape (ofx 0) (ofx *height*)))
         (axis-y (make-shape (0 ofy) (*width* ofy))))
    (sdl:with-init ()
      (sdl:window *width* *height*)
      (sdl:with-events ()
        (:quit-event () t)
        (:mouse-button-down-event (:button button :x x :y y)
          (case button
            ; left click
            (1 (push (cons (- x ofx) (- y ofy)) s))
            ; right click
            (3 (setf s nil))
            ; wheel up
            (4 (setf s (scale 1.1 s)))
            ; wheel down
            (5 (setf s (scale 0.9 s))))
          (print-shape s))
        (:idle ()
          (sdl:clear-display sdl:*black*)
          (draw axis-x)
          (draw axis-y)
          (draw (translate ofx ofy s))
          (sdl:update-display))))))