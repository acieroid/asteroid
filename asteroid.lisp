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
(defvar *asteroid-shapes*)
(defvar *min-size* 15)

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
  ((x :accessor pos-x :i:nitarg :x :initform 0)
   (y :accessor pos-y :initarg :y :initform 0)
   (vx :accessor vel-x :initarg :vx :initform 0)
   (vy :accessor vel-y :initarg :vy :initform 0)
   (direction :accessor dir :initarg :dir :initform 0)
   (shape :accessor shape :initform nil)))

(defmethod update :after ((item item))
  (setf (pos-x item)
        (mod (round (+ (pos-x item) (vel-x item))) *width*))
  (setf (pos-y item)
        (mod (round (+ (pos-y item) (vel-y item))) *height*)))

(defclass asteroid (item)
  ((size :accessor size :initarg :size :initform 25)))

(defmethod initialize-instance :after ((asteroid asteroid) &rest initargs)
  ; TODO: write some macro to avoid repetitions like that
  (when (not (find :dir initargs))
    (setf (dir asteroid) (random (* 2 pi))))
  (when (not (find :x initargs))
    (setf (pos-x asteroid) (random *width*)))
  (when (not (find :y initargs))
    (setf (pos-y asteroid) (random *height*)))
  (when (not (find :vx initargs))
    (setf (vel-x asteroid) (random *max-speed*)))
  (when (not (find :vy initargs))
    (setf (vel-y asteroid) (random *max-speed*)))
  (setf (shape asteroid) (random-elt *asteroid-shapes*)))

(defmethod draw ((asteroid asteroid))
  (draw (translate (pos-x asteroid) (pos-y asteroid)
                   (scale (size asteroid)
                          (rotate (dir asteroid) (shape asteroid))))))

(defmethod update ((asteroid asteroid))
  (incf (dir asteroid) 0.1))

(defun point-in-circle (x y r point)
  (<= (+ (expt (- (car point) x) 2)
         (expt (- (cdr point) y) 2))
      (expt r 2)))

; TODO: it assume the asteroid as some sort of circular shape, we
; should use better collision detection, like point-in-polygon algorithm
(defmethod collides ((asteroid asteroid) (shape list))
  (when shape
    (if (point-in-circle (pos-x asteroid) (pos-y asteroid) (size asteroid)
                         (first shape))
        t
        (collides asteroid (rest shape)))))

(defmethod collides ((asteroid asteroid) (item item))
  (collides asteroid
            (translate (pos-x item) (pos-y item)
                       (rotate (dir item) (shape item)))))

(defmethod next-size ((asteroid asteroid))
  (if (< (size asteroid) *min-size*)
      0
      (/ (size asteroid) 2)))

(defmacro spawn-asteroids (n &rest initargs)
  `(loop for i from 1 to ,n
       collect (make-instance 'asteroid ,@initargs)))

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
    (incf (vel-x ship) (- (* (/ *accel* 8) (vel-x ship))))
    (incf (vel-y ship) (- (* (/ *accel* 8) (vel-y ship)))))
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
  (let ((asteroids (spawn-asteroids 4))
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
          (case key
            (:SDL-KEY-SPACE
             (push (make-instance 'bullet
                                  :x (pos-x ship)
                                  :y (pos-y ship)
                                  :dir (dir ship))
                   bullets))))
        (:idle ()
          (sdl:clear-display sdl:*black*)
          (mapcar
           (lambda (bullet)
             (let ((asteroid (first
                              (remove-if-not (lambda (x) (collides x bullet))
                                             asteroids))))
               (when asteroid
                 ; collision between asteroid and bullet
                 (setf (life bullet) 0) ; destroy the bullet
                 (let ((new-size (next-size asteroid)))
                   ; decrease the size and spawn new asteroids
                   (setf (size asteroid) new-size)
                   (nconc asteroids (spawn-asteroids 1
                                                     :x (pos-x asteroid)
                                                     :y (pos-y asteroid)
                                                     :size new-size))
                   ))))
           bullets)
          ; remove end-of-life'd bullets and zero-sized asteroids
          (setf bullets (remove-if-not (lambda (bullet) (> (life bullet) 0))
                                       bullets))
          (setf asteroids (remove-if-not (lambda (asteroid)
                                           (> (size asteroid) 0))
                                         asteroids))
          (mapcar #'update bullets)
          (mapcar #'draw bullets)
          (mapcar #'update asteroids)
          (mapcar #'draw asteroids)
          (when (remove-if-not (lambda (x) (collides x ship)) asteroids)
            (setf (pos-x ship) (/ *width* 2))
            (setf (pos-y ship) (/ *height* 2))
            (setf (vel-x ship) 0)
            (setf (vel-y ship) 0))
          (update ship)
          (draw ship)
          (sdl:update-display))))))

(defun shape-editor (&rest shape)
  (let* ((ofx (/ *width* 2))
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
            (1 (push (cons (- x ofx) (- y ofy)) shape))
            (2 (format t "~a~%" (contain-point shape
                                               (cons (- x ofx) (- y ofy)))))
            ; right click
            (3 (setf shape nil))
            ; wheel up
            (4 (setf shape (scale 1.1 shape)))
            ; wheel down
            (5 (setf shape (scale 0.9 shape))))
          (print-shape shape))
        (:idle ()
          (sdl:clear-display sdl:*black*)
          (draw axis-x)
          (draw axis-y)
          (draw (translate ofx ofy shape))
          (sdl:update-display))))))
