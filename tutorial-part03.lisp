;;;; tutorial-part03.lisp
;;;; "The Dungeon"
;;;; February 2017
;;;;
;;;; Commentary to be posted on 9bladed.com
;;;;
;;;; Original python+libtcod tutorial, part 3:
;;;; http://www.roguebasin.com/index.php?title=Complete_Roguelike_Tutorial,_using_python%2Blibtcod,_part_3

(ql:quickload :tcod)

(defparameter *screen-width* 80)
(defparameter *screen-height* 50)
(defparameter *limit-fps* 20)

(defvar *map*)
(defparameter *map-width* 80)
(defparameter *map-height* 45)

(defparameter *room-max-size* 10)
(defparameter *room-min-size* 6)
(defparameter *max-rooms* 30)

(tcod:make-color :dark-wall 0 0 100)
(tcod:make-color :dark-ground 50 50 150)

(tcod:console-set-custom-font "arial10x10.png" '(:FONT-TYPE-GREYSCALE :FONT-LAYOUT-TCOD))

(tcod:console-init-root *screen-width* *screen-height*
                        :title "lisp/libtcod tutorial" :fullscreen? nil :renderer :renderer-sdl)
(defparameter *con* (tcod:console-new *screen-width* *screen-height*))

(tcod:sys-set-fps *limit-fps*)

(defgeneric move (obj dx dy)
  (:documentation "move by the given amount"))

(defgeneric draw (obj)
  (:documentation "set the color and then draw the character that represents this object at its position"))

(defgeneric clear (obj)
  (:documentation "erase the character that represents this object"))

(defclass tile ()
  ((blocked
    :initarg :blocked
    :initform (error "Must specify blocked")
    :accessor blocked)
   (block-sight
    :initarg :block-sight
    :initform nil
    :accessor block-sight))
  (:documentation "a tile of the map and its properties"))

(defmethod initialize-instance :after ((tt tile) &key)
  "by default, if a tile is blocked, it also blocks sight"
  (if (not (block-sight tt))
      (setf (block-sight tt) (blocked tt))))

(defgeneric intersect (this other)
  (:documentation "returns true if this rectangle intersects with another one"))

(defclass rect ()
  ((x1
    :initarg :x1
    :initform (error "Must have an x value")
    :accessor x1)
   (y1
    :initarg :y1
    :initform (error "Must have a y value")
    :accessor y1)
   (x2
    :accessor x2)
   (y2
    :accessor y2)
   (center
    :accessor center))
  (:documentation "a rectangle on the map. used to characterize a room."))

(defmethod initialize-instance :after ((r rect) &key width height)
  (setf (x2 r) (+ (x1 r) width))
  (setf (y2 r) (+ (y1 r) height))
  (setf (center r) (cons (floor (/ (+ (x1 r) (x2 r)) 2)) (floor (/ (+ (y1 r) (y2 r)) 2)))))

(defmethod intersect ((this rect) (other rect))
           (and (<= (x1 this) (x2 other)) (>= (x2 this) (x1 other))
                (<= (y1 this) (y2 other)) (>= (y2 this) (y1 other))))

(defun create-room (room)
  ;; go through the tiles in the rectangle and make them passable
  (loop for x from (1+ (x1 room)) below (x2 room)
     do (loop for y from (1+ (y1 room)) below (y2 room)
           do (setf (blocked (aref *map* y x)) nil)
           do (setf (block-sight (aref *map* y x)) nil))))

(defun create-h-tunnel (x1 x2 y)
  (loop for x from (min x1 x2) upto (max x1 x2)
     do (setf (blocked (aref *map* y x)) nil)
     do (setf (block-sight (aref *map* y x)) nil)))

(defun create-v-tunnel (y1 y2 x)
  ;; vertical tunnel
  (loop for y from (min y1 y2) upto (max y1 y2)
     do (setf (blocked (aref *map* y x)) nil)
       (setf (block-sight (aref *map* y x)) nil)))

(defclass object ()
  ((x
    :initarg :x
    :initform (error "Must have an x value")
    :accessor x)
   (y
    :initarg :y
    :initform (error "Must have a y value")
    :accessor y)
   (cha
    :initarg :cha
    :initform (error "Must have a character")
    :accessor cha)
   (color
    :initarg :color
    :initform (error "Must have a color")
    :accessor color))
  (:documentation "this is a generic object: the player, a monster, an item, the stairs...
                   it's always represented by a character on the screen."))

(defmethod move ((obj object) dx dy)
  (if (not (blocked (aref *map* (+ (y obj) dy) (+ (x obj) dx))))
      (progn
        (incf (x obj) dx)
        (incf (y obj) dy))))

(defmethod draw ((obj object))
  (tcod:console-set-default-foreground *con* (tcod:color (color obj)))
  (tcod:console-put-char *con* (x obj) (y obj) (char-code (cha obj)) :NONE))

(defmethod clear ((obj object))
  (tcod:console-put-char *con* (x obj) (y obj) (char-code #\SPACE) :NONE))

(defun handle-keys ()
  (let ((events (tcod:sys-get-events)))
    (loop for event in events
       when (eql (car event) :event-key-press)
       do (cond ((and (eql (tcod:key-vk (cdr event)) :ENTER) (tcod:key-lalt (cdr event)))
                 (tcod:console-set-fullscreen (not (tcod:console-is-fullscreen?))))
                ((eql (tcod:key-vk (cdr event)) :UP) (move *player* 0 -1))
                ((eql (tcod:key-vk (cdr event)) :DOWN) (move *player* 0 1))
                ((eql (tcod:key-vk (cdr event)) :LEFT) (move *player* -1 0))
                ((eql (tcod:key-vk (cdr event)) :RIGHT) (move *player* 1 0))
                ((eql (tcod:key-vk (cdr event)) :ESCAPE) (return-from handle-keys :exit))))))

(defparameter *player* (make-instance 'object
                                      :x 25 :y 23
                                      :cha #\@ :color :white))
(defparameter *npc* (make-instance 'object
                                   :x (- (/ *screen-width* 2) 5) :y (/ *screen-height* 2)
                                   :cha #\@ :color :yellow))
(defparameter *objects* (list *player* *npc*))

(defun make-map ()
  (setf *map* (make-array (list *map-height* *map-width*)))
  (dotimes (i *map-height*)
    (dotimes (j *map-width*)
      (setf (aref *map* i j) (make-instance 'tile :blocked t))))

  (loop with num-rooms = 0
     with rooms
     with w
     with h
     with x
     with y
     with new-room
     with failed
     for r from 0 below *max-rooms* do
       ;; random width and height
       (setf w (tcod:random-get-int tcod:+null+ *room-min-size* *room-max-size*))
       (setf h (tcod:random-get-int tcod:+null+ *room-min-size* *room-max-size*))

       ;; random position without going out of the boundaries of the map
       (setf x (tcod:random-get-int tcod:+null+ 0 (- *map-width* w 1)))
       (setf y (tcod:random-get-int tcod:+null+ 0 (- *map-height* h 1)))

       ;; "rect" class makes rectangles easier to work with
       (setf new-room (make-instance 'rect :x1 x :y1 y :width w :height h))

       ;; run through the other rooms and see if they intersect with this one
       (setf failed nil)
       (loop for other-room in rooms do
            (when (intersect new-room other-room)
              (setf failed t)
              (return)))
       (unless failed
         ;; this means there are no intersections, so this room is valid

         (create-room new-room) ; "paint" it to the map's tiles

         ;; center coordinates of new room, will be useful later
         (let ((new-x (car (center new-room)))
               (new-y (cdr (center new-room))))
           (cond ((eql num-rooms 0)
                  ;; this is the first room, where the player starts at
                  (setf (x *player*) new-x
                        (y *player*) new-y))
                 ;; all rooms after the first:
                 ;; connect it to the previous room with a tunnel
                 (t (let ((prev-x (car (center (car (last rooms))))) ; center coordinates
                          (prev-y (cdr (center (car (last rooms)))))) ; of previous room
                      ;; draw a coin (random number that is either 0 or 1)
                      (cond ((eql (tcod:random-get-int tcod:+null+ 0 1) 1)
                             ;; first move horizontally, then vertically
                             (create-h-tunnel prev-x new-x prev-y)
                             (create-v-tunnel prev-y new-y new-x))
                            (t ; first move vertically, then horizontally
                             (create-v-tunnel prev-y new-y prev-x)
                             (create-h-tunnel prev-x new-x new-y)))))))
         ;; finally, append the new room to the list
         (if (eql num-rooms 0)
             (setf rooms (list new-room))
             (setf rooms (append rooms (list new-room))))
         (incf num-rooms))))

;; generate map (at this point it's not drawn to the screen)
(make-map)

(defun render-all ()
  ;; go through all the tiles, and set their background color
  (dotimes (i *map-height*)
    (dotimes (j *map-width*)
      (if (block-sight (aref *map* i j))
          (tcod:console-set-char-background *con* j i (tcod:color :dark-wall) :set)
          (tcod:console-set-char-background *con* j i (tcod:color :dark-ground) :set))))

  ;; draw all objects in the list
  (mapcar #'draw *objects*)

  ;; blit the contents of con to the root console
  (tcod:console-blit *con* 0 0 *screen-width* *screen-height* tcod:*root* 0 0 1.0 1.0))

(do ()
    ((tcod:console-is-window-closed?))
  (render-all)
  (tcod:console-flush)
  (mapcar #'clear *objects*)
  (handle-keys))
