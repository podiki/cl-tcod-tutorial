;;;; tutorial-part02.lisp
;;;; "The object and the map"
;;;; February 2017
;;;;
;;;; Commentary to be posted on 9bladed.com
;;;;
;;;; Original python+libtcod tutorial, part 2:
;;;; http://www.roguebasin.com/index.php?title=Complete_Roguelike_Tutorial,_using_python%2Blibtcod,_part_2

(ql:quickload :tcod)

(defparameter *screen-width* 80)
(defparameter *screen-height* 50)
(defparameter *limit-fps* 20)

(defparameter *map-width* 80)
(defparameter *map-height* 45)

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

;; (defmethod draw ((tt tile) &key x y)
;;   (if (block-sight tt)
;;       (tcod:console-set-default-background *con* x y :dark-wall :set)
;;       (tcod:console-set-default-background *con* x y :dark-ground :set)))

(defvar *map*)
(defun make-map ()
  (setf *map* (make-array (list *map-height* *map-width*)))
  (dotimes (i *map-height*)
    (dotimes (j *map-width*)
      (setf (aref *map* i j) (make-instance 'tile :blocked nil))))
  
  (setf (blocked (aref *map* 22 30)) t)
  (setf (block-sight (aref *map* 22 30)) t)
  (setf (blocked (aref *map* 22 50)) t)
  (setf (block-sight (aref *map* 22 50)) t))

(defclass object ()
  ((x
    :initarg :x
    :initform (error "Must have a x value")
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
       do (pprint (cdr event))
         (cond ((or (eql (tcod:key-vk (cdr event)) :ENTER) (tcod:key-lctrl (cdr event)))
                 (print (tcod:key-lctrl (cdr event)))
                 (tcod:console-set-fullscreen (not (tcod:console-is-fullscreen?))))
                ((eql (tcod:key-c (cdr event)) #\f)
                 (pprint (tcod:key-c (cdr event))))
                ((eql (tcod:key-vk (cdr event)) :UP) (move *player* 0 -1))
                ((eql (tcod:key-vk (cdr event)) :DOWN) (move *player* 0 1))
                ((eql (tcod:key-vk (cdr event)) :LEFT) (move *player* -1 0))
                ((eql (tcod:key-vk (cdr event)) :RIGHT) (move *player* 1 0))
                ((eql (tcod:key-vk (cdr event)) :ESCAPE) (return-from handle-keys :exit))))))

(defparameter *player* (make-instance 'object
                                      :x (/ *screen-width* 2) :y (/ *screen-height* 2)
                                      :cha #\@ :color :white))
(defparameter *npc* (make-instance 'object
                                   :x (- (/ *screen-width* 2) 5) :y (/ *screen-height* 2)
                                   :cha #\@ :color :yellow))
(defparameter *objects* (list *player* *npc*))

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
