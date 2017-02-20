;;;; tutorial-part01.lisp
;;;; "Graphics"
;;;; February 2017
;;;;
;;;; Commentary to be posted on 9bladed.com
;;;;
;;;; Original python+libtcod tutorial, part 1:
;;;; http://www.roguebasin.com/index.php?title=Complete_Roguelike_Tutorial,_using_python%2Blibtcod,_part_1

(ql:quickload :tcod)

(defparameter *screen-width* 80)
(defparameter *screen-height* 50)
(defparameter *limit-fps* 20)

(tcod:console-set-custom-font "arial10x10.png" '(:FONT-TYPE-GREYSCALE :FONT-LAYOUT-TCOD))

(tcod:console-init-root *screen-width* *screen-height* :title "lisp/libtcod tutorial" :fullscreen? nil)

(tcod:sys-set-fps *limit-fps*)

(defvar *playerx* (/ *screen-width* 2))
(defvar *playery* (/ *screen-height* 2))

(defun handle-keys ()
  (let ((events (tcod:sys-get-events)))
    (loop for event in events
       when (eql (car event) :event-key-press)
       do (cond ((and (eql (tcod:key-vk (cdr event)) :ENTER) (tcod:key-lalt (cdr event)))
                 (tcod:console-set-fullscreen (not (tcod:console-is-fullscreen?))))
                ((eql (tcod:key-c (cdr event)) #\f)
                 (tcod:console-set-fullscreen (not (tcod:console-is-fullscreen?))))
                ((eql (tcod:key-vk (cdr event)) :UP) (decf *playery*))
                ((eql (tcod:key-vk (cdr event)) :DOWN) (incf *playery*))
                ((eql (tcod:key-vk (cdr event)) :LEFT) (decf *playerx*))
                ((eql (tcod:key-vk (cdr event)) :RIGHT) (incf *playerx*))
                ((eql (tcod:key-vk (cdr event)) :ESCAPE) (return-from handle-keys :exit))))))

(do ()
    ((tcod:console-is-window-closed?))
  (tcod:console-set-default-foreground tcod:*root* (tcod:color :white))
  (tcod:console-put-char tcod:*root* *playerx* *playery* (char-code #\@) :NONE)
  (tcod:console-flush)
  (tcod:console-put-char tcod:*root* *playerx* *playery* (char-code #\SPACE) :NONE)
  (handle-keys))
