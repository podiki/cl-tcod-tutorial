;;;; tutorial-part11.lisp
;;;; "Dungeon levels and character progression"
;;;; February 2017
;;;;
;;;; Commentary to be posted on 9bladed.com
;;;;
;;;; Original python+libtcod tutorial, part 11:
;;;; http://www.roguebasin.com/index.php?title=Complete_Roguelike_Tutorial,_using_python%2Blibtcod,_part_11

(ql:quickload :tcod)
(ql:quickload :cl-utilities)
(ql:quickload :cl-store)

(defparameter *screen-width* 80)
(defparameter *screen-height* 50)
(defparameter *limit-fps* 20)

;; sizes and coordinates relevant for the GUI
(defparameter *bar-width* 20)
(defparameter *panel-height* 7)
(defparameter *panel-y* (- *screen-height* *panel-height*))

(defvar *game-msgs*)
(defparameter *msg-x* (+ *bar-width* 2))
(defparameter *msg-width* (- *screen-width* *bar-width* 2))
(defparameter *msg-height* (- *panel-height* 1))

(defvar *game-state*)
(defvar *player*)
(defvar *objects*)
(defvar *inventory*)
(defvar *stairs*)
(defvar *dungeon-level*)

(defparameter *inventory-width* 50)
(defparameter +character-screen-width+ 30)

(defparameter +heal-amount+ 4)
(defparameter +lightning-damage+ 20)
(defparameter +lightning-range+ 5)
(defparameter +confuse-num-turns+ 10)
(defparameter +confuse-range+ 8)
(defparameter +fireball-radius+ 3)
(defparameter +fireball-damage+ 12)

;;; experience and level-ups
(defparameter +level-up-base+ 200)
(defparameter +level-up-factor+ 150)
(defparameter +level-screen-width+ 40)

(defparameter *mouse-event* nil)

(defvar *map*)
(defparameter *map-width* 80)
(defparameter *map-height* 43)

(defparameter *room-max-size* 10)
(defparameter *room-min-size* 6)
(defparameter *max-rooms* 30)
(defparameter *max-room-monsters* 3)
(defparameter *max-room-items* 2)

(defvar *fov-map*)
(defparameter *fov-recompute* t)
(defparameter *fov-algo* :FOV-BASIC)
(defparameter *fov-light-walls* t)
(defparameter *torch-radius* 10)

(tcod:make-color :dark-wall 0 0 100)
(tcod:make-color :light-wall 130 110 50)
(tcod:make-color :dark-ground 50 50 150)
(tcod:make-color :light-ground 200 180 50)

(defun menu (header options width)
  (if (> (length options) 26)
      (error "Cannot have a menu with more than 26 options."))
  ;; calculate total height for the header (after auto-wrap) and one line per option
  (let* ((header-height (progn (if (string= header "")
                                   0
                                   (tcod:console-get-height-rect *con* 0 0 width
                                                                 *screen-height* header))))
         (height (+ (length options) header-height))
         ;; create an off-screen console that represents the menu's window
         (window (tcod:console-new width height)))
    ;; print the header, with auto-wrap
    (tcod:console-set-default-foreground window (tcod:color :white))
    (tcod:console-print-rect-ex window 0 0 width height :none :left header)

    ;; print all the options
    (loop for option-text in options
       for y from header-height
       for letter-index from (char-code #\a) do
         (tcod:console-print-ex window 0 y :none :left
                                (format nil "(~a) ~a" (code-char letter-index) option-text)))

    ;; blit the contents of "window" to the root console
    (let ((x (floor (- (/ *screen-width* 2) (/ width 2))))
          (y (floor (- (/ *screen-height* 2) (/ height 2)))))
      (tcod:console-blit window 0 0 width height tcod:*root* x y 1.0 0.7)))

  ;; present the root console to the player and wait for a key-press
  (tcod:console-flush)

  (let* ((event (cdr (tcod:sys-wait-events :event-key-press t)))
         (key (tcod:key-c event))
         (index (- (char-code key) (char-code #\a))))
    (if (and (eql (tcod:key-vk event) :ENTER) (tcod:key-lalt event))
        (tcod:console-set-fullscreen (not (tcod:console-is-fullscreen?))))
    ;; convert the ASCII code to an index; if it corresponds to an option, return it
    (if (and (>= index 0) (< index (length options)))
        (return-from menu index))))

(defun inventory-menu (header)
  ;; show a menu with each item of the inventory as an option
  (let (options index)
    (if (eql (length *inventory*) 0)
        (setf options (list "Inventory is empty."))
        (setf options (mapcar #'name *inventory*)))
    (setf index (menu header options *inventory-width*))
    ;; if an item was chosen, return it
    (unless (or (not index) (eql (length *inventory*) 0))
      (return-from inventory-menu (nth index *inventory*)))))

(defun msgbox (text &optional (width 50))
  (menu text nil width)) ; use menu as a sort of "message box"

(defun render-bar (x y total-width name value maximum bar-color back-color)
  ;; render a bar (HP, experience, etc.). first calculate the width of the bar
  (let ((bar-width (round (* (/ value maximum) total-width))))
    ;; render the background first
    (tcod:console-set-default-background *panel* (tcod:color back-color))
    (tcod:console-rect *panel* x y total-width 1 nil :screen)

    ;;now render the bar on top
    (tcod:console-set-default-background *panel* (tcod:color bar-color))
    (if (> bar-width 0)
        (tcod:console-rect *panel* x y bar-width 1 nil :screen))

    ;; finally, some centered text with the values
    (tcod:console-set-default-foreground *panel* (tcod:color :white))
    (tcod:console-print-ex *panel* (+ x (/ total-width 2)) y :none :center
                           "~a: ~a/~a" name value maximum)))

(defun message (new-message &optional (color :white))
  ;; split the message if necessary, among multiple lines
  (let* ((words (cl-utilities:split-sequence #\space new-message))
         (format-string (format nil "~a~a~a" "~{~<~%~1," *msg-width* ":;~A~> ~}"))
         (new-lines (format nil "~@?" format-string words))
         (new-msg-lines (cl-utilities:split-sequence #\newline new-lines)))
    (dolist (line new-msg-lines)
      ;; if the buffer is full, remove the first line to make room for the new one
      ;; add the new line as a tuple, with the text and the color
      (if (eql (length *game-msgs*) *msg-height*)
          (setf *game-msgs* (append (cdr *game-msgs*) (list (cons line color))))
          (setf *game-msgs* (append  *game-msgs* (list (cons line color))))))))

(defgeneric move (obj dx dy)
  (:documentation "move by the given amount"))

(defgeneric move-towards (obj target-x target-y)
  (:documentation "move towards target"))

(defgeneric distance-to (obj other)
  (:documentation "return the distance to another object"))

(defgeneric distance (obj x y)
  (:documentation "return the distance to some coordinates"))

(defgeneric draw (obj)
  (:documentation "set the color and then draw the character that represents this object at its position"))

(defgeneric clear (obj)
  (:documentation "erase the character that represents this object"))

(defgeneric send-to-back (obj)
  (:documentation "send object to beginning of list, to be drawn first and overwritten"))

(defclass tile ()
  ((blocked
    :initarg :blocked
    :initform (error "Must specify blocked")
    :accessor blocked)
   (block-sight
    :initarg :block-sight
    :initform nil
    :accessor block-sight)
   (explored
    :initform nil
    :accessor explored))
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

(defun is-blocked (x y)
  ;; first test the map tile
  (if (blocked (aref *map* y x))
      t
      ;; now check for any blocking objects
      (dolist (obj *objects*)
        (if (and (blocks obj) (eql (x obj) x) (eql (y obj) y))
            (return t)))))

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
    :accessor color)
   (name
    :initarg :name
    :initform (error "Must have a name")
    :accessor name)
   (blocks
    :initarg :blocks
    :initform nil
    :accessor blocks)
   (always-visible
    :initarg :always-visible
    :initform nil
    :accessor always-visible)
   (ai
    :initarg :ai
    :initform nil
    :accessor ai))
  (:documentation "this is a generic object: the player, a monster, an item, the stairs...
                   it's always represented by a character on the screen."))

(defmethod move ((obj object) dx dy)
  (if (not (is-blocked (+ (x obj) dx) (+ (y obj) dy)))
      (progn
        (incf (x obj) dx)
        (incf (y obj) dy))))

(defmethod move-towards ((obj object) target-x target-y)
  ;; vector from this object to the target, and distance
  (let* ((dx (- target-x (x obj))) (dy (- target-y (y obj)))
         (distance (sqrt (+ (expt dx 2) (expt dy 2)))))
    ;; normalize it to length 1 (preserving direction), then round it and
    ;; convert to integer so the movement is restricted to the map grid
    (setf dx (round (/ dx distance))
          dy (round (/ dy distance)))
    (move obj dx dy)))

(defmethod distance-to ((obj object) (other object))
  (let ((dx (- (x other) (x obj))) (dy (- (y other) (y obj))))
    (sqrt (+ (expt dx 2) (expt dy 2)))))

(defmethod distance ((obj object) x y)
  ;; return the distance to some coordinates
  (sqrt (+ (expt (- x (x obj)) 2) (expt (- y (y obj)) 2))))

(defmethod move-or-attack ((obj object) dx dy)
  ;;the coordinates the player is moving to/attacking
  (let ((x (+ (x obj) dx)) (y (+ (y obj) dy)) (target nil))
    ;;try to find an attackable object there
    (dolist (object *objects*)
      (when (and (ai object) (eql (x object) x) (eql (y object) y))
          (setf target object)
          (return)))

    ;; attack if target found, move otherwise
    (if target
        (attack *player* target)
        (progn (move obj dx dy)
               (setf *fov-recompute* t)))))

(defmethod draw ((obj object))
  "only show if it's visible to the player; or it's set to always visible
  and on an explored tile"
  (let ((x (x obj)) (y (y obj)))
    (when (or (tcod:map-is-in-fov? *fov-map* x y)
              (and (always-visible obj) (explored (aref *map* y x))))
      (tcod:console-set-default-foreground *con* (tcod:color (color obj)))
      (tcod:console-put-char *con* x y (char-code (cha obj)) :NONE))))

(defmethod clear ((obj object))
  (tcod:console-put-char *con* (x obj) (y obj) (char-code #\SPACE) :NONE))

(defmethod send-to-back ((obj object))
  ;; make this object be drawn first, so all others appear above it if they're in the same tile
  (setf *objects* (remove obj *objects*))
  (push obj *objects*))

(defgeneric pick-up (thing)
  (:documentation "add to the player's inventory and remove from the map"))

(defgeneric use (self)
  (:documentation "call a use-function"))

(defgeneric drop (self)
  (:documentation "add to the map and remove from the player's inventory.
                   also, place it at the player's coordinates"))

(defclass item (object)
  ((use-function
    :initarg :use-function
    :initform nil
    :accessor use-function))
  (:documentation "an item that can be picked up and used"))

(defmethod pick-up ((obj item))
  (if (>= (length *inventory*) 26)
      (message (format nil "Your inventory is full, cannot pick up ~a." (name obj)) :red)
      (progn (setf *inventory* (append *inventory* (list obj))
                   *objects* (remove obj *objects*))
             (message (format nil "You picked up a ~a!" (name obj)) :green))))

(defmethod use ((self item))
  ;; just call the "use-function" if it is defined
  (if (not (use-function self))
      (message (format nil "The ~a cannot be used." (name self)))
      (unless (eql (funcall (use-function self)) :canceled)
        ;; destroy after use, unless it was canceled for some reason
        (setf *inventory* (remove self *inventory*)))))

(defmethod drop ((self item))
  (setf *objects* (append *objects* (list self))
        *inventory* (remove self *inventory*)
        (x self) (x *player*)
        (y self) (y *player*))
  (message (format nil "You dropped a ~a." (name self)) :yellow))

(defun cast-heal ()
  ;; heal the player
  (when (eql (hp *player*) (max-hp *player*))
    (message "You are already at full health." :red)
    (return-from cast-heal :canceled))

  (message "Your wounds start to feel better!" :purple)
  (heal *player* +heal-amount+))

(defun closest-monster (max-range)
  "find closest enemy, up to a maximum range, and in the player's FOV"
  (loop for obj in *objects*
        ;; start with (slightly more than) maximum range
        with closest-dist = (1+ max-range)
        if (and (typep obj 'fighter) (not (eql obj *player*))
                (tcod:map-is-in-fov? *fov-map* (x obj) (y obj)))
          ;; calculate distance between this object and the player
          if (< (distance-to *player* obj) closest-dist)
            collect obj into closest-enemy
            and do (setf closest-dist (distance-to *player* obj))
        finally (return (car (last closest-enemy)))))

(defun cast-lightning ()
  ;; find closest enemy (inside a maximum range) and damage it
  (let ((monster (closest-monster +lightning-range+)))
    (unless monster ; no enemy found within maximum range
      (message "No enemy is close enough to strike." :red)
      (return-from cast-lightning :canceled))

    ;; zap it!
    (message (format nil "A lightning bolt strikes the ~a with a loud thunder! ~
                          The damage is ~a hit points." (name monster) +lightning-damage+)
             :light-blue)
    (take-damage monster +lightning-damage+)))

(defun cast-confuse ()
  ;; ask the player for a target to confuse
  (message "Left-click an enemy to confuse it, or right-click to cancel." :cyan)
  (let ((monster (target-monster +confuse-range+)))
    (if (not monster)
      (return-from cast-confuse :canceled))

    ;; replace the monster's AI with a "confused" one; after some turns it will restore the old AI
    (let ((old-ai (ai monster)))
      (setf (ai monster) (lambda (mon) (confused-monster mon old-ai))))
    (message (format nil "The eyes of the ~a look vacant, as he starts to stumble around!"
                     (name monster)) :light-green)))

(defun cast-fireball ()
  ;; ask the player for a tile to throw a fireball at
  (message "Left-click a target tile for the fireball, or right-click to cancel." :cyan)
  (let* ((coords (target-tile))
         (x (car coords))
         (y (cdr coords)))
    (if (not coords)
        (return-from cast-fireball :canceled))
    (message (format nil "The fireball explodes, burning everything within ~a tiles!"
                     +fireball-radius+) :orange)

    (dolist (obj *objects*)
      (when (and (<= (distance obj x y) +fireball-radius+)
                 (typep obj 'fighter))
        (message (format nil "The ~a gets burned for ~a hit points." (name obj) +fireball-damage+)
                 :orange)
        (take-damage obj +fireball-damage+)))))

(defun target-tile (&optional (max-range nil))
  "return the position of a tile left-clicked in layer's FOV (optionally in a range),
   or nil if right-clicked"
  (do (events x y)
      ;; render the screen. this erases the inventory and shows the names of objects
      ;; under the mouse.
      (nil)
    (tcod:console-flush)
    (setf events (tcod:sys-get-events))
    (render-all)

    (dolist (event events)
      (when (eql (car event) :event-mouse-press)
        (setf x (tcod:mouse-cx (cdr event))
              y (tcod:mouse-cy (cdr event)))
        ;; accept the target if the player clicked in FOV, and in case a range is specified,
        ;; if it's in that range
        (if (and (tcod:mouse-lbutton (cdr event))
                 (tcod:map-is-in-fov? *fov-map* x y)
                 (or (not max-range) (<= (distance *player* x y) max-range)))
            (return-from target-tile (cons x y)))
        ;; cancel if the player right-clicked or pressed Escape
        (if (tcod:mouse-rbutton (cdr event))
            (return-from target-tile nil)))
      (if (and (eql (car event) :event-key-press)
               (eql (tcod:key-vk (cdr event)) :ESCAPE))
          (return-from target-tile nil)))))

(defun target-monster (&optional max-range)
  "returns a clicked monster inside FOV up to a range, or NIL if right-clicked"
  (do ((coords (target-tile max-range) (target-tile max-range)))
      ;; player canceled
      ((not coords) nil)

    ;; return the first clicked monster, otherwise continue looping
    (dolist (obj *objects*)
      (if (and (eql (x obj) (car coords)) (eql (y obj) (cdr coords))
               (typep obj 'fighter) (not (eql obj *player*)))
          (return-from target-monster obj)))))

(defclass fighter (object)
  ((max-hp
    :initarg :max-hp
    :initform (error "Must provide max-hp")
    :accessor max-hp)
   (hp
    :accessor hp)
   (defense
     :initarg :defense
     :initform (error "Must provide defense")
     :accessor defense)
   (power
    :initarg :power
    :initform (error "Must provide power")
    :accessor power)
   (xp
    :initarg :xp
    :initform (error "Must provide xp")
    :accessor xp)
   (level
    :initarg :level
    :initform 1
    :accessor level)
   (death-fun
    :initarg :death-fun
    :initform nil
    :accessor death-fun))
  (:documentation "fighter class for players, npcs, monsters"))

(defmethod initialize-instance :after ((f fighter) &key)
  (setf (hp f) (max-hp f)))

(defgeneric heal (self amount)
  (:documentation "heal"))

(defgeneric take-damage (self damage)
  (:documentation "take damage"))

(defgeneric attack (self target)
  (:documentation "attack target"))

(defmethod attack ((self fighter) target)
  ;; a simple formula for attack damage
  (let ((damage (- (power self) (defense target))))
    (if (> damage 0)
        ;; make the target take some damage
        (progn (message (format nil "~&~@(~a~) attacks ~a for ~a hit points."
                                (name self) (name target) damage))
               (take-damage target damage))
        (message (format nil "~&~@(~a~) attacks ~a but it has no effect!"
                         (name self) (name target))))))

(defmethod take-damage ((self fighter) damage)
  ;; apply damage if possible
  (if (> damage 0)
      (decf (hp self) damage))
  ;; check for death. if there's a death function, call it
  (when (<= (hp self) 0)
      (if (death-fun self)
          (funcall (death-fun self) self))
      (if (not (eql self *player*)) ; yield experience to the player
          (incf (xp *player*) (xp self)))))

(defmethod heal ((self fighter) amount)
  ;; heal by the given amount, without going over the maximum
  (incf (hp self) amount)
  (if (> (hp self) (max-hp self))
      (setf (hp self) (max-hp self))))

(defgeneric take-turn (self)
  (:documentation "take turn"))

(defclass basic-monster (object)
  ((ai
    :initform #'take-turn))
  (:documentation "AI for a basic monster"))

(defmethod take-turn ((monster basic-monster))
  "a basic monster takes its turn. If you can see it, it can see you"
  (if (tcod:map-is-in-fov? *fov-map* (x monster) (y monster))
      (cond
        ;; move towards player if far away
        ((>= (distance-to monster *player*) 2)
         (move-towards monster (x *player*) (y *player*)))
        ;; close enough, attack! (if the player s still alive.)
        ((> (hp *player*) 0)
         (attack monster *player*)))))

(defun confused-monster (monster old-ai &optional (num-turns +confuse-num-turns+))
  "AI for a temporarily confused monster (reverts to previous AI after a while)"
  (if (> num-turns 0) ; still confused...
      ;; move in a random direction, and decrease the number of turns confused
      (progn (move monster (tcod:random-get-int tcod:+null+ -1 1)
                   (tcod:random-get-int tcod:+null+ -1 1))
             (setf (ai monster) (lambda (mon) (confused-monster mon old-ai (1- num-turns)))))
      ;; restore the previous AI (this one will be deleted because it's not referenced anymore)
      (progn (setf (ai monster) old-ai)
             (message (format nil "The ~a is no longer confused!" (name monster)) :red))))

(defclass basic-fighter-monster (fighter basic-monster)
  ())

(defun check-level-up ()
  "see if the player's experience is enough to level-up"
  (let ((level-up-xp (+ +level-up-base+ (* (level *player*) +level-up-factor+))))
    (when (> (xp *player*) level-up-xp)
      ;; it is! level up
      (incf (level *player*))
      (decf (xp *player*) level-up-xp)
      (message (format nil "Your battle skills grow stronger! You reached level ~a!"
                       (level *player*))
               :yellow)
      (do (choice)
          (choice)
        (setf choice (menu "Level up! Choose a stat to raise~%"
                           (list (format nil "Constitution (+20 HP, from ~a)"
                                         (max-hp *player*))
                                 (format nil "Strength (+1 attack, from ~a)"
                                         (power *player*))
                                 (format nil "Agility (+1 defense, from ~a)"
                                         (defense *player*)))
                           +level-screen-width+))
        (when choice
          (cond ((= choice 0)
                 (incf (max-hp *player*) 20)
                 (incf (hp *player*) 20))
                ((= choice 1)
                 (incf (power *player*) 1))
                ((= choice 2)
                 (incf (defense *player*) 1))))))))

(defun player-death (player)
  ;; the game ended!
  (message "You died!" :red)
  (setf *game-state* :dead)

  ;; for added effect, transform the player into a corpse!
  (setf (cha player) #\%
        (color player) :dark-red))

(defun monster-death (monster)
  ;; transform it into a nasty corpse! it doesn't block, can't be
  ;; attacked and doesn't move
  (message (format nil "~@(~a~) is dead! You gain ~a experience points"
                   (name monster) (xp monster))
           :orange)
  (setf *objects* (append (list (make-instance 'object :x (x monster) :y (y monster) :cha #\%
                                                       :color :dark-red
                                                       :name (format nil "remains of ~a"
                                                                     (name monster))))
                          *objects*))
  (setf *objects* (remove monster *objects*)))

(defun place-objects (room)
  ;; choose random number of monsters
  (let ((num-monsters (tcod:random-get-int tcod:+null+ 0 *max-room-monsters*))
        monster x y)
    (dotimes (i num-monsters)
      ;; choose random spot for this monster
      (setf x (tcod:random-get-int tcod:+null+ (+ (x1 room) 1) (- (x2 room) 1))
            y (tcod:random-get-int tcod:+null+ (+ (y1 room) 1) (- (y2 room) 1)))

      (unless (is-blocked x y)
        (if (< (tcod:random-get-int tcod:+null+ 0 100) 80) ; 80% chance of getting an orc
            ;; create an orc
            (setf monster (make-instance 'basic-fighter-monster
                                         :x x :y y :cha #\o :name "orc" :color :pale-green
                                         :blocks t :max-hp 10 :defense 0 :power 3 :xp 35
                                         :death-fun #'monster-death))
            ;; create a troll
            (setf monster (make-instance 'basic-fighter-monster
                                         :x x :y y :cha #\T :name "troll" :color :dark-sea-green
                                         :blocks t :max-hp 10 :defense 1 :power 4 :xp 100
                                         :death-fun #'monster-death)))

        (setf *objects* (append *objects* (list monster))))))

  ;; items appear below other objects
  (let (dice)
    (setf *objects*
          ;; choose random number of items
          (append (loop for i below (tcod:random-get-int tcod:+null+ 0 *max-room-items*)
                        ;; choose random spot for this item
                        for x = (tcod:random-get-int tcod:+null+
                                                     (+ (x1 room) 1) (- (x2 room) 1))
                        for y = (tcod:random-get-int tcod:+null+
                                                     (+ (y1 room) 1) (- (y2 room) 1))
                        ;; only place it if the tile is not blocked
                        unless (is-blocked x y)
                          do (setf dice (tcod:random-get-int tcod:+null+ 0 100))
                          and if (< dice 70)
                                ;; create a healing potion (70% chance)
                                collect (make-instance 'item :x x :y y :cha #\!
                                                             :name "healing potion"
                                                             :use-function #'cast-heal
                                                             :color :purple
                                                             :always-visible t)
                          else if (< dice (+ 70 10))
                                 ;; create a lightning bolt scroll (10% chance)
                                 collect (make-instance 'item :x x :y y :cha #\#
                                                              :name "scroll of lightning bolt"
                                                              :use-function #'cast-lightning
                                                              :color :light-yellow
                                                              :always-visible t)
                          else if (< dice (+ 70 10 10))
                                 ;; create a fireball scroll (10% chance)
                                 collect (make-instance 'item :x x :y y :cha #\#
                                                              :name "scroll of fireball"
                                                              :use-function #'cast-fireball
                                                              :color :light-yellow
                                                              :always-visible t)
                          else
                            ;; create a confuse scroll (10% chance)
                            collect (make-instance 'item :x x :y y :cha #\#
                                                         :name "scroll of confusion"
                                                         :use-function #'cast-confuse
                                                         :color :light-yellow
                                                         :always-visible t))
                  *objects*))))

(defun next-level ()
  "advance to the next level"
  (message "You take a moment to rest, and recover your strength." :purple)
  (heal *player* (/ (max-hp *player*) 2)) ; heal the player by 50%

  (message "After a rare moment of peace, you descend deeper into the heart of the dungeon..." :red)
  (incf *dungeon-level*)
  (make-map) ; create a fresh new level!
  (initialize-fov))

(defun handle-keys (event)
  (let ((key (tcod:key-vk event)))
    (cond ((and (eql key :ENTER) (tcod:key-lalt event))
           (tcod:console-set-fullscreen (not (tcod:console-is-fullscreen?))))
          ((eql key :ESCAPE) (return-from handle-keys :exit)))
    (if (eql *game-state* :playing)
        (cond
          ;; movement keys
          ((or (eql key :UP) (eql key :KP8))
           (move-or-attack *player* 0 -1))
          ((or (eql key :DOWN) (eql key :KP2))
           (move-or-attack *player* 0 1))
          ((or (eql key :LEFT) (eql key :KP4))
           (move-or-attack *player* -1 0))
          ((or (eql key :RIGHT) (eql key :KP6))
           (move-or-attack *player* 1 0))
          ((or (eql key :HOME) (eql key :KP7))
           (move-or-attack *player* -1 -1))
          ((or (eql key :PAGEUP) (eql key :KP9))
           (move-or-attack *player* 1 -1))
          ((or (eql key :END) (eql key :KP1))
           (move-or-attack *player* -1 1))
          ((or (eql key :PAGEDOWN) (eql key :KP3))
           (move-or-attack *player* 1 1))
          ((eql key :KP5)
           ) ;; do nothing ie wait for the monster to come to you
          ((char= (tcod:key-c event) #\g)
           ;; pick up an item
           (dolist (obj *objects*) ; look for an item in the player's tile
             (when (and (eql (x obj) (x *player*)) (eql (y obj) (y *player*))
                        (typep obj 'item))
               (pick-up obj)))
           (return-from handle-keys :didnt-take-turn))
          ((char= (tcod:key-c event) #\i)
           ;; show the inventory; if an item is selected, use it
           (let ((chosen-item
                  (inventory-menu
                   (format nil
                           "Press the key next to an item to use it, or any other to cancel.~%"))))
             (if chosen-item
                 (use chosen-item))))
          ((char= (tcod:key-c event) #\d)
           ;; show the inventory; if an item is selected, drop it
           (let ((chosen-item
                   (inventory-menu
                    (format nil "Press the key next to an item to drop it, ~
                                 or any other to cancel.~%"))))
             (if chosen-item
                 (drop chosen-item))))
          ((and (tcod:key-shift event) (char= (tcod:key-c event) #\.))
           ;; go down stairs, if the player is on them
           (if (and (= (x *stairs*) (x *player*)) (= (y *stairs*) (y *player*)))
               (next-level)))
          ((char= (tcod:key-c event) #\c)
           ;; show character information
           (let ((level-up-xp (+ +level-up-base+ (* (level *player*) +level-up-factor+))))
             (msgbox (format nil "Character Information~%~%Level: ~a~%Experience: ~a~%~
                                  Experience to level up: ~a~%~%Maximum HP: ~a~%Attack: ~a~%~
                                  Defense: ~a" (level *player*) (xp *player*) level-up-xp
                                  (max-hp *player*) (power *player*) (defense *player*))
                     +character-screen-width+)))
          (t (return-from handle-keys :didnt-take-turn))))))

(defun get-mouse-events ()
  (let ((events (tcod:sys-wait-events :event-mouse t)))
    (loop for ev in events
       when (eql (car ev) :event-mouse-move)
       collect (cdr ev) into mouse-events
         finally (return mouse-events))))

(defun get-names-under-mouse (event)
  ;;returns a string with the names of all objects under the mouse
  (if (not event)
      (return-from get-names-under-mouse ""))
  (let* ((x (tcod:mouse-cx event)) (y (tcod:mouse-cy event))
         (fov (tcod:map-is-in-fov? *fov-map* x y)))
    (loop for obj in *objects*
       when (and (eql (x obj) x) (eql (y obj) y) fov)
       collect (name obj) into obj-names
       finally (return (format nil "~{~@(~a~^, ~)~}" obj-names)))))

(defun make-map ()
  ;; the list of objects with just the player
  (setf *objects* (list *player*))
  
  (setf *map* (make-array (list *map-height* *map-width*)))
  (dotimes (i *map-height*)
    (dotimes (j *map-width*)
      (setf (aref *map* i j) (make-instance 'tile :blocked t))))

  (loop with num-rooms = 0 and rooms = nil
        and w and h and x and y
        and new-room and failed
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

            ;; add some contents to this room, such as monsters
            (place-objects new-room)
         
            ;; finally, append the new room to the list
            (setf rooms (append rooms (list new-room)))
            (incf num-rooms))

        ;; create stairs at the center of the last room
        finally (let ((new-x (car (center (car (last rooms)))))
                      (new-y (cdr (center (car (last rooms))))))
                  (setf *stairs* (make-instance 'object :x new-x :y new-y :cha #\>
                                                        :name "stairs" :color :white
                                                        :always-visible t))
                  (push *stairs* *objects*))))

(defun new-game ()
  ;; create object representing the player
  (setf *player* (make-instance 'fighter
                                :x 0 :y 0 :cha #\@ :name "player" :color :white :blocks t
                                :max-hp 30 :defense 2 :power 5 :xp 0 :level 1
                                :death-fun #'player-death))
  
  ;; generate map (at this point it's not drawn to the screen)
  (setf *dungeon-level* 1)
  (make-map)
  (initialize-fov)

  (setf *game-state* :playing)
  (setf *inventory* '())

  ;;create the list of game messages and their colors, starts empty
  (setf *game-msgs* '())

  ;; a warm welcoming message!
  (message "Welcome stranger! Prepare to perish in the Tombs of the Ancient Kings." :red))

(defun initialize-fov ()
  (setf *fov-recompute* t)

  ;; create the FOV map, according to the generated map
  (setf *fov-map* (tcod:map-new *map-width* *map-height*))
  (dotimes (y *map-height*)
    (dotimes (x *map-width*)
      (tcod:map-set-properties *fov-map* x y (not (block-sight (aref *map* y x)))
                               (not (blocked (aref *map* y x))))))

  (tcod:console-clear *con*)) ; unexplored areas start black (which is the default background color)

(defun render-all ()
  (when *fov-recompute*
    ;; recompute FOV if needed (the player moved or something)
    (setf *fov-recompute* nil)
    (tcod:map-compute-fov *fov-map* (x *player*) (y *player*) *torch-radius*
                          *fov-light-walls* *fov-algo*))
  
  ;; go through all the tiles, and set their background color
  (dotimes (i *map-height*)
    (dotimes (j *map-width*)
      (let ((visible (tcod:map-is-in-fov? *fov-map* j i))
            (wall (block-sight (aref *map* i j))))
        (if (not visible)
            ;; if it's not visible right now, the player can only see it if it's explored
            (if (explored (aref *map* i j))
                ;; it's out of the player's FOV
                (if wall
                    (tcod:console-set-char-background *con* j i (tcod:color :dark-wall) :set)
                    (tcod:console-set-char-background *con* j i (tcod:color :dark-ground) :set)))
          ;; it's visible
            (progn (if wall
                       (tcod:console-set-char-background *con* j i (tcod:color :light-wall) :set)
                       (tcod:console-set-char-background *con* j i (tcod:color :light-ground) :set))
                   ;; since it's visible, explore it
                   (setf (explored (aref *map* i j)) t))))))

  ;; draw all objects in the list, except the player. we want it to
  ;; always appear over all other objects! so it's drawn later.
  (dolist (obj *objects*)
    (if (not (eql obj *player*))
        (draw obj)))
  (draw *player*)

  ;; blit the contents of con to the root console
  (tcod:console-blit *con* 0 0 *map-width* *map-height* tcod:*root* 0 0 1.0 1.0)
  
  ;; prepare to render the GUI panel
  (tcod:console-set-default-background *panel* (tcod:color :black))
  (tcod:console-clear *panel*)

  ;; print the game messages, one line at a time
  (loop for (line . color) in *game-msgs*
     and y from 1 do
       (tcod:console-set-default-foreground *panel* (tcod:color color))
       (tcod:console-print-ex *panel* *msg-x* y :none :left line))

  ;; show the player's stats
  (render-bar 1 1 *bar-width* "HP" (hp *player*) (max-hp *player*) :light-red :dark-red)
  (tcod:console-print-ex *panel* 1 3 :none :left "Dungeon level ~a" *dungeon-level*)

  ;; display names of objects under the mouse
  (tcod:console-set-default-background *panel* (tcod:color :slate-gray))
  (tcod:console-print-ex *panel* 1 0 :none :left (get-names-under-mouse *mouse-event*))

  ;; blit the contents of "panel" to the root console
  (tcod:console-blit *panel* 0 0 *screen-width* *panel-height* tcod:*root* 0 *panel-y* 1.0 1.0))

(defun clear-windows ()
  (tcod:console-delete *panel*)
  (tcod:console-delete *con*)
  (tcod:console-delete tcod:*root*))

(defun play-game ()
  (let ((player-action nil))
    (do ()
        ((tcod:console-is-window-closed?))
      ;; render the screen
      (render-all)

      (tcod:console-flush)

      ;; level up if needed
      (check-level-up)

      ;; erase all objects at their old locations, before they move
      (mapcar #'clear *objects*)

      ;; handle keys and exit game if needed
      (setf player-action :didnt-take-turn)
      (dolist (event (tcod:sys-get-events))
        (if (eql (car event) :event-key-press)
            (setf player-action (handle-keys (cdr event)))
            (if (eql (car event) :event-mouse-move)
                (setf *mouse-event* (cdr event)))))
      (when (eql player-action :exit)
        (save-game)
        (return))

      ;; let monsters take their turn
      (if (and (eql *game-state* :playing) (not (eql player-action :didnt-take-turn)))
          (dolist (obj *objects*)
            (if (ai obj)
                (funcall (ai obj) obj)))))))

(defun main-menu ()
  (let ((img (tcod:image-load "menu_background.png")))

    (do (choice)
        ((tcod:console-is-window-closed?))
      ;; show the background image, at twice the regular console resolution
      (tcod:image-blit-2x img tcod:*root* 0 0 0 0 -1 -1)

      ;; show the game's title, and some credits!
      (tcod:console-set-default-foreground tcod:*root* (tcod:color :light-yellow))
      (tcod:console-print-ex tcod:*root* (/ *screen-width* 2) (- (/ *screen-height* 2) 4) :none
                             :center "TOMBS OF THE ANCIENT KINGS")
      (tcod:console-print-ex tcod:*root* (/ *screen-width* 2) (- *screen-height* 2) :none
                             :center "By Jotaf (Common Lisp port by John)")

      ;; show options and wait for the player's choice
      (setf choice (menu "" (list "Play a new game" "Continue last game" "Quit") 24))

      (cond ((eql choice 0) ; new game
             (new-game)
             (play-game))
            ((eql choice 1) ; load last game
             (if (probe-file "savegame.store")
                 (load-game)
                 (msgbox "~%No saved game to load.~%" 24))
             (play-game))
            ((eql choice 2) ; quit
             (clear-windows)
             (return))))))

(defun save-game ()
  ;; create a hash table to store the current game state
  ;; NOTE: error on loading if monster saved with confused AI, due to lambda function
  (let ((cur-game (make-hash-table)))
    (setf (gethash 'map cur-game) *map*
          (gethash 'objects cur-game) *objects*
          (gethash 'player cur-game) *player*
          (gethash 'inventory cur-game) *inventory*
          (gethash 'game-msgs cur-game) *game-msgs*
          (gethash 'game-state cur-game) *game-state*
          (gethash 'dungeon-level cur-game) *dungeon-level*
          (gethash 'stairs cur-game) *stairs*)
    ;; write it out with cl-store
    (cl-store:store cur-game "savegame.store")))

(defun load-game ()
  "open the previously stored hash map and load the game data"
  (let ((old-game (cl-store:restore "savegame.store")))
    (setf *map* (gethash 'map old-game)
          *objects* (gethash 'objects old-game)
          *player* (gethash 'player old-game)
          *inventory* (gethash 'inventory old-game)
          *game-msgs* (gethash 'game-msgs old-game)
          *game-state* (gethash 'game-state old-game)
          *dungeon-level* (gethash 'dungeon-level old-game)
          *stairs* (gethash 'stairs old-game)))

  (initialize-fov))

(defun startup ()
  ;; system initialization
  (tcod:console-set-custom-font "terminal16x16_gs_ro.png"
                                '(:FONT-TYPE-GREYSCALE :FONT-LAYOUT-ASCII-IN-ROW))
  (tcod:console-init-root *screen-width* *screen-height*
                          :title "lisp/libtcod tutorial" :fullscreen? nil :renderer :renderer-sdl)
  (tcod:sys-set-fps *limit-fps*)
  (defparameter *con* (tcod:console-new *map-width* *map-height*))
  (defparameter *panel* (tcod:console-new *screen-width* *panel-height*))

  (main-menu))

(startup)
