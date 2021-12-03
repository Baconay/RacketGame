;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname adventure) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require "adventure-define-struct.rkt")
(require "macros.rkt")
(require "utilities.rkt")

;;;
;;; OBJECT
;;; Base type for all in-game objects
;;;

(define-struct object
  ;; adjectives: (listof string)
  ;; List of adjectives to be printed in the description of this object
  (adjectives)
  
  #:methods
  ;; noun: object -> string
  ;; Returns the noun to use to describe this object.
  (define (noun o)
    (type-name-string o))

  ;; description-word-list: object -> (listof string)
  ;; The description of the object as a list of individual
  ;; words, e.g. '("a" "red" "door").
  (define (description-word-list o)
    (add-a-or-an (append (object-adjectives o)
                         (list (noun o)))))
  ;; description: object -> string
  ;; Generates a description of the object as a noun phrase, e.g. "a red door".
  (define (description o)
    (words->string (description-word-list o)))
  
  ;; print-description: object -> void
  ;; EFFECT: Prints the description of the object.
  (define (print-description o)
    (begin (printf (description o))
           (newline)
           (void))))

;;;
;;; CONTAINER
;;; Base type for all game objects that can hold things
;;;

(define-struct (container object)
  ;; contents: (listof thing)
  ;; List of things presently in this container
  (contents)
  
  #:methods
  ;; container-accessible-contents: container -> (listof thing)
  ;; Returns the objects from the container that would be accessible to the player.
  ;; By default, this is all the objects.  But if you want to implement locked boxes,
  ;; rooms without light, etc., you can redefine this to withhold the contents under
  ;; whatever conditions you like.
  (define (container-accessible-contents c)
    (container-contents c))
  
  ;; prepare-to-remove!: container thing -> void
  ;; Called by move when preparing to move thing out of
  ;; this container.  Normally, this does nothing, but
  ;; if you want to prevent the object from being moved,
  ;; you can throw an exception here.
  (define (prepare-to-remove! container thing)
    (void))
  
  ;; prepare-to-add!: container thing -> void
  ;; Called by move when preparing to move thing into
  ;; this container.  Normally, this does nothing, but
  ;; if you want to prevent the object from being moved,
  ;; you can throw an exception here.
  (define (prepare-to-add! container thing)
    (void))
  
  ;; remove!: container thing -> void
  ;; EFFECT: removes the thing from the container
  (define (remove! container thing)
    (set-container-contents! container
                             (remove thing
                                     (container-contents container))))
  
  ;; add!: container thing -> void
  ;; EFFECT: adds the thing to the container.  Does not update the thing's location.
  (define (add! container thing)
    (set-container-contents! container
                             (cons thing
                                   (container-contents container))))

  ;; describe-contents: container -> void
  ;; EFFECT: prints the contents of the container
  (define (describe-contents container)
    (begin (local [(define other-stuff (remove me (container-accessible-contents container)))]
             (if (empty? other-stuff)
                 (printf "There's nothing here.~%")
                 (begin (printf "You see:~%")
                        (for-each print-description other-stuff))))
           (void))))

;; move!: thing container -> void
;; Moves thing from its previous location to container.
;; EFFECT: updates location field of thing and contents
;; fields of both the new and old containers.
(define (move! thing new-container)
  (begin
    (prepare-to-remove! (thing-location thing)
                        thing)
    (prepare-to-add! new-container thing)
    (prepare-to-move! thing new-container)
    (remove! (thing-location thing)
             thing)
    (add! new-container thing)
    (set-thing-location! thing new-container)))

;; destroy!: thing -> void
;; EFFECT: removes thing from the game completely.
(define (destroy! thing)
  ; We just remove it from its current location
  ; without adding it anyplace else.
  (remove! (thing-location thing)
           thing))

;;;
;;; ROOM
;;; Base type for rooms and outdoor areas
;;;

(define-struct (room container)
  ())

;; new-room: string -> room
;; Makes a new room with the specified adjectives
(define (new-room adjectives)
  (make-room (string->words adjectives)
             '()))

;;;
;;; THING
;;; Base type for all physical objects that can be inside other objects such as rooms
;;;

(define-struct (thing container)
  ;; location: container
  ;; What room or other container this thing is presently located in.
  (location)
  
  #:methods
  (define (examine thing)
    (print-description thing))

  ;; prepare-to-move!: thing container -> void
  ;; Called by move when preparing to move thing into
  ;; container.  Normally, this does nothing, but
  ;; if you want to prevent the object from being moved,
  ;; you can throw an exception here.
  (define (prepare-to-move! container thing)
    (void)))

;; initialize-thing!: thing -> void
;; EFFECT: adds thing to its initial location
(define (initialize-thing! thing)
  (add! (thing-location thing)
        thing))

;; new-thing: string container -> thing
;; Makes a new thing with the specified adjectives, in the specified location,
;; and initializes it.
(define (new-thing adjectives location)
  (local [(define thing (make-thing (string->words adjectives)
                                    '() location))]
    (begin (initialize-thing! thing)
           thing)))

;;;
;;; DOOR
;;; A portal from one room to another
;;; To join two rooms, you need two door objects, one in each room
;;;

(define-struct (door thing)
  ;; destination: container
  ;; The place this door leads to
  (destination)
  
  #:methods
  ;; go: door -> void
  ;; EFFECT: Moves the player to the door's location and (look)s around.
  (define (go door)
    (begin (move! me (door-destination door))
           (look))))

;; join: room string room string
;; EFFECT: makes a pair of doors with the specified adjectives
;; connecting the specified rooms.
(define (join! room1 adjectives1 room2 adjectives2 material weakness)
  (local [(define r1->r2 (make-barricade (string->words adjectives1)
                                    '() room1 room2 material weakness #t))
          (define r2->r1 (make-door (string->words adjectives2)
                                    '() room2 room1))]
    (begin (initialize-thing! r1->r2)
           (initialize-thing! r2->r1)
           (void))))

;;;
;;; PERSON
;;; A character in the game.  The player character is a person.
;;;

(define-struct (person thing)
  (health))

;; initialize-person: person -> void
;; EFFECT: do whatever initializations are necessary for persons.
(define (initialize-person! p)
  (initialize-thing! p))

;; new-person: string container -> person
;; Makes a new person object and initializes it.
(define (new-person adjectives location health)
  (local [(define person
            (make-person (string->words adjectives)
                         '()
                         health
                         location))]
    (begin (initialize-person! person)
           person)))

;; This is the global variable that holds the person object representing
;; the player.  This gets reset by (start-game)
(define me empty)

;;;
;;; PROP
;;; A thing in the game that doesn't serve any purpose other than to be there.
;;;

(define-struct (prop thing)
  (;; noun-to-print: string
   ;; The user can set the noun to print in the description so it doesn't just say "prop"
   noun-to-print
   ;; examine-text: string
   ;; Text to print if the player examines this object
   examine-text
   )
  
  #:methods
  (define (noun prop)
    (prop-noun-to-print prop))

  (define (examine prop)
    (display-line (prop-examine-text prop))))

;; new-prop: string container -> prop
;; Makes a new prop with the specified description.
(define (new-prop description examine-text location)
  (local [(define words (string->words description))
          (define noun (last words))
          (define adjectives (drop-right words 1))
          (define prop (make-prop adjectives '() location noun examine-text))]
    (begin (initialize-thing! prop)
           prop)))



;;;
;;; ADD YOUR TYPES HERE!
;;;

;;PICKAXE ========================================================
(define-struct (pickaxe thing)
  (material durability)

  #:methods
  (define (upgrade new-material pickaxe)
    (set! pickaxe-material new-material))
    
  (define (check-durability pickaxe)
    (pickaxe-durability pickaxe))
)

;; new-pickaxe: string string container -> pickaxe
;; Makes a new pickaxe thing and intializes it
(define (new-pickaxe adjectives material durability location)
  (local [(define the-pickaxe
            (make-pickaxe (string->words adjectives)
                          '()
                          location
                          material
                          durability))]
      (begin (initialize-thing! the-pickaxe)
              the-pickaxe)))


;;CHEST ========================================================
(define-struct (chest thing)
  (open?)
  #:methods
  (define (open c)
    (set-chest-open?! c #t))
  (define (close c)
    (set-chest-open?! c #f))
  (define (container-accessible-contents c)
    (if (chest-open? c)
        (container-contents c)
        (error "The chest is closed"))))
  
  (define (examine-contents c)
    (describe-contents c))

;; new-chest : string container -> chest
;; Makes a chest with adjectives and in a location
(define (new-chest adjectives location)
  (local [(define the-chest 
            (make-chest (string->words adjectives)
                        '() location
                        false))]
      (begin (initialize-thing! the-chest)
              the-chest)))
              
              
;; THE HOME DEPOT===================================================
(define-struct (TheHomeDepot chest)
 ())

;; new-homedepot : stuff -> 
 (define (new-homedepot adjectives location)
  (local [(define the-home-depot 
            (make-TheHomeDepot (string->words adjectives)
                        '() location
                        true))]
      (begin (initialize-thing! the-home-depot)
              the-home-depot)))

;; ENDER CHEST====================================================
(define-struct (enderchest chest)
  (trapped?)
  #:methods
  (define (open c)
  (if (enderchest-trapped? c)
    (begin (set-person-health! me (- (hp) 50))
          (printf "BOOOM... You take 50 damage.")
          (newline)
          (printf "Can't believe you fell for that lmao")
          (newline)
          (destroy! c)
          (when (<= (person-health me) 0)
          (begin (display "You died! LMAO")
                 (newline)
                 (newline)
                 (display "RESTARTING GAME")
                  (start-game)
                  )))
          (set-chest-open?! c #t))))

;; new-trapchest : string container boolean -> chest
;; Makes a ender chest in given location
(define (new-enderchest adjectives location trapped)
  (local [(define the-enderchest
            (make-enderchest (string->words adjectives)
                            '() location
                            false trapped))]
      (begin (initialize-thing! the-enderchest)
              the-enderchest)))

;; Stick ========================================================
(define-struct (stick thing)
  ())
;; new-stick : string container -> stick
;; Makes a stick with adjective and location
(define (new-stick adjectives location)
  (local [(define the-stick
            (make-stick (string->words adjectives)
                        '()
                        location))]
          (begin (initialize-thing! the-stick)
                  the-stick)))

;; Tree ============================================================
(define-struct (tree thing)
(durability)
#:methods
(define (punch t)
  (begin (set-tree-durability! t (- (tree-durability t) 1))
        (if (= (tree-durability t) 0)
            (begin (new-stick "wooden" me)
                    (destroy! t)
                    (display "You receive: a stick. The tree is now destoryed."))
            (begin (new-stick "wooden" me)
                   (display "You receive: a stick."))))))
;;Creating tree
(define (new-tree adjectives durability location)
  (local [(define the-tree
        (make-tree (string->words adjectives)
                    '() 
                    location
                    durability))]
      (begin (initialize-thing! the-tree)
                the-tree)))
;; Barricade ========================================================
(define-struct (barricade door)
  (material weakness blocked?)
  #:methods
  (define (go barricade)
    (if (barricade-blocked? barricade)
    (error "The barricade is blocking your way. You should probably mine it!")
    (begin (move! me (door-destination barricade))
           (look)))))

  (define (new-barricade adjectives material weakness location)
    (local [(define the-barricade
            (make-barricade (string->words adjectives)
                            material
                            weakness
                            #t
                            '()
                            location))]
            (begin (initialize-thing! the-barricade)               
                            the-barricade)))

;; Element ========================================================
(define-struct (element thing)
(craftability matter))

;; Wood ========================================================
(define-struct (wood element)
())

(define (new-wood adjectives location)
  (local [(define the-wood
        (make-wood (string->words adjectives)
                    '() 
                    location
                    true
                    "wood"
                    ))]
      (begin (initialize-thing! the-wood)
                the-wood)))

;; Diamond ========================================================
(define-struct (diamond element)
())

(define (new-diamond adjectives location)
  (local [(define the-diamond
        (make-diamond (string->words adjectives)
                    '() 
                    location
                    true
                    "diamond"
                    ))]
      (begin (initialize-thing! the-diamond)
                the-diamond)))

;; Cobblestone ========================================================
(define-struct (cobblestone element)
())

(define (new-cobblestone adjectives location)
  (local [(define the-cobblestone
        (make-cobblestone (string->words adjectives)
                    '() 
                    location
                    true
                    "stone"
                    ))]
      (begin (initialize-thing! the-cobblestone)
                the-cobblestone)))

;; Ingot ========================================================
(define-struct (ingot element)
(luster)

#:methods
(define (examine ingot)
  (if (ingot-luster ingot)
      (display "Shiny!")
      (display "Dull :(")))

)

;; Iron ========================================================
(define-struct (ironingot ingot)
())

(define (new-ironingot adjectives location)
  (local [(define the-ironingot
        (make-ironingot (string->words adjectives)
                    '() 
                    location
                    true
                    "iron"
                    true
                    ))]
      (begin (initialize-thing! the-ironingot)
                the-ironingot)))

;; Gold ========================================================
(define-struct (goldingot ingot)
())

(define (new-goldingot adjectives location)
  (local [(define the-goldingot
        (make-goldingot (string->words adjectives)
                    '() 
                    location
                    true
                    "gold"
                    true
                    ))]
      (begin (initialize-thing! the-goldingot)
                the-goldingot)))

;; Obsidian ========================================================
(define-struct (obsidian element)
())

(define (new-obsidian adjectives location)
  (local [(define the-obsidian
        (make-obsidian (string->words adjectives)
                    '() 
                    location
                    true
                    "obsidian"
                    ))]
      (begin (initialize-thing! the-obsidian)
                the-obsidian)))

                            
;;;
;;; USER COMMANDS
;;;

(define (look)
  (begin (printf "You are in ~A.~%"
                 (description (here)))
         (describe-contents (here))
         (void)))

(define-user-command (look) "Prints what you can see in the room")

(define (inventory)
  (if (empty? (my-inventory))
      (printf "You don't have anything.~%")
      (begin (printf "You have:~%")
             (for-each print-description (my-inventory)))))

(define-user-command (inventory)
  "Prints the things you are carrying with you.")

(define-user-command (examine thing)
  "Takes a closer look at the thing")

(define (take thing)
  (move! thing me))

(define-user-command (take thing)
  "Moves thing to your inventory")

(define (drop thing)
  (move! thing (here)))

(define-user-command (drop thing)
  "Removes thing from your inventory and places it in the room
")

(define (put thing container)
  (move! thing container))

(define-user-command (put thing container)
  "Moves the thing from its current location and puts it in the container.")

(define (help)
  (for-each (λ (command-info)
              (begin (display (first command-info))
                     (newline)
                     (display (second command-info))
                     (newline)
                     (newline)))
            (all-user-commands)))

(define-user-command (help)
  "Displays this help information")

(define-user-command (go door)
  "Go through the door to its destination")

(define (check condition)
  (if condition
      (display-line "Check succeeded")
      (error "Check failed!!!")))

(define-user-command (check condition)
  "Throws an exception if condition is false.")

;;;
;;; ADD YOUR COMMANDS HERE!
;;;

(define (mine barricade pickaxe) 
  (if (barricade-blocked? barricade)
    (if (and (have? pickaxe) (string=? (pickaxe-material pickaxe) (barricade-weakness barricade)))
        (begin (set-barricade-blocked?! barricade #f)
               (destroy! pickaxe)
               (display "The barricade is now unblocked. Your pickaxe has broken."))
        (if (<= (pickaxe-durability pickaxe) 0)
            (error "Your pickaxe is broken!")
            (error "You need the right pickaxe to mine this barricade")))
      (display "The barricade is already unblocked lMaO")))

(define-user-command (mine barricade pickaxe)
  "Mines a barricade with a pickaxe")

(define (hp)
  (person-health me))
  
(define-user-command (hp)
  "Prints the user's health")

(define (craft-pickaxe stick material)
      (begin
        (if (have? stick)
            (if (element-craftability material)
                (begin (destroy! material)
                        (destroy! stick)
                        (new-pickaxe (element-matter material) (element-matter material) 1 me)
                        (display "Congrats, you can mine now")
                        (newline))
              (error "Cannot craft pickaxe"))
        (error "Cannot craft pickaxe"))
        (when (and (ingot? material) (ingot-luster material))
            (display "Achievement Unlocked: Oooooo Shiny Pickaxe!"))))

(define-user-command (craft-pickaxe stick material)
  "Crafts a pickaxe using a stick and a material")
            
            
 (define-user-command (thisisaverylongandhardtotypecommandbecausewewantyoutoworktoseeifthechestistrapped enderchest)
  "Checks to see if a chest is trapped or not")       

(define (thisisaverylongandhardtotypecommandbecausewewantyoutoworktoseeifthechestistrapped enderchest)
  (if (enderchest-trapped? enderchest)
        (display "OH NO, THERE MAY OR MAY NOT BE A BOMB UNDER THE CHEST")
        (display "Nah you safe chief")))

  (define-user-command (open chest)
  "Opens a chest")      

  (define-user-command (close chest)
  "Closes a chest")

  (define-user-command (punch tree)
  "Punches a tree for sticks")   

  (define-user-command (check-durability pickaxe)
  "Checks the durability of a pickaxe")     

  (define (vulnerability barricade)
    (begin
    (display "The barricade is weak to ")
    (display (barricade-weakness barricade))))

  (define-user-command (vulnerability barricade)
  "Checks what type of pickaxe the barricade is weak to")

  (define (obstructed? barricade)
    (if (barricade-blocked? barricade)
        (begin 
        (display "The barricade seems to be blocked by some ")
        (display (barricade-material barricade))
        (display " debris"))
        (display "The path is clear! I hope")))
  
  (define-user-command (obstructed? barricade)
  "Checks to see if a barricade is blocked")

  (define (alchemy??? material pickaxe)
    (begin (upgrade pickaxe (element-matter material))
          (display "Achievement unlocked: Black Magic???")
          (newline)
          (display "Your pickaxe has now been upgraded")))

    (define-user-command (alchemy??? m p)
    "?????????????")
;;;
;;; THE GAME WORLD - FILL ME IN
;;;

;; start-game: -> void
;; Recreate the player object and all the rooms and things.
(define (start-game)
  ;; Fill this in with the rooms you want
  (local [(define starting-room (new-room "green forest"))
          (define room2 (new-room "cave"))
          (define room3 (new-room "dry desert"))
          (define room4 (new-room "snowy mountain"))
          (define room5 (new-room "the void"))]
    (begin (set! me (new-person "Steve" 100 starting-room))

           ;; Add join commands to connect your rooms with doors
           (join! starting-room "cave"
                  room2 "green forest" "cobblestone" "wood")
            (join! room2 "dry desert"
                    room3 "cave" "iron" "iron")
            (join! room3 "dry desert"
                    room4 "snowy mountain" "obsidian" "diamond")
            (join! room4 "snowy mountain"
                    room5 "the void" "bedrock" "obsidian")

           ;; Add code here to add things to your rooms

           ;;
           ;; Starting-Room
           ;;
           (new-enderchest "tempting" starting-room true)
           (new-wood "fresh" (new-homedepot "friendly" starting-room))
           (new-tree "tall" 1 starting-room)
           (new-pickaxe "wooden" "wood" 1 starting-room)

           ;;
           ;; Room2
           ;;
           (new-tree "purple pride" 1 room2)
           (new-ironingot "shiny" (new-chest "wooden" room2))
           (new-goldingot "shiny" (new-enderchest "???" room2 true))
           (new-enderchest "inviting" room2 true)

           ;;
           ;; Room3
           ;;
           (new-tree "Christmas" 1 room3)
           (new-diamond "diamond" (new-enderchest "terrifying" room3 false))
           (new-enderchest "totally safe" room3 true)

           ;;
           ;; Room4
           ;;
           (new-tree "festive" 1 room4)
           


           ;;
           ;; Room5
           ;;
           (check-containers!) 
           (void))))

;;;
;;; PUT YOUR WALKTHROUGHS HERE
;;;

(define-walkthrough test
  (open (the wood chest))
  (take (within (the wood chest)))
  (punch (the tree))
  (mine (the barricade))
  (go (the barricade)))
















;;;
;;; UTILITIES
;;;

;; here: -> container
;; The current room the player is in
(define (here)
  (thing-location me))

;; stuff-here: -> (listof thing)
;; All the stuff in the room the player is in
(define (stuff-here)
  (container-accessible-contents (here)))

;; stuff-here-except-me: -> (listof thing)
;; All the stuff in the room the player is in except the player.
(define (stuff-here-except-me)
  (remove me (stuff-here)))

;; my-inventory: -> (listof thing)
;; List of things in the player's pockets.
(define (my-inventory)
  (container-accessible-contents me))

;; accessible-objects -> (listof thing)
;; All the objects that should be searched by find and the.
(define (accessible-objects)
  (append (stuff-here-except-me)
          (my-inventory)))

;; have?: thing -> boolean
;; True if the thing is in the player's pocket.
(define (have? thing)
  (eq? (thing-location thing)
       me))

;; have-a?: predicate -> boolean
;; True if the player as something satisfying predicate in their pocket.
(define (have-a? predicate)
  (ormap predicate
         (container-accessible-contents me)))

;; find-the: (listof string) -> object
;; Returns the object from (accessible-objects)
;; whose name contains the specified words.
(define (find-the words)
  (find (λ (o)
          (andmap (λ (name) (is-a? o name))
                  words))
        (accessible-objects)))

;; find-within: container (listof string) -> object
;; Like find-the, but searches the contents of the container
;; whose name contains the specified words.
(define (find-within container words)
  (find (λ (o)
          (andmap (λ (name) (is-a? o name))
                  words))
        (container-accessible-contents container)))

;; find: (object->boolean) (listof thing) -> object
;; Search list for an object matching predicate.
(define (find predicate? list)
  (local [(define matches
            (filter predicate? list))]
    (case (length matches)
      [(0) (error "There's nothing like that here")]
      [(1) (first matches)]
      [else (error "Which one?")])))

;; everything: -> (listof container)
;; Returns all the objects reachable from the player in the game
;; world.  So if you create an object that's in a room the player
;; has no door to, it won't appear in this list.
(define (everything)
  (local [(define all-containers '())
          ; Add container, and then recursively add its contents
          ; and location and/or destination, as appropriate.
          (define (walk container)
            ; Ignore the container if its already in our list
            (unless (member container all-containers)
              (begin (set! all-containers
                           (cons container all-containers))
                     ; Add its contents
                     (for-each walk (container-contents container))
                     ; If it's a door, include its destination
                     (when (door? container)
                       (walk (door-destination container)))
                     ; If  it's a thing, include its location.
                     (when (thing? container)
                       (walk (thing-location container))))))]
    ; Start the recursion with the player
    (begin (walk me)
           all-containers)))

;; print-everything: -> void
;; Prints all the objects in the game.
(define (print-everything)
  (begin (display-line "All objects in the game:")
         (for-each print-description (everything))))

;; every: (container -> boolean) -> (listof container)
;; A list of all the objects from (everything) that satisfy
;; the predicate.
(define (every predicate?)
  (filter predicate? (everything)))

;; print-every: (container -> boolean) -> void
;; Prints all the objects satisfying predicate.
(define (print-every predicate?)
  (for-each print-description (every predicate?)))

;; check-containers: -> void
;; Throw an exception if there is an thing whose location and
;; container disagree with one another.
(define (check-containers!)
  (for-each (λ (container)
              (for-each (λ (thing)
                          (unless (eq? (thing-location thing)
                                       container)
                            (error (description container)
                                   " has "
                                   (description thing)
                                   " in its contents list but "
                                   (description thing)
                                   " has a different location.")))
                        (container-contents container)))
            (everything)))

;; is-a?: object word -> boolean
;; True if word appears in the description of the object
;; or is the name of one of its types
(define (is-a? obj word)
  (let* ((str (if (symbol? word)
                  (symbol->string word)
                  word))
         (probe (name->type-predicate str)))
    (if (eq? probe #f)
        (member str (description-word-list obj))
        (probe obj))))

;; display-line: object -> void
;; EFFECT: prints object using display, and then starts a new line.
(define (display-line what)
  (begin (display what)
         (newline)
         (void)))

;; words->string: (listof string) -> string
;; Converts a list of one-word strings into a single string,
;; e.g. '("a" "red" "door") -> "a red door"
(define (words->string word-list)
  (string-append (first word-list)
                 (apply string-append
                        (map (λ (word)
                               (string-append " " word))
                             (rest word-list)))))

;; string->words: string -> (listof string)
;; Converts a string containing words to a list of the individual
;; words.  Inverse of words->string.
(define (string->words string)
  (string-split string))

;; add-a-or-an: (listof string) -> (listof string)
;; Prefixes a list of words with "a" or "an", depending
;; on whether the first word in the list begins with a
;; vowel.
(define (add-a-or-an word-list)
  (local [(define first-word (first word-list))
          (define first-char (substring first-word 0 1))
          (define starts-with-vowel? (string-contains? first-char "aeiou"))]
    (cons (if starts-with-vowel?
              "an"
              "a")
          word-list)))

;;
;; The following calls are filling in blanks in the other files.
;; This is needed because this file is in a different langauge than
;; the others.
;;
(set-find-the! find-the)
(set-find-within! find-within)
(set-restart-game! (λ () (start-game)))
(define (game-print object)
  (cond [(void? object)
         (void)]
        [(object? object)
         (print-description object)]
        [else (write object)]))

(current-print game-print)
   
;;;
;;; Start it up
;;;

(start-game)
(look)

