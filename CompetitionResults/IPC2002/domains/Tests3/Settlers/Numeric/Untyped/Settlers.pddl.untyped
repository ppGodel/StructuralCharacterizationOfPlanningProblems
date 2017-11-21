(define (domain civ)
(:requirements :fluents :conditional-effects)
(:predicates
	 (connected-by-land ?p1 ?p2) (connected-by-rail ?p1 ?p2) (connected-by-sea ?p1 ?p2) (woodland ?p) (mountain ?p) (metalliferous ?p) (by-coast ?p) (has-cabin ?p) (has-coal-stack ?p) (has-quarry ?p) (has-mine ?p) (has-sawmill ?p) (has-ironworks ?p) (has-docks ?p) (has-wharf ?p) (is-cart ?v) (is-train ?v) (is-ship ?v) (is-at ?v ?p) (potential ?v)(place ?x) (vehicle ?x) (resource ?x) )
(:functions
	 (available ?r ?s) (space-in ?v) (labour) (resource-use) (pollution) (housing ?p))
(:constants iron ore wood timber coal stone)
(:action load
 :parameters ( ?v ?p ?r)
 :precondition
	(and (vehicle ?v) (place ?p) (resource ?r)  (is-at ?v ?p) (> (available ?r ?p) 0) (> (space-in ?v) 0))
 :effect
	(and (decrease (space-in ?v) 1) (increase (available ?r ?v) 1) (decrease (available ?r ?p) 1) (increase (labour) 1)))

(:action unload
 :parameters ( ?v ?p ?r)
 :precondition
	(and (vehicle ?v) (place ?p) (resource ?r)  (is-at ?v ?p) (> (available ?r ?v) 0))
 :effect
	(and (increase (space-in ?v) 1) (decrease (available ?r ?v) 1) (increase (available ?r ?p) 1) (increase (labour) 1)))

(:action move-cart
 :parameters ( ?v ?p1 ?p2)
 :precondition
	(and (vehicle ?v) (place ?p1) (place ?p2)  (is-cart ?v) (connected-by-land ?p1 ?p2) (is-at ?v ?p1))
 :effect
	(and (is-at ?v ?p2) (not (is-at ?v ?p1)) (increase (labour) 2)))

(:action move-train
 :parameters ( ?v ?p1 ?p2)
 :precondition
	(and (vehicle ?v) (place ?p1) (place ?p2)  (is-train ?v) (connected-by-rail ?p1 ?p2) (is-at ?v ?p1) (>= (available coal ?v) 1))
 :effect
	(and (is-at ?v ?p2) (not (is-at ?v ?p1)) (decrease (available coal ?v) 1) (increase (pollution) 1)))

(:action move-ship
 :parameters ( ?v ?p1 ?p2)
 :precondition
	(and (vehicle ?v) (place ?p1) (place ?p2)  (is-ship ?v) (connected-by-sea ?p1 ?p2) (is-at ?v ?p1) (>= (available coal ?v) 2))
 :effect
	(and (is-at ?v ?p2) (not (is-at ?v ?p1)) (decrease (available coal ?v) 2) (increase (pollution) 2)))

(:action build-cabin
 :parameters ( ?p)
 :precondition
	(and (place ?p) (woodland ?p))
 :effect
	(and (has-cabin ?p) (increase (labour) 1)))

(:action build-quarry
 :parameters ( ?p)
 :precondition
	(and (place ?p) (mountain ?p))
 :effect
	(and (has-quarry ?p) (increase (labour) 2)))

(:action build-coal-stack
 :parameters ( ?p)
 :precondition
	(and (place ?p) (>= (available timber ?p) 1))
 :effect
	(and (has-coal-stack ?p) (increase (labour) 2) (decrease (available timber ?p) 1)))

(:action build-sawmill
 :parameters ( ?p)
 :precondition
	(and (place ?p) (>= (available timber ?p) 2))
 :effect
	(and (has-sawmill ?p) (increase (labour) 2) (decrease (available timber ?p) 2)))

(:action build-mine
 :parameters ( ?p)
 :precondition
	(and (place ?p)  (metalliferous ?p) (>= (available wood ?p) 2))
 :effect
	(and (has-mine ?p) (increase (labour) 3) (decrease (available wood ?p) 2)))

(:action build-ironworks
 :parameters ( ?p)
 :precondition
	(and (place ?p)  (>= (available stone ?p) 2) (>= (available wood ?p) 2))
 :effect
	(and (has-ironworks ?p) (increase (labour) 3) (decrease (available stone ?p) 2) (decrease (available wood ?p) 2)))

(:action build-docks
 :parameters ( ?p)
 :precondition
	(and (place ?p)  (by-coast ?p) (>= (available stone ?p) 2) (>= (available wood ?p) 2))
 :effect
	(and (has-docks ?p) (decrease (available stone ?p) 2) (decrease (available wood ?p) 2) (increase (labour) 2)))

(:action build-wharf
 :parameters ( ?p)
 :precondition
	(and (place ?p)  (has-docks ?p) (>= (available stone ?p) 2) (>= (available iron ?p) 2))
 :effect
	(and (has-wharf ?p) (decrease (available stone ?p) 2) (decrease (available iron ?p) 2) (increase (labour) 2)))

(:action build-rail
 :parameters ( ?p1 ?p2)
 :precondition
	(and (place ?p1) (place ?p2)  (connected-by-land ?p1 ?p2) (>= (available wood ?p1) 1) (>= (available iron ?p1) 1))
 :effect
	(and (connected-by-rail ?p1 ?p2) (decrease (available wood ?p1) 1) (decrease (available iron ?p1) 1) (increase (labour) 2)))

(:action build-house
 :parameters ( ?p)
 :precondition
	(and (place ?p)  (>= (available wood ?p) 1) (>= (available stone ?p) 1))
 :effect
	(and (increase (housing ?p) 1) (decrease (available wood ?p) 1) (decrease (available stone ?p) 1)))

(:action build-cart
 :parameters ( ?p ?v)
 :precondition
	(and (place ?p) (vehicle ?v)  (>= (available timber ?p) 1) (potential ?v))
 :effect
	(and (is-at ?v ?p) (is-cart ?v) (not (potential ?v)) (forall (?r ) (when (resource ?r)  (assign (available ?r ?v) 0))) (decrease (available timber ?p) 1) (assign (space-in ?v) 1) (increase (labour) 1)))

(:action build-train
 :parameters ( ?p ?v)
 :precondition
	(and (place ?p) (vehicle ?v)  (potential ?v) (>= (available iron ?p) 2))
 :effect
	(and (is-at ?v ?p) (is-train ?v) (not (potential ?v)) (forall (?r ) (when (resource ?r)  (assign (available ?r ?v) 0))) (decrease (available iron ?p) 2) (assign (space-in ?v) 5) (increase (labour) 2)))

(:action build-ship
 :parameters ( ?p ?v)
 :precondition
	(and (place ?p) (vehicle ?v)  (potential ?v) (>= (available iron ?p) 4))
 :effect
	(and (has-wharf ?p) (is-at ?v ?p) (is-ship ?v) (not (potential ?v)) (forall (?r ) (when (resource ?r)  (assign (available ?r ?v) 0))) (decrease (available iron ?p) 4) (assign (space-in ?v) 10) (increase (labour) 3)))

(:action fell-timber
 :parameters ( ?p)
 :precondition
	(and (place ?p) (has-cabin ?p))
 :effect
	(and (increase (available timber ?p) 1) (increase (labour) 1)))

(:action break-stone
 :parameters ( ?p)
 :precondition
	(and (place ?p) (has-quarry ?p))
 :effect
	(and (increase (available stone ?p) 1) (increase (labour) 1) (increase (resource-use) 1)))

(:action mine-ore
 :parameters ( ?p)
 :precondition
	(and (place ?p) (has-mine ?p))
 :effect
	(and (increase (available ore ?p) 1) (increase (resource-use) 2)))

(:action burn-coal
 :parameters ( ?p)
 :precondition
	(and (place ?p)  (has-coal-stack ?p) (>= (available timber ?p) 1))
 :effect
	(and (decrease (available timber ?p) 1) (increase (available coal ?p) 1) (increase (pollution) 1)))

(:action saw-wood
 :parameters ( ?p)
 :precondition
	(and (place ?p)  (has-sawmill ?p) (>= (available timber ?p) 1))
 :effect
	(and (decrease (available timber ?p) 1) (increase (available wood ?p) 1)))

(:action make-iron
 :parameters ( ?p)
 :precondition
	(and (place ?p)  (has-ironworks ?p) (>= (available ore ?p) 1) (>= (available coal ?p) 2))
 :effect
	(and (decrease (available ore ?p) 1) (decrease (available coal ?p) 2) (increase (available iron ?p) 1) (increase (pollution) 2)))

)
