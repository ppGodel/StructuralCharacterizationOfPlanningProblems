(define (problem ZTRAVEL-1-2)
(:domain zeno-travel)
(:objects
	plane1 - aircraft
	person1 - person
	person2 - person
	city0 - city
	city1 - city
	city2 - city
	fl0 - flevel
	fl1 - flevel
	fl2 - flevel
	fl3 - flevel
	fl4 - flevel
	fl5 - flevel
	fl6 - flevel
	)
(:init
	(at plane1 city0)
	(fuellevel plane1 fl1)
	(at person1 city0)
	(at person2 city2)
	(nxt fl0 fl1)
	(nxt fl1 fl2)
	(nxt fl2 fl3)
	(nxt fl3 fl4)
	(nxt fl4 fl5)
	(nxt fl5 fl6)
)
(:goal (and
	(at plane1 city1)
	(at person1 city0)
	(at person2 city2)
	))

)
