; ====================================================
; TEMPLATES
; ====================================================
(deftemplate restaurant-data
	(slot name)

	(slot smoke (type SYMBOL) (allowed-symbols yes no))

	(slot min-budget (type NUMBER) (range 0 9999))

	(slot max-budget (type NUMBER) (range 0 9999))

	(multislot dresscode (type SYMBOL) (allowed-symbols casual formal informal))

	(slot wifi (type SYMBOL) (allowed-symbols yes no))

	(slot latitude (type NUMBER))

	(slot longitude (type NUMBER))
)

(deftemplate restaurant (slot name) (slot att) (multislot val))
(deftemplate user-input (slot att) (slot val (default ?NONE)))

; ==================================================================
; FACTS DEFINITIONS
; ==================================================================

(deffacts initial-facts
	(new-input yes)
	(new-init-input yes)
	(next-restaurant-count 1)
)


(deffacts restaurants-data
	(restaurant-data (name "Restaurant A") (smoke yes) (min-budget 1000) (max-budget 2000) (dresscode casual) (wifi yes) (latitude -6.8922186) (longitude 107.5886173))

	(restaurant-data (name "Restaurant B") (smoke no) (min-budget 1200) (max-budget 2500) (dresscode informal) (wifi yes) (latitude -6.224085) (longitude 106.7859815))

	(restaurant-data (name "Restaurant C") (smoke yes) (min-budget 2000) (max-budget 4000) (dresscode formal) (wifi no) (latitude -6.2145285) (longitude 106.8642591))

	(restaurant-data (name "Restaurant D") (smoke no) (min-budget 500) (max-budget 1400) (dresscode formal) (wifi no) (latitude -6.9005363) (longitude 107.6222191))

	(restaurant-data (name "Restaurant E") (smoke yes) (min-budget 1000) (max-budget 2000) (dresscode casual informal) (wifi yes) (latitude -6.2055617) (longitude 106.8001591))

	(restaurant-data (name "Restaurant F") (smoke no) (min-budget 2500) (max-budget 5000) (dresscode informal) (wifi yes) (latitude -6.9045679) (longitude 107.6399745))

	(restaurant-data (name "Restaurant G") (smoke yes) (min-budget 1300) (max-budget 3000) (dresscode casual) (wifi yes) (latitude -6.1881082) (longitude 106.7844409))

	(restaurant-data (name "Restaurant H") (smoke no) (min-budget 400) (max-budget 1000) (dresscode informal) (wifi no) (latitude -6.9525133) (longitude 107.6052906))

	(restaurant-data (name "Restaurant I") (smoke no) (min-budget 750) (max-budget 2200) (dresscode casual informal) (wifi yes) (latitude -6.9586985) (longitude 107.7092282))

	(restaurant-data (name "Restaurant J") (smoke yes) (min-budget 1500) (max-budget 2000) (dresscode casual) (wifi yes) (latitude -6.2769732) (longitude 106.775133))
)

; ==================================================================
; Functions
; ==================================================================

(deffunction find-distance (?x1 ?y1 ?x2 ?y2)
	(sqrt (+ (** (- ?y2 ?y1) 2) (** (- ?x2 ?x1) 2)))
)

; ==================================================================
; Rules
; ==================================================================

(defrule init-restaurant-list
	(restaurant-data (name ?name))
=>
	(assert (initiated ?name no))
)

(defrule make-restaurant-list
	?f1 <- (initiated ?name no)
	(restaurant-data (name ?name) (latitude ?res-lat) (longitude ?res-long))
	(user-input (att latitude) (val ?lat))
	(test (numberp ?lat))
	(user-input (att longitude) (val ?long))
	(test (numberp ?long))
	?f2 <- (next-restaurant-count ?c)
=>
	(retract ?f1)
	(retract ?f2)
	(assert (restaurant-score ?name score 0 distance (find-distance ?lat ?long ?res-lat ?res-long) rank ?c))
	(assert (next-restaurant-count (+ ?c 1)))
)

(defrule list-restaurant-attributes
	(restaurant-data (name ?name) (smoke ?smoke) (min-budget ?min) (max-budget ?max) (dresscode $?dresscodes) (wifi ?wifi) (latitude ?lat) (longitude ?long))
=>
	(assert (restaurant (name ?name) (att smoke) (val ?smoke)))
	(assert (restaurant (name ?name) (att min-budget) (val ?min)))
	(assert (restaurant (name ?name) (att max-budget) (val ?max)))
	(assert (restaurant (name ?name) (att dresscode) (val ?dresscodes)))
	(assert (restaurant (name ?name) (att wifi) (val ?wifi)))
	(assert (restaurant (name ?name) (att latitude) (val ?lat)))
	(assert (restaurant (name ?name) (att longitude) (val ?long)))
)

(defrule start-input
=>
	(printout t "Insert name" crlf)
	(printout t "> ")
	(assert (user-input (att name) (val (readline))))
	(printout t crlf)

	(printout t "Do you plan to smoke? (yes / no / -)" crlf)
	(printout t "> ")
	;(bind ?smoke (read))
	(assert (user-input (att smoke) (val (read))))
	(printout t crlf)

	(printout t "What's your minimum budget? (0 - 9999 or '-')" crlf)
	(printout t "> ")
	(bind ?min (read))
	(assert (user-input (att min-budget) (val ?min)))
	(printout t crlf)
	
	(if (neq ?min -) then
	(printout t "What's you maximum budget? (0 - 9999, greater than minimum)" crlf)
	(printout t "> ")
	(assert (user-input (att max-budget) (val (read))))
	(printout t crlf))

	(printout t "What's your dresscode preference? (casual / informal / formal / -)" crlf)
	(printout t "> ")
	(assert (user-input (att dresscode) (val (read))))
	(printout t crlf)

	(printout t "Do you need Wifi? (yes / no / -)" crlf)
	(printout t "> ")
	(assert (user-input (att wifi) (val (read))))
	(printout t crlf)

	(printout t "What's your latitude?" crlf)
	(printout t "> ")
	(assert (user-input (att latitude) (val (read))))
	(printout t crlf)

	(printout t "What's your longitude?" crlf)
	(printout t "> ")
	(assert (user-input (att longitude) (val (read))))
	(printout t crlf)
)

(defrule badSmoke
	?f1 <- (user-input (att smoke) (val ?s&~yes&~no&~-))
=>
	(retract ?f1)
	(printout t "Reinput smoke preference (you entered '" ?s "')" crlf)
	(printout t "> ")
	(assert (user-input (att smoke) (val (read))))
)

(defrule badType-min
	?f1 <- (user-input (att min-budget) (val ?min&~-))
	(test (not (numberp ?min)))
=>
	(retract ?f1)
	(printout t "Reinput minimum budget (you entered '" ?min "')" crlf)
	(printout t "> ")
	(assert (user-input (att min-budget) (val (read))))
)

(defrule outOfBound-min
	?f1 <- (user-input (att min-budget) (val ?min&~-))
	(test (numberp ?min))
	(test (or (< ?min 0) (> ?min 9999)))
=>
	(retract ?f1)
	(printout t "Reinput minimum budget (you entered '" ?min "')" crlf)
	(printout t "> ")
	(assert (user-input (att min-budget) (val (read))))
)

(defrule badType-max
	(user-input (att min-budget) (val ?min&~-))
	?f1 <- (user-input (att max-budget) (val ?max))
	(test (not (numberp ?max)))
=>
	(retract ?f1)
	(printout t "Reinput maximum budget (you entered '" ?max "' for maximum)" crlf)
	(printout t "> ")
	(assert(user-input (att max-budget) (val read)))
)

(defrule outOfBound-max
	?f0 <- (user-input (att min-budget) (val ?min&~-))
	?f1 <- (user-input (att max-budget) (val ?max))
	(test (numberp ?max))
	(test (or (< ?max 0) (> ?max 9999)))
=>
	(retract ?f1)
	(printout t "Reinput maximum budget (you entered '" ?max "' for maximum)" crlf)
	(printout t "> ")
	(assert (user-input (att max-budget) (val (read))))
)

(defrule badRange-max
	(user-input (att min-budget) (val ?min&~-))
	?f1 <- (user-input (att max-budget) (val ?max))
	(test (numberp ?min))
	(test (numberp ?max))
	(test (< ?max ?min))
=>
	(retract ?f1)
	(printout t "Reinput maximum budget (you entered '" ?max "' for maximum)" crlf)
	(printout t "> ")
	(assert (user-input (att max-budget) (val (read))))
)

(defrule badDresscode
	?f1 <- (user-input (att dresscode) (val ?dress&~casual&~informal&~formal&~-))
=>
	(retract ?f1)
	(printout t "Reinput dresscode preference (you entered '" ?dress "')" crlf)
	(printout t "> ")
	(assert (user-input (att dresscode) (val (read))))
)

(defrule badWifi
	?f1 <- (user-input (att wifi) (val ?w&~yes&~no&~-))
=>
	(retract ?f1)
	(printout t "Reinput wifi preference (you entered '" ?w "')" crlf)
	(printout t "> ")
	(assert (user-input (att wifi) (val (read))))
)

(defrule badType-latitude
	?f1 <- (user-input (att latitude) (val ?lat&~-))
	(test (not (numberp ?lat)))
=>
	(retract ?f1)
	(printout t "Reinput latitude with NUMERIC value (you entered '" ?lat "')" crlf)
	(printout t "> ")
	(assert (user-input (att latitude) (val (read))))
)

(defrule badType-longitude
	?f1 <- (user-input (att longitude) (val ?long&~-))
	(test (not (numberp ?long)))
=>
	(retract ?f1)
	(printout t "Reinput longitude with NUMERIC value (you entered '" ?long "')" crlf)
	(printout t "> ")
	(assert (user-input (att longitude) (val (read))))
)

(defrule convertEmptySmoke
	?f1 <- (user-input (att smoke) (val -))
=>
	(retract ?f1)
	(assert (user-input (att smoke) (val no)))
)

(defrule convertEmptyWifi
	?f1 <- (user-input (att wifi) (val -))
=>
	(retract ?f1)
	(assert (user-input (att wifi) (val yes)))
)

(defrule convertEmptyDresscode
	?f1 <- (user-input (att dresscode) (val -))
=>
	(retract ?f1)
	(assert (user-input (att dresscode) (val casual)))
)

(defrule convertEmptyLatitude
	?f1 <- (user-input (att latitude) (val -))
=>
	(retract ?f1)
	(assert (user-input (att latitude) (val 0)))
)

(defrule convertEmptyLongitude
	?f1 <- (user-input (att longitude) (val -))
=>
	(retract ?f1)
	(assert (user-input (att longitude) (val 0)))
)

(defrule checkCompatibility
	?f1 <- (restaurant (name ?name) (att ?att1&~min-budget&~max-budget&~latitude&~longitude) (val ?val))
	(user-input (att ?att2&?att1) (val ?val))
	?f2 <- (restaurant-score ?name score ?score distance ?d rank ?r)
=>
	(retract ?f1)
	(retract ?f2)
	(assert (restaurant-score ?name score (+ ?score 1) distance ?d rank ?r))
)

(defrule checkCompatibility-minBudget
	?f1 <- (restaurant (name ?name) (att min-budget) (val ?min))
	(user-input (att min-budget) (val ?val))
	(test (>= ?val ?min))
	?f2 <- (restaurant-score ?name score ?score distance ?d rank ?r)
=>
	(retract ?f1)
	(retract ?f2)
	(assert (restaurant-score ?name score (+ ?score 1) distance ?d rank ?r))	
)

(defrule checkCompatibility-maxBudget
	?f1 <- (restaurant (name ?name) (att max-budget) (val ?max))
	(user-input (att max-budget) (val ?val))
	(test (>= ?val ?max))
	?f2 <- (restaurant-score ?name score ?score distance ?d rank ?r)
=>
	(retract ?f1)
	(retract ?f2)
	(assert (restaurant-score ?name score (+ ?score 1) distance ?d rank ?r))	
)

(defrule normal-score-reposition
	?f1 <- (restaurant-score ?name1 score ?score1 distance ?d1 rank ?rank1)
	?f2 <- (restaurant-score ?name2 score ?score2 distance ?d2 rank ?rank2)
	(test (< ?rank1 ?rank2))
	(test (> ?score2 ?score1))
=>
	(retract ?f1)
	(retract ?f2)
	(assert (restaurant-score ?name1 score ?score1 distance ?d1 rank ?rank2))
	(assert (restaurant-score ?name2 score ?score2 distance ?d2 rank ?rank1))
)

(defrule distance-reposition
	?f1 <- (restaurant-score ?name1 score ?score1 distance ?d1 rank ?rank1)
	?f2 <- (restaurant-score ?name2 score ?score2 distance ?d2 rank ?rank2)
	(test (< ?rank1 ?rank2))
	(test (eq ?score1 ?score2))
	(test (> ?d1 ?d2))
=>
	(retract ?f1)
	(retract ?f2)
	(assert (restaurant-score ?name1 score ?score1 distance ?d1 rank ?rank2))
	(assert (restaurant-score ?name2 score ?score2 distance ?d2 rank ?rank1))	
)