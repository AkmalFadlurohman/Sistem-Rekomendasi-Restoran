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

(deftemplate recommendation
	(slot restaurant (type STRING) (default ?DERIVE))
	(slot rating (type NUMBER))
	(slot distance (type NUMBER))
	(slot position (type NUMBER) (range 1 10))
)

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

;	RULE INISIASI
(defrule init-input
	?f1 <- (new-init-input yes)
=>
	(retract ?f1)
	(assert (get smoke))
	(assert (get min))
	(assert (get max))
	(assert (get dresscode))
	(assert (get wifi))
	(assert (get latitude))
	(assert (get longitude))
)

(defrule init-rec
	(restaurant-data (name ?name))
=>
	(assert (generate-rank ?name))
	(assert (check-for ?name smoke))
	(assert (check-for ?name min-budget))
	(assert (check-for ?name max-budget))
	(assert (check-for ?name dresscode))
	(assert (check-for ?name wifi))
)

(defrule gen-initial-rank
	?f1 <- (generate-rank ?name)
	(restaurant-data (name ?name) (latitude ?res-lat) (longitude ?res-long))
	(user latitude ?lat)
	(test (numberp ?lat))
	(user longitude ?long)
	(test (numberp ?long))
	?f2 <- (next-restaurant-count ?c)
=>
	(retract ?f1)
	(retract ?f2)
	(assert (restaurant ?name score 0 distance (find-distance ?lat ?long ?res-lat ?res-long) rank ?c))
	(assert (next-restaurant-count (+ ?c 1)))
)



;	RULE MENERIMA INPUT
(defrule getLongitude
	?f1 <- (get longitude)
=>
	(retract ?f1)
	(printout t "Insert your longitude. ('-' for default longitude)" crlf)
	(printout t "> ")
	(assert (user longitude (read)))
)

(defrule getLatitude
	?f1 <- (get latitude)
=>
	(retract ?f1)
	(printout t "Insert your latitude. ('-' for default latitude)" crlf)
	(printout t "> ")
	(assert (user latitude (read)))
)

(defrule getWifi
	?f1 <- (get wifi)
=>
	(retract ?f1)
	(printout t "Do you need Wifi? (yes/no or '-')" crlf)
	(printout t "> ")
	(assert (user wifi (read)))
)

(defrule getDresscode
	?f1 <- (get dresscode)
=>
	(retract ?f1)
	(printout t "What's your dresscode preference? (casual / informal / formal or '-')" crlf)
	(printout t "> ")
	(assert (user dresscode (read)))
)

(defrule getMax
	(user min-budget ?min&~-)
	?f1 <- (get max)
=>
	(retract ?f1)
	(printout t "What's you maximum budget? (0 - 9999 or '-')" crlf)
	(printout t "> ")
	(assert (user max-budget (read)))
)

(defrule getMin
	?f1 <- (get min)
=>
	(retract ?f1)
	(printout t "What's you minimum budget? (0 - 9999 or '-')" crlf)
	(printout t "> ")
	(assert (user min-budget (read)))
)

(defrule getSmoke
	?f1 <- (get smoke)
=>
	(retract ?f1)
	(printout t "Do you want to smoke? (yes/no or '-')" crlf)
	(printout t "> ")
	(assert (user smoke (read)))
)

(defrule getName
	?f1 <- (new-input yes)
=>
	(retract ?f1)
	(printout t "What's your name? ")
	(assert(user name (readline)))
)



;	RULES CEK INPUT

(defrule badSmoke
	?f1 <- (user smoke ?s&~yes&~no&~-)
=>
	(retract ?f1)
	(printout t "Reinput smoke preference (you entered '" ?s "')" crlf)
	(printout t "> ")
	(assert (user smoke (read)))
)

(defrule badType-min
	?f1 <- (user min-budget ?min&~-)
	(test (not (numberp ?min)))
=>
	(retract ?f1)
	(printout t "Reinput minimum budget (you entered '" ?min "')" crlf)
	(printout t "> ")
	(assert (user min-budget (read)))
)

(defrule outOfBound-min
	?f1 <- (user min-budget ?min&~-)
	(test (numberp ?min))
	(test (or (< ?min 0) (> ?min 9999)))
=>
	(retract ?f1)
	(printout t "Reinput minimum budget (you entered '" ?min "')" crlf)
	(printout t "> ")
	(assert (user min-budget (read)))	
)

(defrule badType-max
	(user min-budget ?min&~-)
	?f1 <- (user max-budget ?max)
	(test (not (numberp ?max)))
=>
	(retract ?f1)
	(printout t "Reinput maximum budget (you entered '" ?max "' for maximum)" crlf)
	(printout t "> ")
	(assert (user max-budget (read)))
)

(defrule outOfBound-max
	?f0 <- (user min-budget ?min&~-)
	?f1 <- (user max-budget ?max)
	(test (numberp ?max))
	(test (or (< ?max 0) (> ?max 9999)))
=>
	(retract ?f1)
	(printout t "Reinput maximum budget (you entered '" ?max "' for maximum)" crlf)
	(printout t "> ")
	(assert (user max-budget (read)))
)

(defrule badRange-max
	(user min-budget ?min&~-)
	?f1 <- (user max-budget ?max)
	(test (numberp ?min))
	(test (numberp ?max))
	(test (< ?max ?min))
=>
	(retract ?f1)
	(printout t "Reinput maximum budget (you entered '" ?max "' for maximum)" crlf)
	(printout t "> ")
	(assert (user max-budget (read)))	
)

(defrule badDresscode
	?f1 <- (user dresscode ?dress&~casual&~informal&~formal&~-)
=>
	(printout t "Reinput dresscode preference (you entered '" ?dress "')" crlf)
	(printout t "> ")
	(retract ?f1)
	(assert (user dresscode (read)))
)

(defrule badWifi
	?f1 <- (user wifi ?w&~yes&~no&~-)
=>
	(retract ?f1)
	(printout t "Reinput wifi preference (you entered '" ?w "')" crlf)
	(printout t "> ")
	(assert (user wifi (read)))
)

(defrule badType-latitude
	?f1 <- (user latitude ?lat&~-)
	(test (not (numberp ?lat)))
=>
	(retract ?f1)
	(printout t "Reinput latitude with NUMERIC value (you entered '" ?lat "')" crlf)
	(printout t "> ")
	(assert (user latitude (read)))
)

(defrule badLongitude
	?f1 <- (user longitude ?long&~-)
	(test (not (numberp ?long)))
=>
	(retract ?f1)
	(printout t "Reinput longitude with NUMERIC value (you entered '" ?long "')" crlf)
	(printout t "> ")
	(assert (user longitude (read)))
)

(defrule convertEmptySmoke
	?f1 <- (user smoke -)
=>
	(retract ?f1)
	(assert (user smoke no))
)

(defrule convertEmptyWifi
	?f1 <- (user wifi -)
=>
	(retract ?f1)
	(assert (user wifi yes))
)

(defrule convertEmptyDresscode
	?f1 <- (user dresscode -)
=>
	(retract ?f1)
	(assert (user dresscode casual))
)

(defrule convertEmptyLatitude
	?f1 <- (user latitude -)
=>
	(retract ?f1)
	(assert (user latitude 0))
)

(defrule convertEmptyLongitude
	?f1 <- (user longitude -)
=>
	(retract ?f1)
	(assert (user longitude 0))
)



;	RULE UNTUK NILAI RESTORAN

(defrule checkSmoke
	?f1 <- (check-for ?name smoke)
	(restaurant-data (name ?name) (smoke ?s))
	(user smoke ?s)
	?f2 <- (restaurant ?name score ?score distance ?d rank ?r)
=>
	(retract ?f1)
	(retract ?f2)
	(assert (restaurant ?name score (+ ?score 1) distance ?d rank ?r))
)

(defrule checkMin
	?f1 <- (check-for ?name min-budget)
	(restaurant-data (name ?name) (min-budget ?res-min))
	(user min-budget ?user-min&~-)
	(test (>= ?user-min ?res-min))
	?f2 <- (restaurant ?name score ?score distance ?d rank ?r)
=>
	(retract ?f1)
	(retract ?f2)
	(assert (restaurant ?name score (+ ?score 1) distance ?d rank ?r))
)

(defrule checkMax
	?f1 <- (check-for ?name max-budget)
	(restaurant-data (name ?name) (max-budget ?res-max))
	(user max-budget ?user-max)
	(test (>= ?user-max ?res-max))
	?f2 <- (restaurant ?name score ?score distance ?d rank ?r)
=>
	(retract ?f1)
	(retract ?f2)
	(assert (restaurant ?name score (+ ?score 1) distance ?d rank ?r))
)

(defrule checkDresscode
	?f1 <- (check-for ?name dresscode)
	(restaurant-data (name ?name) (dresscode $?res-dress))
	(user dresscode ?user-dress)
	(test (member ?user-dress ?res-dress))
	?f2 <- (restaurant ?name score ?score distance ?d rank ?r)
=>
	(retract ?f1)
	(retract ?f2)
	(assert (restaurant ?name score (+ ?score 1) distance ?d rank ?r))
)

(defrule checkWifi
	?f1 <- (check-for ?name wifi)
	(restaurant-data (name ?name) (wifi ?wifi))
	(user wifi ?wifi)
	?f2 <- (restaurant ?name score ?score distance ?d rank ?r)
=>
	(retract ?f1)
	(retract ?f2)
	(assert (restaurant ?name score (+ ?score 1) distance ?d rank ?r))
)



;	REPOSITION RULES

(defrule normal-score-reposition
	?f1 <- (restaurant ?name1 score ?score1 distance ?d1 rank ?rank1)
	?f2 <- (restaurant ?name2 score ?score2 distance ?d2 rank ?rank2)
	(test (< ?rank1 ?rank2))
	(test (> ?score2 ?score1))
=>
	(retract ?f1)
	(retract ?f2)
	(assert (restaurant ?name1 score ?score1 distance ?d1 rank ?rank2))
	(assert (restaurant ?name2 score ?score2 distance ?d2 rank ?rank1))
)

(defrule noBudget-score-reposition
	(user min-budget -)
	(restaurant-data (name ?name1) (min-budget ?min1))
	(restaurant-data (name ?name2) (min-budget ?min2))
	?f1 <- (restaurant ?name1 score ?score1 distance ?d1 rank ?rank1)
	?f2 <- (restaurant ?name2 score ?score2 distance ?d2 rank ?rank2)
	(test (< ?rank1 ?rank2))
	(test (eq ?score1 ?score2))
	(test (> ?min1 ?min2))

=>
	(retract ?f1)
	(retract ?f2)
	(assert (restaurant ?name1 score ?score1 distance ?d1 rank ?rank2))
	(assert (restaurant ?name2 score ?score2 distance ?d2 rank ?rank1))
)

(defrule distance-reposition
	?f1 <- (restaurant ?name1 score ?score1 distance ?d1 rank ?rank1)
	?f2 <- (restaurant ?name2 score ?score2 distance ?d2 rank ?rank2)
	(test (< ?rank1 ?rank2))
	(test (eq ?score1 ?score2))
	(test (> ?d1 ?d2))
=>
	(retract ?f1)
	(retract ?f2)
	(assert (restaurant ?name1 score ?score1 distance ?d1 rank ?rank2))
	(assert (restaurant ?name2 score ?score2 distance ?d2 rank ?rank1))	
)