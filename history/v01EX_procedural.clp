;=====================================================
; CHANGELOGS
;=====================================================
; 9 September 2017	:	
;	Bikin 3 template:
;		- Data restoran, data user, dan rekomendasi

; 	Rules untuk basic input sudah bisa
;		- Input tanpa kriteria sudah bisa
;		- Pengecekan tipe dan range input sudah jalan

;	Rules untuk konversi input string jadi tipe simbol sudah jalan

;	Fakta awal sudah lengkap
;		- Data restoran sudah lengkap dan benar	

;	Menambah methods dan fungsi baru
;		- overload method float dan integer agar bisa konversi string ke number
;		- fungsi find-distance untuk mencari euclidian distance

; NOTES:
; ingat pakai fungsi 'member' 
;=====================================================


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

(deftemplate user-data
	(slot id (type NUMBER) (range 1 ?VARIABLE))

	(slot name (type STRING) (default ?DERIVE))

	(multislot smoke (type SYMBOL) (allowed-symbols yes no) (default no))

	(slot min-budget (type NUMBER) (range 0 9999) (default 9999))

	(slot max-budget (type NUMBER) (range 0 9999) (default 9999))

	(multislot dresscode (type SYMBOL) (allowed-symbols casual formal informal) (default casual))

	(multislot wifi (type SYMBOL) (allowed-symbols yes no) (default yes))

	(slot latitude (type NUMBER) (default 0.0))

	(slot longitude (type NUMBER) (default 0.0))
)

(deftemplate recommendation
	(slot user-id (type NUMBER))
	(slot restaurant (type STRING) (default ?DERIVE))
	(slot rating (type NUMBER))
	(slot distance (type NUMBER))
	(slot position (type NUMBER) (range 1 10))
)

; ==================================================================
; Method Overloads
; ==================================================================

(defmethod float ((?s STRING))
   (float (string-to-field ?s)))

(defmethod integer ((?s STRING))
   (integer (string-to-field ?s)))

; ==================================================================
; Functions
; ==================================================================

(deffunction find-distance (?x1 ?y1 ?x2 ?y2)
	(sqrt (+ (** (- ?y2 ?y1) 2) (** (- ?x2 ?x1) 2)))
)

(deffunction check-distance (?d1 ?d2)
	(<= ?d1 ?d2)
)

(deffunction check-ranking (?r1 ?r2 ?d1 ?d2)
	(if (> ?r1 ?r2 ) then TRUE
	else 
		(if (eq ?r1 ?r2) then (check-distance ?d1 ?d2) else FALSE)
	)
)

(deffunction rating-to-words (?rating ?max-score)
	(if (>= ?rating ?max-score) then "Very Recommended"
	else (if (>= ?rating (- ?max-score 2)) then "Recommended"
			else "Not Recommended"
	))
)

; ==================================================================
; Rules
; ==================================================================


; 					RULE UNTUK INPUT INFORMASI
(defrule init-input
	(new-input yes)
	?f2 <- (next-user-id ?id)
=>
	; Hapus fakta kontrol dan menambah hitungan user
	(retract ?f2)

	; Masukkan user-data dengan 
	(assert (next-user-id (+ ?id 1)))


	; Mulai input
	(printout t crlf)
	(printout t "============================================================" crlf)
	(printout t "Please enter informations that are required to give you our recommendation" crlf).
	(printout t "You have to insert a valid input, or you may leave some fields blank if you don't have any preference." crlf)
	(printout t "============================================================" crlf crlf)

	(printout t "Insert name" crlf)
	(printout t "> ")
	(assert (user ?id name (readline)))
	(printout t crlf)

	(printout t "Planning to smoke? (yes/no or leave it blank)" crlf)
	(printout t "> ")
	(assert (user ?id smoke (readline)))
	(printout t crlf)

	(printout t "Insert your minimum-budget (0 - 9999 or '-')" crlf)
	(printout t "> ")
	(bind ?min (read))
	(if (floatp ?min) then (bind ?min (integer ?min))
	 else (if (eq ?min -) then (bind ?min 9999)))
	(while (not (integerp ?min)) do
		(printout t "Please enter a valid NUMERIC value or '-' for 'minimum-budget' (0 - 9999 or '-')" crlf)
		(printout t "minimum budget > ")
		(bind ?min (read))

		(if (floatp ?min) then (bind ?min (integer ?min))
	 	 else (if (eq ?min -) then (bind ?min 9999)))
	)
	(assert (user ?id min-budget ?min))
	(printout t crlf)

	(printout t "Insert your maximum-budget (0 - 9999 or '-')" crlf)
	(printout t "> ")
	(bind ?max (read))
	(if (floatp ?max) then (bind ?max (integer ?max))
	 else (if (eq ?max -) then (bind ?max 9999))

	)
	(while (not (integerp ?max)) do
		(printout t "Please enter a valid NUMERIC value or '-' for 'maximum-budget' (0 - 9999 or '-')" crlf)
		(printout t "maximum budget > ")
		(bind ?max (read))

		(if (floatp ?max) then (bind ?max (integer ?max))
	 	 else (if (eq ?max -) then (bind ?max 9999))
	))
	(assert (user ?id max-budget ?max))
	(printout t crlf)

	(printout t "Do you need wifi? (yes/no or leave it blank)" crlf)
	(printout t "> ")
	(assert (user ?id wifi (readline)))
	(printout t crlf)

	(printout t "Insert your preferred dresscode (casual / informal / formal or leave it blank)" crlf)
	(printout t "> ")
	(assert (user ?id dresscode (readline)))
	(printout t crlf)

	(printout t "Insert your latitude (insert '-' for absolute distance)" crlf)
	(printout t "> ")
	(bind ?lat (read))
	(if (integerp ?lat) then (bind ?lat (float ?lat))
	 else (if (eq ?lat -) then (bind ?lat 0.0)))
	(while (not (floatp ?lat)) do
		(printout t "Please enter a valid NUMERIC value or '-' for 'latitude'" crlf)
		(printout t "latitude > ")
		(bind ?lat (read))

		(if (integerp ?lat) then (bind ?lat (float ?lat))
	 	 else (if (eq ?lat -) then (bind ?lat 0.0))
	))
	(assert (user ?id latitude ?lat))
	(printout t crlf)

	(printout t "Insert your longitude (insert '-' for absolute distance)" crlf)
	(printout t "> ")
	(bind ?long (read))
	(if (integerp ?long) then (bind ?long (float ?long))
	 else (if (eq ?long -) then (bind ?long 0.0)))
	(while (not (floatp ?long)) do
		(printout t "Please enter a valid NUMERIC value or '-' for 'longitude'" crlf)
		(printout t "longitude > ")
		(bind ?long (read))
		(if (integerp ?long) then (bind ?long (float ?long))
	 	 else (if (eq ?long -) then (bind ?long 0.0))
	 	 )
	)
	(assert (user ?id longitude ?long))
	(printout t crlf)
)





; 					RULES UNTUK MENGECEK KEVALIDAN INPUT

(defrule error-smoke
	?f1 <- (user ?id smoke ~"yes"&~"no"&~"")
=>
	(retract ?f1)

	(printout t "Please enter valid input for 'smoking' (yes/no or leave it blank)" crlf)
	(printout t "> ")
	(assert (user ?id smoke (readline)))
	(printout t crlf)
)

(defrule error-min-budget
	(declare (salience 100))
	?f1 <- (user ?id min-budget ?min)
	(test (or (< ?min 0) (> ?min 9999)))
=>
	(retract ?f1)

	(printout t "Please enter a valid value for 'minimum-budget' (0 - 9999)" crlf)
	(printout t "You entered min: " ?min crlf)
	(printout t "minimum budget > ")
	(bind ?answer (read))
	(if (floatp ?answer) then (bind ?answer (integer ?answer))
	 else (if (eq ?answer -) then (bind ?answer 9999)))
	(while (not (integerp ?answer)) do
		(printout t "Please enter a valid NUMERIC value or '-' for 'minimum-budget' (0 - 9999 or '-')" crlf)
		(printout t "minimum budget > ")
		(bind ?answer (read))

		(if (floatp ?answer) then (bind ?answer (integer ?answer))
	 	 else (if (eq ?answer -) then (bind ?answer 9999)))
	)
	(assert (user ?id min-budget ?answer))
	(printout t crlf)
)

(defrule error-max-budget
	(declare (salience 100))
	?f1 <- (user ?id max-budget ?max)
	(test (or (> ?max 9999) (< ?max 0)))
=>
	(retract ?f1)

	(printout t "Please enter a valid value for 'max-budget' (0 - 9999)" crlf)
	(printout t "You entered max: " ?max crlf)
	(printout t "maximum budget > ")
	(bind ?answer (read))
	(if (floatp ?answer) then (bind ?answer (integer ?answer))
	 else (if (eq ?answer -) then (bind ?answer 9999)))
	(while (not (integerp ?answer)) do
		(printout t "Please enter a valid NUMERIC value or '-' for 'maximum-budget' (0 - 9999 or '-')" crlf)
		(printout t "maximum budget > ")
		(bind ?answer (read))

		(if (floatp ?answer) then (bind ?answer (integer ?answer))
	 	 else (if (eq ?answer -) then (bind ?answer 9999)))
	)
	(assert (user ?id max-budget ?answer))
	(printout t crlf)
)

(defrule error-budget-range
	?f1 <- (user ?id min-budget ?min)
	?f2 <- (user ?id max-budget ?max)
	(test (> ?min ?max))
=>
	(retract ?f1)
	(retract ?f2)

	(printout t "Please enter a valid range for 'budget' (min <= max budget)" crlf)
	(printout t "You entered min: " ?min " and max: " ?max crlf)
	(printout t "minimum budget > ")
	(bind ?answer (read))
	(if (floatp ?answer) then (bind ?answer (integer ?answer))
	 else (if (eq ?answer -) then (bind ?answer 9999)))
	(while (not (integerp ?answer)) do
		(printout t "Please enter a valid NUMERIC value or '-' for 'minimum-budget' (0 - 9999 or '-')" crlf)
		(printout t "minimum budget > ")
		(bind ?answer (read))

		(if (floatp ?answer) then (bind ?answer (integer ?answer))
	 	 else (if (eq ?answer -) then (bind ?answer 9999)))
	)
	(assert (user ?id min-budget ?answer))

	(printout t "maximum budget > ")
	(bind ?answer (read))
	(if (floatp ?answer) then (bind ?answer (integer ?answer))
	 else (if (eq ?answer -) then (bind ?answer 9999)))
	(while (not (integerp ?answer)) do
		(printout t "Please enter a valid NUMERIC value or '-' for 'maximum-budget' (0 - 9999 or '-')" crlf)
		(printout t "maximum budget > ")
		(bind ?answer (read))

		(if (floatp ?answer) then (bind ?answer (integer ?answer))
	 	 else (if (eq ?answer -) then (bind ?answer 9999)))
	)
	(assert (user ?id max-budget ?answer))
	(printout t crlf)
)

(defrule error-dresscode
	?f1 <- (user ?id dresscode ~"casual"&~"informal"&~"formal"&~"")
=>
	(retract ?f1)
	(printout t "Please enter a valid value for 'dresscode' (casual / informal / formal or leave it blank)" crlf)
	(printout t "dresscode > ")
	(assert (user ?id dresscode (readline)))
	(printout t crlf)
)

(defrule error-wifi
	?f1 <- (user ?id wifi ~"yes"&~"no"&~"")
=>
	(retract ?f1)
	(printout t "Please enter a valid value for 'wifi' (yes/no or leave it blank)" crlf)
	(printout t "wifi > ")
	(assert (user ?id wifi (readline)))
	(printout t crlf)
)







; 					RULE UNTUK MEMPROSES INPUT
(defrule process-input
	?f0 <- (new-input yes)
	?f1 <- (user ?id name ?name)

	?f2 <- (user ?id smoke ?smoke)
	(test (or (eq ?smoke "yes") (eq ?smoke "no") (eq ?smoke "")))

	?f3 <- (user ?id min-budget ?min)
	(test (>= ?min 0))

	?f4 <- (user ?id max-budget ?max)
	(test (<= ?max 9999))
	(test (<= ?min ?max))

	?f5 <- (user ?id dresscode ?dress)
	(test (or (eq ?dress "casual") (eq ?dress "informal") (eq ?dress "formal") (eq ?dress "")))

	?f6 <- (user ?id wifi ?wifi)
	(test (or (eq ?wifi "yes") (eq ?wifi "no") (eq ?wifi "")))

	?f7 <- (user ?id latitude ?lat)
	(test (floatp ?lat))

	?f8 <- (user ?id longitude ?long)
	(test (floatp ?long))
=>
	(retract ?f0)
	(retract ?f1)
	(retract ?f2)
	(retract ?f3)
	(retract ?f4)
	(retract ?f5)
	(retract ?f6)
	(retract ?f7)
	(retract ?f8)
	
	(assert (user-data 
		(id ?id)
		(name ?name)
		(smoke (if (neq ?smoke "") then (explode$ ?smoke) else no))
		(min-budget ?min)
		(max-budget ?max)
		(wifi (if (neq ?wifi "") then (explode$ ?wifi) else yes))
		(dresscode (if (neq ?dress "") then (explode$ ?dress) else casual))
		(latitude (float ?lat))
		(longitude (float ?long))))

	(assert (generate-rec yes))
	(assert (initial-pos 1))
)



;					 RULE GENERATE RECOMMENDATION

(defrule init-rec
	(generate-rec yes)
	(restaurant-data (name ?name))
=>
	(assert (generate-rec ?name))
)

(defrule generate-rec
	(restaurant-data (name ?res_name) (smoke ?res_smoke) (min-budget ?res_min) (max-budget ?res_max) (dresscode $?res_dress) (wifi ?res_wifi) (latitude ?res_lat) (longitude ?res_long))

	?f1 <- (generate-rec yes)
	?f2 <- (initial-pos ?pos)
	?f3 <- (generate-rec ?res_name)

	(user-data (id ?id) (name ?name) (smoke ?smoke) (min-budget ?min) (max-budget ?max) (dresscode ?dress) (wifi ?wifi) (latitude ?lat) (longitude ?long))

=>
;	(retract ?f1)
	(retract ?f2)
	(retract ?f3)

	(if (< ?pos 10) then (assert (initial-pos (+ ?pos 1)))
	else (assert (reposition-list yes))
	)

	(bind ?rating (+ 
		(if (eq ?smoke ?res_smoke) then 1 else 0)
		(if (>= ?min ?res_min) then 1 else 0)
		(if (member ?dress ?res_dress) then 1 else 0)
		(if (eq ?wifi ?res_wifi) then 1 else 0)
		(if (>= ?max ?res_max) then 1 else 0)

	))

	(assert (recommendation (user-id ?id) (restaurant ?res_name) (rating ?rating) (distance (find-distance ?lat ?long ?res_lat ?res_long)) (position ?pos))
	)
)

;					RULE REPOSITIONING LIST

(defrule reposition-list
	(generate-rec yes)
	(reposition-list yes)

	?f1 <- (recommendation (user-id ?id) (restaurant ?name1) (rating ?r1) (distance ?d1)(position ?pos1))
	?f2 <- (recommendation (user-id ?id) (restaurant ?name2&~?name1) (rating ?r2) (distance ?d2) (position ?pos2&~?pos1))

	(test (< ?pos1 ?pos2))
=>
	(if (< ?r1 ?r2) then 
		(modify ?f1 (position ?pos2))
		(modify ?f2 (position ?pos1))
	)

	(if (and (eq ?r1 ?r2) (> ?d1 ?d2)) then 
		(modify ?f1 (position ?pos2))
		(modify ?f2 (position ?pos1))
	)
)

(defrule reposition-stop
	?f1 <- (generate-rec yes)
	?f2 <- (reposition-list yes)

	(recommendation (user-id ?id) (rating ?r1) (distance ?d1) (position 1))
	(recommendation (user-id ?id) (rating ?r2) (distance ?d2) (position 2))
	(recommendation (user-id ?id) (rating ?r3) (distance ?d3) (position 3))
	(recommendation (user-id ?id) (rating ?r4) (distance ?d4) (position 4))
	(recommendation (user-id ?id) (rating ?r5) (distance ?d5) (position 5))
	(recommendation (user-id ?id) (rating ?r6) (distance ?d6) (position 6))
	(recommendation (user-id ?id) (rating ?r7) (distance ?d7) (position 7))
	(recommendation (user-id ?id) (rating ?r8) (distance ?d8) (position 8))
	(recommendation (user-id ?id) (rating ?r9) (distance ?d9) (position 9))
	(recommendation (user-id ?id)  (rating ?r10) (distance ?d10) (position 10))

	(test (and 
				(check-ranking ?r1 ?r2 ?d1 ?d2)
				(check-ranking ?r2 ?r3 ?d2 ?d3)
				(check-ranking ?r3 ?r4 ?d3 ?d4)
				(check-ranking ?r4 ?r5 ?d4 ?d5)
				(check-ranking ?r5 ?r6 ?d5 ?d6)
				(check-ranking ?r6 ?r7 ?d6 ?d7)
				(check-ranking ?r7 ?r8 ?d7 ?d8)
				(check-ranking ?r8 ?r9 ?d8 ?d9)
				(check-ranking ?r9 ?r10 ?d9 ?d10)
	))
=>
	(retract ?f1)
	(retract ?f2)
	(assert (print-time yes))
)

;					RULE UNTUK PRINT SOLUSI

(defrule print-recommendation
	?f1 <- (print-time yes)
	(max-score ?max-score)
	(recommendation (user-id ?id) (restaurant ?name1) (rating ?r1) (distance ?d1) (position 1))
	(recommendation (user-id ?id) (restaurant ?name2) (rating ?r2) (distance ?d2) (position 2))
	(recommendation (user-id ?id) (restaurant ?name3) (rating ?r3) (distance ?d3) (position 3))
	(recommendation (user-id ?id) (restaurant ?name4) (rating ?r4) (distance ?d4) (position 4))
	(recommendation (user-id ?id) (restaurant ?name5) (rating ?r5) (distance ?d5) (position 5))
	(recommendation (user-id ?id) (restaurant ?name6) (rating ?r6) (distance ?d6) (position 6))
	(recommendation (user-id ?id) (restaurant ?name7) (rating ?r7) (distance ?d7) (position 7))
	(recommendation (user-id ?id) (restaurant ?name8) (rating ?r8) (distance ?d8) (position 8))
	(recommendation (user-id ?id) (restaurant ?name9) (rating ?r9) (distance ?d9) (position 9))
	(recommendation (user-id ?id) (restaurant ?name10) (rating ?r10) (distance ?d10) (position 10))
=>
	(format t " No. | %-16s | %-12s | %-6s%n" Rating Name Distance)
	(format t "------------------------------------------------------------%n")
	(format t "  1  | %-16s | %-12s | %-6.3f%n" (rating-to-words ?r1 ?max-score) ?name1 ?d1)
	(format t "  2  | %-16s | %-12s | %-6.3f%n" (rating-to-words ?r2 ?max-score) ?name2 ?d2)
	(format t "  3  | %-16s | %-12s | %-6.3f%n" (rating-to-words ?r3 ?max-score) ?name3 ?d3)
	(format t "  4  | %-16s | %-12s | %-6.3f%n" (rating-to-words ?r4 ?max-score) ?name4 ?d4)
	(format t "  5  | %-16s | %-12s | %-6.3f%n" (rating-to-words ?r5 ?max-score) ?name5 ?d5)
	(format t "  6  | %-16s | %-12s | %-6.3f%n" (rating-to-words ?r6 ?max-score) ?name6 ?d6)
	(format t "  7  | %-16s | %-12s | %-6.3f%n" (rating-to-words ?r7 ?max-score) ?name7 ?d7)
	(format t "  8  | %-16s | %-12s | %-6.3f%n" (rating-to-words ?r8 ?max-score) ?name8 ?d8)
	(format t "  9  | %-16s | %-12s | %-6.3f%n" (rating-to-words ?r9 ?max-score) ?name9 ?d9)
	(format t " 10  | %-16s | %-12s | %-6.3f%n%n" (rating-to-words ?r10 ?max-score) ?name10 ?d10)

	(retract ?f1)
	(assert (ask-to-repeat yes))
)

;							RULE UNTUK ULANG PENCARIAN
(defrule find-again
	?f1 <- (ask-to-repeat yes)
=>
	(retract ?f1)
	(printout t "Would you like to have another recommendation? (yes/no)" crlf)
	(bind ?answer "empty")

	(while (and (neq ?answer "yes") (neq ?answer "no")) do
		(printout t "> ")
		(bind ?answer (readline))
	)

	(if (eq ?answer "yes") then (assert (new-input yes)) else (printout t "Thank you for using our product :D" crlf crlf))

)


; ==================================================================
; FACTS DEFINITIONS
; ==================================================================

(deffacts initial-facts
	(new-input yes)
	(next-user-id 1)
	(max-score 5)
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



