(defrule start
?state <-(state movie)
=>
(printout t crlf)(printout t crlf)
(printout t "=================================================================="crlf)
(printout t "                       WELCOME TO CINEMA GURU                     "crlf)
(printout t "=================================================================="crlf)
(printout t crlf)
(printout t crlf))


(defrule map
?state <- (state ?s)
?op <- (op continue)
?list <- (sequence_list ?head $?tail)
=>
(retract ?state ?op ?list)
(assert (state ?head))
(assert (sequence_list ?tail ?head))
(if (eq ?s end)
 then (reset)))

(deffunction insert_favor_movies (?movielist)
(if (> (length ?movielist) 0)
then
(assert (favor (first$ ?movielist)))
(bind ?*favor_count* (+ ?*favor_count* 1)) ;;modified by chenxi
(insert_favor_movies (rest$ ?movielist))))

(defrule ask_movies
(state movie)
(initial-fact)
=>
(printout t "Please enter your favorite movies by index, separated by space" crlf)
(printout t "For example: 10 11 20 22 7" crlf)
(bind ?line (readline))
(insert_favor_movies (explode$ ?line))
(assert (op continue)))

(deftemplate moviecomputed
(slot index)
(slot iscatcomputed (allowed-symbols yes no) (default no))
(slot istagcomputed (allowed-symbols yes no) (default no))
)

(defrule assertmoviecomputed
(declare (salience 10))
(state movie)
(movie (index ?i) (is_released no))
=>
(assert (moviecomputed (index ?i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TAG;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftemplate tag_list
	(slot tag_name)
	(slot tag_index))

(defrule ask_tag
	(state tag)
	(exists (favor ?))
	=>
	(printout t "Please enter your 5 favorite tags by index ordered by preference, seperated by space" crlf)
	(printout t "For example: 1 2 3 4 5 (tag 1 is the most favorite)" crlf)
	(bind ?*count* ?*favor_count*)
	(assert (state tag_print)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deffunction format_display (?content ?index)
	(loop-for-count (?cnt 1 (- (- 30 (str-length ?content)) (str-length (implode$ (create$ ?index))))) do
		(printout t " "))
	(if (evenp ?index)
		then
		(printout t "" crlf)))

(defrule insert_tag
	(state tag_print)
	(favor ?f)
	(movie (index ?f)(tags $?tag_name))
	(test (> ?*count* 0))
	=>
	(bind ?listlen (length $?tag_name))
	(loop-for-count (?cnt 1 ?listlen) do
		(bind ?curtag (nth$ ?cnt ?tag_name))
		(if (not (subsetp (create$ ?curtag) ?*tag_list*))
			then
		(bind ?*tag_list* (insert$ ?*tag_list* (+ (length ?*tag_list*) 1) ?curtag))
		(assert (tag_list (tag_name ?curtag)(tag_index ?*tag_index*)))
		(printout t ?*tag_index* "." ?curtag)
		(format_display ?curtag ?*tag_index*)
		(bind ?*tag_index* (+ ?*tag_index* 1))))
	(bind ?*count* (- ?*count* 1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deffunction insert_favor_tags (?taglist)
	(loop-for-count (?cnt 1 (length ?taglist)) do
		(bind ?cur (nth$ ?cnt ?taglist))
		(assert (tag ?cur (- 6 ?cnt)))))

(defrule continue_ask_tag
	(test (= ?*count* 0))
	?state <- (state tag_print)
    ?state_current <- (state tag)
	=>
	(retract ?state)
	(if (evenp ?*tag_index*)
		then
		(printout t "" crlf))
	(bind ?line (readline))
	(insert_favor_tags (explode$ ?line))
(printout t crlf)(printout t crlf)
(printout t "==================================================================" crlf)
(printout t "1. top10 2. display all 3. continue 4. restart 5. end" crlf)
(bind ?line (read))
(switch ?line
(case 1 then
(assert (display top10))
(assert (count 10))
(printout t crlf)
(printout t "=================================top10=============================" crlf))
(case 2 then
(assert (display all))
(assert (count 209))
(printout t crlf)
(printout t "==================================all==============================" crlf))
(case 3 then
(assert (op continue)))
(case 4 then
(retract ?state_current)
(assert (state end))
(assert (op continue)))
(case 5 then
(printout t crlf)
(printout t "==================================================================" crlf)
(printout t "            THANKY YOU FOR USING CINEMA GURU, GOODBYE!            " crlf)
(printout t "==================================================================" crlf)
(printout t crlf)
(halt))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;CAST;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftemplate cast_list
	(slot cast_name)
	(slot cast_index))

(defrule ask_cast
	(state cast)
	(exists (favor ?))
	=>
	(printout t "Please enter your 5 favorite casts by index ordered by preference, seperated by space" crlf)
	(printout t "For example: 1 2 3 4 5 (cast 1 is the most favorite)" crlf)
	(bind ?*count* ?*favor_count*)
	(assert (state cast_print)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrule insert_cast
	(state cast_print)
	(favor ?f)
	(movie (index ?f)(casts $?cast_name))
	(test (> ?*count* 0))
	=>
	(bind ?listlen (length $?cast_name))
	(loop-for-count (?cnt 1 ?listlen) do
		(bind ?curcast (nth$ ?cnt ?cast_name))
		(if (not (subsetp (create$ ?curcast) ?*cast_list*))
			then
		(bind ?*cast_list* (insert$ ?*cast_list* (+ (length ?*cast_list*) 1) ?curcast))
		(assert (cast_list (cast_name ?curcast)(cast_index ?*cast_index*)))
		(printout t ?*cast_index* "." ?curcast)
		(format_display ?curcast ?*cast_index*)
		(bind ?*cast_index* (+ ?*cast_index* 1))))
	(bind ?*count* (- ?*count* 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deffunction insert_favor_casts (?castlist)
	(loop-for-count (?cnt 1 (length ?castlist)) do
		(bind ?cur (nth$ ?cnt ?castlist))
		(assert (cast ?cur (- 6 ?cnt)))))

(defrule continue_ask_cast
	(test (= ?*count* 0))
	?state <- (state cast_print)
    ?state_current <- (state cast)
	=>
	(retract ?state)
    (assert (continue f))
	(if (evenp ?*cast_index*)
		then
		(printout t "" crlf))
	(bind ?line (readline))
	(insert_favor_casts (explode$ ?line))
(printout t crlf)(printout t crlf)
(printout t "==================================================================" crlf)
(printout t "1. top10 2. display all 3. restart 4. end" crlf)
(bind ?line (read))
(switch ?line
(case 1 then
(assert (display top10))
(assert (count 10))
(printout t crlf)
(printout t "=================================top10=============================" crlf))
(case 2 then
(assert (display all))
(assert (count 209))
(printout t crlf)
(printout t "==================================all==============================" crlf))
(case 3 then
(retract ?state_current)
(assert (state end))
(assert (op continue)))
(case 4 then
(printout t crlf)
(printout t "==================================================================" crlf)
(printout t "            THANKY YOU FOR USING CINEMA GURU, GOODBYE!            " crlf)
(printout t "==================================================================" crlf)
(printout t crlf)
(halt))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;compute score and display;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;define global
(defglobal ?*tagname1* = ""
?*tagname2* = ""
?*tagname3* = ""
?*tagname4* = ""
?*tagname5* = ""
?*tagscore1* = 0
?*tagscore2* = 0
?*tagscore3* = 0
?*tagscore4* = 0
?*tagscore5* = 0)


;;compute category score
(defrule computecategoryscore
(declare (salience -10))
(display ?x)
(category ?cat1 ?score1)
(category ?cat2 ?score2)
(category ?cat3 ?score3)
(test (neq ?cat1 ?cat2 ?cat3))
?movie<-(movie (index ?i) (categories $?cats) (is_released no) (rate ?rate) (score ?originalscore))
?moviecomputed<-(moviecomputed (index ?i) (iscatcomputed no))
=>
(bind ?total (* ?rate 0.3))
(if(member$ ?cat1 $?cats)then(bind ?total (+ ?total ?score1)))
(if(member$ ?cat2 $?cats)then(bind ?total (+ ?total ?score2)))
(if(member$ ?cat3 $?cats)then(bind ?total (+ ?total ?score3)))
(modify ?movie (score (+ ?originalscore ?total)))
(modify ?moviecomputed (iscatcomputed yes)))

;; get tag name by index
(defrule gettagname
(declare (salience -10))
(display ?x)
(tag ?index1 1)
(tag ?index2 2)
(tag ?index3 3)
(tag ?index4 4)
(tag ?index5 5)
(tag_list (tag_name ?name1) (tag_index ?index1))
(tag_list (tag_name ?name2) (tag_index ?index2))
(tag_list (tag_name ?name3) (tag_index ?index3))
(tag_list (tag_name ?name4) (tag_index ?index4))
(tag_list (tag_name ?name5) (tag_index ?index5))
=>
(bind ?*tagname1* ?name1)
(bind ?*tagname2* ?name2)
(bind ?*tagname3* ?name3)
(bind ?*tagname4* ?name4)
(bind ?*tagname5* ?name5)
(bind ?*tagscore1* 1)
(bind ?*tagscore2* 2)
(bind ?*tagscore3* 3)
(bind ?*tagscore4* 4)
(bind ?*tagscore5* 5))

;; compute tag score
(defrule computetagscore
(declare (salience -20))
(display ?x)
?movie<-(movie (index ?i) (tags $?tags) (is_released no) (score ?originalscore))
?moviecomputed<-(moviecomputed (index ?i) (istagcomputed no))
=>
(bind ?total ?originalscore)
(if(member$ ?*tagname1* $?tags)then(bind ?total (+ ?total ?*tagscore1*)))
(if(member$ ?*tagname2* $?tags)then(bind ?total (+ ?total ?*tagscore2*)))
(if(member$ ?*tagname3* $?tags)then(bind ?total (+ ?total ?*tagscore3*)))
(if(member$ ?*tagname4* $?tags)then(bind ?total (+ ?total ?*tagscore4*)))
(if(member$ ?*tagname5* $?tags)then(bind ?total (+ ?total ?*tagscore5*)))
(modify ?movie (score ?total))
(modify ?moviecomputed (istagcomputed yes)))

(defrule displaytopN
(declare (salience -30))
?count <-(count ?counter)
(test (> ?counter 0))
(display top10)
?top<-(movie (index ?i) (title ?title) (categories $?cat) (tags $?tag) (casts $?cast) (date ?date) (is_released no) (rate ?rate) (score ?score))
(not (movie (is_released no) (score ?score1&:(> ?score1 ?score))))
=>
(printout t ?title " " ?score crlf)
(retract ?top)
(retract ?count)
(assert (count (- ?counter 1)))
(assert (movie_temp (index ?i) (title ?title) (categories $?cat) (tags $?tag) (casts $?cast) (date ?date) (is_released no) (rate ?rate) (score ?score))))

(defrule putback
(declare (salience -40))
?movie_temp<-(movie_temp (index ?i) (title ?title) (categories $?cat) (tags $?tag) (casts $?cast) (date ?date) (is_released no) (rate ?rate) (score ?score))
=>
(assert (movie (index ?i) (title ?title) (categories $?cat) (tags $?tag) (casts $?cast) (date ?date) (is_released no) (rate ?rate) (score ?score)))
(retract ?movie_temp))

(defrule displayall
(declare (salience -30))
(display all)
?top<-(movie (index ?i) (title ?title) (categories $?cat) (tags $?tag) (casts $?cast) (date ?date) (is_released no) (rate ?rate) (score ?score))
(not (movie (is_released no) (score ?score1&:(> ?score1 ?score))))
?count <-(count ?counter)
(test (> ?counter 0))
=>
(printout t ?title " " ?score crlf)
(retract ?top)
(assert (movie_temp (index ?i) (title ?title) (categories $?cat) (tags $?tag) (casts $?cast) (date ?date) (is_released no) (rate ?rate) (score ?score)))
(retract ?count)
(assert (count (- ?counter 1))))


(defrule post_display1
(declare (salience -40))
?display <-(display ?x)
?state_current <- (state ?s)
(not (continue f))
=>
(printout t "==================================================================" crlf)
(printout t "1. top10 2. display all 3. continue 4. restart 5. end" crlf)
(retract ?display)
(bind ?line (read))
(switch ?line
(case 1 then
(assert (display top10))
(assert (count 10))
(printout t crlf)
(printout t "=================================top10=============================" crlf))
(case 2 then
(assert (display all))
(assert (count 192))
(printout t crlf)
(printout t "==================================all==============================" crlf))
(case 3 then
(assert (op continue)))
(case 4 then
(retract ?state_current)
(assert (state end))
(assert (op continue)))
(case 5 then
(printout t crlf)
(printout t "==================================================================" crlf)
(printout t "            THANKY YOU FOR USING CINEMA GURU, GOODBYE!            " crlf)
(printout t "==================================================================" crlf)
(printout t crlf)
(halt))))

(defrule post_display2
(declare (salience -50))
?display <-(display ?x)
?state_current <- (state ?s)
(continue f)
=>
(printout t "==================================================================" crlf)
(printout t "1. top10 2. display all 3. restart 4. end" crlf)
(retract ?display)
(bind ?line (read))
(switch ?line
(case 1 then
(assert (display top10))
(assert (count 10))
(printout t crlf)
(printout t "=================================top10=============================" crlf))
(case 2 then
(assert (display all))
(assert (count 192))
(printout t crlf)
(printout t "==================================all==============================" crlf))
(case 3 then
(retract ?state_current)
(assert (state end))
(assert (op continue)))
(case 4 then
(printout t crlf)
(printout t "==================================================================" crlf)
(printout t "            THANKY YOU FOR USING CINEMA GURU, GOODBYE!            " crlf)
(printout t "==================================================================" crlf)
(printout t crlf)
(halt))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;category;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftemplate categoryRank
(slot cat)
(slot times (default 0))
(slot average_score(default 0))
)


(deffacts category_list
(categoryList))


(deffacts loop_time1
(loop 0))


(deffacts loop_time2
(loop1 0))


(deffunction insert_category_ranking (?ratings $?category)
(if (> (length $?category) 0)
then
(assert (categoryScore (first$ $?category) ?ratings))
(insert_category_ranking ?ratings (rest$ $?category))))


(deffunction greater-predicate (?value1 ?value2)
(> ?value1 ?value2))


(deffunction equal-predicate (?value1 ?value2)
(= ?value1 ?value2))


(defrule compute_category_rank
(declare (salience 40))
(state category)
(favor ?index)
(movie (index ?index) (categories $?cat) (rate ?rate))
=>
(insert_category_ranking ?rate $?cat))


(defrule create_category_rank
(declare (salience 35))
(state category)
?temp <- (categoryScore ?cat ?score)
?catList <- (categoryList $?list)
(test(eq (subsetp (create$ ?cat) $?list) FALSE))
=>
(retract ?temp)
(retract ?catList)
(assert (categoryRank (cat ?cat) (times 1) (average_score ?score)))
(assert (categoryList (create$ $?list ?cat))))

(defrule update_category_rank
(declare (salience 30))
(state category)
?temp <- (categoryScore ?cat ?score)
?catList <- (categoryList $?list)
(test(eq (subsetp (create$ ?cat) $?list) TRUE))
?update <- (categoryRank (cat ?cat) (times ?time) (average_score ?ori_score))
=>
(modify ?update (times (+ ?time 1)) (average_score (/ (+ (* ?ori_score ?time) ?score) (+ ?time 1))))
(retract ?temp))


(defrule find-max-value
(declare (salience 25))
(state category)
?loop_time <- (loop ?loop)
(test (< ?loop 3))
(state category)
?cate1 <-(categoryRank (cat ?cat1) (times ?time1) (average_score ?score1))
(not (categoryRank (times ?time2&:(greater-predicate ?time2 ?time1)) (average_score ?score2)))
(not (categoryRank (times ?time2&:(equal-predicate ?time2 ?time1)) (average_score ?score2&:(greater-predicate ?score2 ?score1))))

=>
;;(printout t "categroy " ?cat1 " is the maximum" crlf)
(assert (loop (+ ?loop 1)))
(assert (top_3 ?cat1))
(retract ?loop_time)
(retract ?cate1))


(defrule ask_category
(declare (salience 20))
(state category)
?top <- (top_3 ?cat)
?high <- (loop1 ?loop)
(test (< ?loop 3))
=>
(printout t "To what extent do you like " ?cat " movies" crlf)
(printout t "Please input a number between 1 - 5, 1 is less significant, 5 is most significant" crlf)
(bind ?line (readline))
(retract ?top)
(retract ?high)
(assert (category ?cat (explode$ ?line)))
(assert (loop1 (+ ?loop 1))))


(defrule destruct_temporary_fact
(declare (salience 15))
(state category)
?f1 <-(categoryRank (cat ?cat) (times ?time) (average_score ?score))
=>
(retract ?f1))


(defrule category_display
(declare (salience 5))
?state_current <-(state category)
=>
(printout t crlf)(printout t crlf)
(printout t "==================================================================" crlf)
(printout t "1. top10 2. display all 3. continue 4. restart 5. end" crlf)
(bind ?line (read))
(switch ?line
(case 1 then
(assert (display top10))
(assert (count 10))
(printout t crlf)
(printout t "=================================top10=============================" crlf))
(case 2 then
(assert (display all))
(assert (count 209))
(printout t crlf)
(printout t "==================================all==============================" crlf))
(case 3 then
(assert (op continue)))
(case 4 then
(retract ?state_current)
(assert (state end))
(assert (op continue)))
(case 5 then
(printout t crlf)
(printout t "==================================================================" crlf)
(printout t "            THANKY YOU FOR USING CINEMA GURU, GOODBYE!            " crlf)
(printout t "==================================================================" crlf)
(printout t crlf)
(halt))))


(defrule destruct_control_variable
(declare (salience 10))
(state category)
?f1<-(loop1 ?loop1&:(= ?loop1 3))
?f2<-(loop  ?loop&:(= ?loop 3))
?f3<-(categoryList $?list)
=>
(retract ?f1)
(retract ?f2)
(retract ?f3))