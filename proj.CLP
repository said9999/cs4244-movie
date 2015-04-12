
(defrule map
?state <- (state ?s)
?op <- (op continue)
=>
(switch ?s
(case movie then
(retract ?state)
(assert (state category)))
(case category then
(retract ?state)
(assert (state tag)))
(case tag then
(retract ?state)
(assert (state cast)))
(case cast then
(retract ?state)
(assert (state end)))
(case end then
(retract ?state)
(reset)))
(retract ?op))

(deffunction insert_favor_movies (?movielist)
(if (> (length ?movielist) 0)
then
(assert (favor (first$ ?movielist)))
(bind ?*ct* (+ ?*ct* 1)) ;;modified by chenxi
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

(deftemplate temp
(slot count))

(defrule ask_tag
(state tag)
(exists (favor ?))
=>
(printout t "Please enter your 5 favorite tags" crlf)
(assert (state tag_print)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;MODIFIED

(deffunction format_tag_display (?tag ?index)
(loop-for-count (?cnt 1 (- 30 (str-length ?tag))) do
(printout t " "))
(if (= 0 (mod ?index 2))
then
(printout t "" crlf)))

(defrule insert_tag
(state tag_print)
(favor ?f)
(movie (index ?f)(tags $?tag_name))
(test (> ?*ct* 0))
=>
(bind ?listlen (length $?tag_name))
(loop-for-count (?cnt 1 ?listlen) do
(bind ?curtag (nth$ ?cnt ?tag_name))
(if (not (subsetp (create$ ?curtag) ?*list*))
then
(bind ?*list* (insert$ ?*list* (+ (length ?*list*) 1) ?curtag))
(assert (tag_list (tag_name ?curtag)(tag_index ?*x*)))
(printout t ?*x* "." ?curtag)
(format_tag_display ?curtag ?*x*)
(bind ?*x* (+ ?*x* 1))))
(bind ?*ct* (+ ?*ct* 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deffunction insert_favor_tags (?taglist)
(loop-for-count (?cnt 1 (length ?taglist)) do
(bind ?cur (nth$ ?cnt ?taglist))
(assert (tag ?cur (- 6 ?cnt)))))

(defrule continue_ask_tag
(test (= ?*ct* 0))
?state <- (state tag_print)
=>
(retract ?state)
(bind ?line (readline))
(insert_favor_tags (explode$ ?line))
(printout t "1. top10 2. all 3. continue 4. end" crlf)
(bind ?action (readline))
(switch ?action
(case 1 then
(assert (display top10)))
(case 2 then
(assert (display all)))
(case 3 then
(assert (op continue)))
(case 4 then
(assert (restart)))))


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
?*tagscore5* = 0
?*scorelist* = (create$)
?*top1score* = 0
?*top2score* = 0
?*top3score* = 0
?*top4score* = 0
?*top5score* = 0
?*top6score* = 0
?*top7score* = 0
?*top8score* = 0
?*top9score* = 0
?*top10score* = 0
?*top50score* = 0)


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
(bind ?total ?rate)
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

;; sort score
(defrule createscorelist
(declare (salience -30))
(display ?x)
?move<-(movie (index ?i) (is_released no) (score ?score))
?moviecomputed<-(moviecomputed (index ?i) (iscatcomputed yes))
=>
(bind ?*scorelist* (insert$ ?*scorelist* (+ (length$ ?*scorelist*) 1) ?score))
(printout t "FUCK")
(printout t ?*scorelist*))


(deffunction sortscore()
(bind ?*scorelist* (sort < ?*scorelist*))
(bind ?*top1score* (nth$ 1 ?*scorelist*))
(bind ?*top2score* (nth$ 2 ?*scorelist*))
(bind ?*top3score* (nth$ 3 ?*scorelist*))
(bind ?*top4score* (nth$ 4 ?*scorelist*))
(bind ?*top5score* (nth$ 5 ?*scorelist*))
(bind ?*top6score* (nth$ 6 ?*scorelist*))
(bind ?*top7score* (nth$ 7 ?*scorelist*))
(bind ?*top8score* (nth$ 8 ?*scorelist*))
(bind ?*top9score* (nth$ 9 ?*scorelist*))
(bind ?*top10score* (nth$ 10 ?*scorelist*))
(bind ?*top50score* (nth$ 50 ?*scorelist*))
(bind ?*scorelist* (create$))
0)


(defrule sortscore
(declare (salience -40))
(display ?x)
=>
(sortscore))



;;(defrule displaytop1
;;(declare (salience -50))
;;(display top10)
;;(movie (is_released no) (score ?score) (title ?title))
;;=>
;;(if(= ?score ?*top1score*)then(printout t ?title " " ?score crlf)));;

;;(defrule displaytop2
;;(declare (salience -50))
;;(display top10)
;;(movie (is_released no) (score ?score) (title ?title))
;;=>
;;(if(= ?score ?*top2score*)then(printout t ?title " " ?score crlf)));;

;;(defrule displaytop3
;;(declare (salience -50))
;;(display top10)
;;(movie (is_released no) (score ?score) (title ?title))
;;=>
;;(if(= ?score ?*top3score*)then(printout t ?title " " ?score crlf)));;

;;(defrule displaytop4
;;(declare (salience -50))
;;(display top10)
;;(movie (is_released no) (score ?score) (title ?title))
;;=>
;;(if(= ?score ?*top4score*)then(printout t ?title " " ?score crlf)));;

;;(defrule displaytop5
;;(declare (salience -50))
;;(display top10)
;;(movie (is_released no) (score ?score) (title ?title))
;;=>
;;(if(= ?score ?*top5score*)then(printout t ?title " " ?score crlf)));;

;;(defrule displaytop6
;;(declare (salience -50))
;;(display top10)
;;(movie (is_released no) (score ?score) (title ?title))
;;=>
;;(if(= ?score ?*top6score*)then(printout t ?title " " ?score crlf)));;

;;(defrule displaytop7
;;(declare (salience -50))
;;(display top10)
;;(movie (is_released no) (score ?score) (title ?title))
;;=>
;;(if(= ?score ?*top7score*)then(printout t ?title " " ?score crlf)));;

;;(defrule displaytop8
;;(declare (salience -50))
;;(display top10)
;;(movie (is_released no) (score ?score) (title ?title))
;;=>
;;(if(= ?score ?*top8score*)then(printout t ?title " " ?score crlf)));;

;;(defrule displaytop9
;;(declare (salience -50))
;;(display top10)
;;(movie (is_released no) (score ?score) (title ?title))
;;=>
;;(if(= ?score ?*top9score*)then(printout t ?title " " ?score crlf)))

(defrule displaytop10
(declare (salience -50))
(display top10)
(movie (is_released no) (score ?score) (title ?title))
=>
(if(>= ?score ?*top10score*)then(printout t ?title " " ?score crlf)))

(defrule displaytop50
(declare (salience -50))
(display top50)
(movie (is_released no) (score ?score) (title ?title))
(test (>= ?score ?*top50score*))
=>
(printout t ?title crlf))

(defrule displayall
(declare (salience -50))
(display all)
(movie (title ?title) (is_released no))
=>
(printout t ?title crlf))






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
(declare (salience 10))
?state <-(state category)
=>
(printout t "1. top10 2. all 3. continue 4. end" crlf)
(bind ?line (read))
(switch ?line
(case 1 then
(assert (display top10)))
(case 2 then
(assert (display all)))
(case 3 then
(assert (op continue)))
(case 4 then
(retract ?state)
(assert (state end))
(assert (op continue)))))


(defrule destruct_control_variable
(declare (salience 5))
(state category)
?f1<-(loop1 ?loop1&:(= ?loop1 3))
?f2<-(loop  ?loop&:(= ?loop 3))
?f3<-(categoryList $?list)
=>
(retract ?f1)
(retract ?f2)
(retract ?f3))