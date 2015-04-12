(deftemplate movie
(slot index)
(slot title)
(multislot categories)
(multislot tags)
(multislot casts)
(slot date)
(slot is_released (allowed-symbols yes no))
(slot rate (default 0))
(slot score (default 0))
)

(deftemplate movie_temp
(slot index)
(slot title)
(multislot categories)
(multislot tags)
(multislot casts)
(slot date)
(slot is_released (allowed-symbols yes no))
(slot rate (default 0))
(slot score (default 0))
)

(defglobal
?*x* = 1
?*ct* = 0
?*list* = (create$))

(deffacts movie2014
(movie (index 1)(title "Planes: Fire &amp; Rescue")(categories Animation Adventure Comedy)(tags "fire" "rescue" "battle" "forest" "firefighting")(casts "Dane Cook" "Ed Harris" "Julie Bowen")(date "4 September 2014")(is_released yes)(rate 5.9)(score 0))
(movie (index 2)(title "A Walk Among the Tombstones")(categories Crime Drama Mystery)(tags "drug dealer" "private investigator" "brooklyn new york city" "violence" "murder of wife")(casts "Liam Neeson" "Dan Stevens" "David Harbour")(date "18 September 2014")(is_released yes)(rate 6.6)(score 0))
(movie (index 3)(title "Lone Survivor")(categories Action Biography Drama)(tags "navy seal" "pashtunwali" "commando unit" "afghanistan" "fight")(casts "Peter Berg" "Mark Wahlberg" "Taylor Kitsch")(date "13 February 2014")(is_released yes)(rate 7.6)(score 0))
(movie (index 4)(title "Ouija")(categories Horror)(tags "ouija" "spirit" "ouija board" "swimming pool" "drowning")(casts "Olivia Cooke" "Ana Coto" "Daren Kagasoff")(date "23 October 2014")(is_released yes)(rate 4.4)(score 0))
(movie (index 5)(title "The Grand Budapest Hotel")(categories Adventure Comedy Drama)(tags "first person narration" "hotel" "painting" "mentor protege relationship" "pastry shop")(casts "Ralph Fiennes" "F. Murray Abraham" "Mathieu Amalric")(date "20 March 2014")(is_released yes)(rate 8.1)(score 0))
(movie (index 6)(title "Neighbors")(categories Comedy)(tags "lesbian kiss" "sex in a chair" "fraternity" "friendship" "one word title")(casts "Seth Rogen" "Rose Byrne" "Zac Efron")(date "8 May 2014")(is_released no)(rate 6.4)(score 0))
(movie (index 7)(title "Pompeii")(categories Action Adventure Drama)(tags "pompeii" "roman" "senator" "slave" "arena")(casts "Kit Harington" "Emily Browning" "Kiefer Sutherland")(date "20 February 2014")(is_released no)(rate 5.6)(score 0))
(movie (index 8)(title "Into the Storm")(categories Action Thriller)(tags "storm" "tornado" "danger" "graduation" "storm chaser")(casts "Richard Armitage" "Sarah Wayne Callies" "Matt Walsh")(date "7 August 2014")(is_released no)(rate 5.9)(score 0))
(movie (index 9)(title "Xi yan")(categories Comedy Drama Romance)(tags "marriage of convenience" "gay couple" "gay son" "marriage pressure" "asian man white man relationship")(casts "Ang Lee" "Neil Peng" "Winston Chao")(date "4 August 1993")(is_released no)(rate 7.7)(score 0))
(movie (index 10)(title "Blended")(categories Comedy Romance)(tags "children" "date" "blind date" "daughter" "son")(casts "Ivan Menchell" "Adam Sandler" "Drew Barrymore")(date "5 June 2014")(is_released no)(rate 6.5)(score 0))
)

(deffacts movie2015
(movie (index 11)(title "Foxcatcher")(categories Biography Drama Sport)(tags "domination" "olympics" "training" "brother" "wrestler")(casts "E. Max Frye" "Steve Carell" "Channing Tatum")(date "16 January 2015")(is_released no)(rate 7.2)(score 0))
(movie (index 12)(title "Paul Blart: Mall Cop 2")(categories Action Comedy)(tags "sequel" "character name in title")(casts "Kevin James" "Nick Bakay" "Kevin James")(date "7 May 2015")(is_released no)(rate 0)(score 0))
(movie (index 13)(title "Unlucky Plaza")(categories Comedy Crime Drama)(tags )(casts "Jeffrey Quizon" "Adrian Pang" "Judee Tan")(date "16 April 2015")(is_released no)(rate 6.9)(score 0))
(movie (index 14)(title "Avengers: Age of Ultron")(categories Action Adventure Fantasy)(tags "marvel cinematic universe" "superhero team" "based on comic book" "superhero" "captain america")(casts "Joss Whedon" "Robert Downey Jr." "Chris Evans")(date "23 April 2015")(is_released no)(rate 0)(score 0))
(movie (index 15)(title "Hot Pursuit")(categories Action Comedy)(tags )(casts "David Feeney" "Reese Witherspoon" "Sofía Vergara")(date "7 May 2015")(is_released no)(rate 0)(score 0))
(movie (index 16)(title "Pitch Perfect 2")(categories Comedy Music)(tags "sequel" "number in title")(casts "Anna Kendrick" "Rebel Wilson" "Hailee Steinfeld")(date "14 May 2015")(is_released no)(rate 0)(score 0))
(movie (index 17)(title "Pixels")(categories Action Comedy Sci-Fi)(tags "based on short film" "new york city" "one word title")(casts "Adam Sandler" "Kevin James" "Michelle Monaghan")(date "3 September 2015")(is_released no)(rate 0)(score 0))
(movie (index 18)(title "Mad Max: Fury Road")(categories Action Adventure Thriller)(tags "inbetwequel" "peak oil" "dark future" "future" "post apocalypse")(casts "George Miller" "Brendan McCarthy" "Tom Hardy")(date "14 May 2015")(is_released no)(rate 0)(score 0))
(movie (index 19)(title "Jurassic World")(categories Action Adventure Sci-Fi)(tags "theme park" "dinosaur" "t rex" "velociraptor" "genetic engineering")(casts "Chris Pratt" "Judy Greer" "Ty Simpkins")(date "11 June 2015")(is_released no)(rate 0)(score 0))
(movie (index 20)(title "Minions")(categories Animation Comedy Family)(tags )(casts "Kyle Balda" "Chris Renaud" "Pierre Coffin")(date "18 June 2015")(is_released no)(rate 0)(score 0))
)

(deffacts default_fact
(state movie))

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
=>
(printout t ?title " " ?score crlf)
(retract ?top)
(assert (movie_temp (index ?i) (title ?title) (categories $?cat) (tags $?tag) (casts $?cast) (date ?date) (is_released no) (rate ?rate) (score ?score)))
(retract ?count)
(assert (count (+ ?counter 1))))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;category;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ; Yunjin
(deftemplate categoryRank
(slot cat)
(slot times (default 0))
(slot average_score(default 0))
)

(deffacts category_list
(categoryList))

(deffacts loop_time
(loop 0))

(deffacts loop_time1
(loop1 0))

(deffunction insert_category_ranking (?ratings $?category)
(if (> (length $?category) 0)
then
(assert (categoryScore (first$ $?category) ?ratings))
(insert_category_ranking ?ratings (rest$ $?category))))



(defrule compute_category_rank
(declare (salience 35))
(state category)
(favor ?index)
(movie (index ?index) (categories $?cat) (rate ?rate))
=>
(insert_category_ranking ?rate $?cat))


(defrule create_category_rank
(declare (salience 30))
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
(declare (salience 25))
(state category)
?temp <- (categoryScore ?cat ?score)
?catList <- (categoryList $?list)
(test(eq (subsetp (create$ ?cat) $?list) TRUE))
?update <- (categoryRank (cat ?cat) (times ?time) (average_score ?ori_score))
=>
(modify ?update (times (+ ?time 1)) (average_score (/ (+ (* ?ori_score ?time) ?score) (+ ?time 1))))
(retract ?temp))



(deffunction greater-predicate (?value1 ?value2)
(> ?value1 ?value2))


(deffunction equal-predicate (?value1 ?value2)
(= ?value1 ?value2))


(defrule find-max-value
(declare (salience 20))
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
(declare (salience 15))
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
(declare (salience 10))
(state category)
?f1 <-(categoryRank (cat ?cat) (times ?time) (average_score ?score))
=>
(retract ?f1))


(defrule category_display
(declare (salience 5))
?state <-(state category)
=>
(printout t "1. top10 2. all 3. continue 4. end" crlf)
(bind ?line (read))
(switch ?line
(case 1 then
(assert (display top10))
(assert (count 10)))
(case 2 then
(assert (display all))
(assert (count 0)))
(case 3 then
(assert (op continue)))
(case 4 then
(retract ?state)
(assert (state end))
(assert (op continue)))))


(defrule destruct_control_variable
(declare (salience 0))
(state category)
?f1<-(loop1 ?loop1&:(= ?loop1 3))
?f2<-(loop  ?loop&:(= ?loop 3))
?f3<-(categoryList $?list)
=>
(retract ?f1)
(retract ?f2)
(retract ?f3))