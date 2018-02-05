;;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-
;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig

;;;; File grammar.lisp:  The grammar of English given in Chapter 21.

(requires "unifgram" "lexicon")

(rule (NP ?agr (common ?) -wh ?x ?g1 ?g1 (the ?x (name ?name ?x))) ==>
  (name ?agr ?name))

(rule (NP ?agr ?case ?wh ?x ?g1 ?g1 ?sem) ==>
  (pronoun ?agr ?case ?wh ?x ?sem))

(rule (NP (- - - +) ?case -wh ?x ?g1 ?g2 (group ?x ?sem)) ==>
  (:ex "dogs") ; Plural nouns don't need a determiner
  (NP2 (- - - +) ?case ?x ?g1 ?g2 ?sem))

(rule (NP ?agr (common ?) ?wh ?x ?g1 ?g2 ?sem) ==>
  (:ex "Every man" "The dogs on the beach")
  (Det ?agr ?wh ?x ?restriction ?sem)
  (NP2 ?agr (common ?) ?x ?g1 ?g2 ?restriction))

(rule (NP ?agr ?case ?wh ?x (gap (NP ?agr ?case ?x)) (gap nil) t)
  ==> ;; Gapped NP
  )

(rule (NP2 ?agr (common ?) ?x ?g1 ?g2 :sem) ==>
  (modifiers pre noun ?agr () ?x (gap nil) (gap nil) ?pre)
  (noun ?agr ?slots ?x ?noun)
  (modifiers post noun ?agr ?slots ?x ?g1 ?g2 ?post))

(rule (modifiers ?pre/post ?cat ?info (?slot . ?slots) ?h
                 ?g1 ?g3 :sem) ==>
  (complement ?cat ?info ?slot ?h ?g1 ?g2 ?mod)
  (modifiers ?pre/post ?cat ?info ?slots ?h ?g2 ?g3 ?mods))

(rule (modifiers ?pre/post ?cat ?info ((? (?) ?) . ?slots) ?h
                 ?g1 ?g2 ?mods) ==>
  (modifiers ?pre/post ?cat ?info ?slots ?h ?g1 ?g2 ?mods))

(rule (modifiers ?pre/post ?cat ?info ?slots ?h ?g1 ?g3 :sem) ==>
  (adjunct ?pre/post ?cat ?info ?h ?g1 ?g2 ?adjunct)
  (modifiers ?pre/post ?cat ?info ?slots ?h ?g2 ?g3 ?mods))

(rule (modifiers ? ? ? () ? ?g1 ?g1 t) ==> )

(rule (adjunct pre noun ?info ?x ?gap ?gap ?sem) ==>
  (adj ?x ?sem))

(rule (adjunct pre noun ?info ?h ?gap ?gap :sem) ==>
  (:sem (noun-noun ?h ?x))
  (noun ?agr () ?x ?sem))

(rule (adjunct post ?cat ?info ?x ?g1 ?g2 ?sem) ==>
  (PP ?prep ?prep ?wh ?np ?x ?g1 ?g2 ?sem))

(rule (PP ?prep ?role ?wh ?np ?x ?g1 ?g2 :sem) ==>
  (prep ?prep t)
  (:sem (?role ?x ?np))
  (NP ?agr (common obj) ?wh ?np ?g1 ?g2 ?np-sem))

(rule (PP ?prep ?role ?wh ?np ?x
          (gap (PP ?prep ?role ?np ?x)) (gap nil) t) ==> )

(rule (adjunct post noun ?agr ?x ?gap ?gap ?sem) ==>
  (:ex (the man) "visiting me" (the man) "visited by me")
  (:test (member ?infl (-ing passive)))
  (clause ?infl ?x ? ?v (gap (NP ?agr ? ?x)) (gap nil) ?sem))

(rule (adjunct post noun ?agr ?x ?gap ?gap ?sem) ==>
  (rel-clause ?agr ?x ?sem))

(rule (rel-clause ?agr ?x :sem) ==>
  (:ex (the man) "that she liked" "that liked her"
       "that I know Lee liked")
  (opt-rel-pronoun ?case ?x ?int-subj ?rel-sem) 
  (clause (finite ? ?) ? ?int-subj ?v
          (gap (NP ?agr ?case ?x)) (gap nil) ?clause-sem))

(rule (opt-rel-pronoun ?case ?x ?int-subj (?type ?x)) ==>
  (:word ?rel-pro)
  (:test (word ?rel-pro rel-pro ?case ?type)))

(rule (opt-rel-pronoun (common obj) ?x int-subj t) ==> )

(rule (Det ?agr ?wh ?x ?restriction (?art ?x ?restriction)) ==>
  (:ex "the" "every")
  (art ?agr ?art)
  (:test (if (= ?art wh) (= ?wh +wh) (= ?wh -wh))))

(rule (Det ?agr ?wh ?x ?r (the ?x ?restriction)) ==>
  (:ex "his" "her")
  (pronoun ?agr gen ?wh ?y ?sem)
  (:test (and* ((genitive ?y ?x) ?sem ?r) ?restriction)))

(rule (Det ?agr -wh ?x ?r ((number ?n) ?x ?r)) ==>
  (:ex "three")
  (cardinal ?n ?agr))

(rule (VP ?infl ?x ?subject-slot ?v ?g1 ?g2 :sem) ==>
  (:ex "sleeps" "quickly give the dog a bone")
  (modifiers pre verb ? () ?v (gap nil) (gap nil) ?pre-sem)
  (:sem (?role ?x ?v)) (:test (= ?subject-slot (?role 1 ?)))
  (verb ?verb ?infl (?subject-slot . ?slots) ?v ?v-sem) 
  (modifiers post verb ? ?slots ?v ?g1 ?g2 ?mod-sem))

(rule (VP ?infl ?x ?subject-slot ?v ?g1 ?g2 :sem) ==>
  (:ex "is sleeping" "would have given a bone to the dog."
       "did not sleep" "was given a bone by this old man")
  ;; An aux verb, followed by a VP
  (aux ?infl ?needs-infl ?v ?aux)
  (modifiers post aux ? () ?v (gap nil) (gap nil) ?mod)
  (VP ?needs-infl ?x ?subject-slot ?v ?g1 ?g2 ?vp))

(rule (adjunct post aux ? ?v ?gap ?gap (not ?v)) ==>
  (:word not))

(rule (adjunct ?pre/post verb ?info ?v ?g1 ?g2 ?sem) ==>
  (advp ?wh ?v ?g1 ?g2 ?sem))

(rule (advp ?wh ?v ?gap ?gap ?sem) ==>
  (adverb ?wh ?v ?sem))

(rule (advp ?wh ?v (gap (advp ?v)) (gap nil) t) ==> )

(rule (clause ?infl ?x ?int-subj ?v ?gap1 ?gap3 :sem) ==>
  (subject ?agr ?x ?subj-slot ?int-subj ?gap1 ?gap2 ?subj-sem)
  (VP ?infl ?x ?subj-slot ?v ?gap2 ?gap3 ?pred-sem)
  (:test (subj-pred-agree ?agr ?infl)))

(rule (subject ?agree ?x ?subj-slot ext-subj
               (gap ?subj) (gap nil) t) ==>
  ;; Externally realized subject (the normal case for S)
  (:test (slot-constituent ?subj-slot ?subj ?x ?)
         (if (= ?subj (NP ?agr ?case ?x))
             (= ?agree ?agr)
             (= ?agree (- - + -))))) ;Non-NP subjects are 3sing

(rule (subject ?agr ?x (?role 1 (NP ?x)) int-subj ?gap ?gap ?sem)
  ==> 
  (NP ?agr (common nom) ?wh ?x (gap nil) (gap nil) ?sem))

(<- (subj-pred-agree ?agr (finite ?agr ?)))
(<- (subj-pred-agree ? ?infl) (atom ?infl))

(rule (S ?s :sem) ==>
  (:ex "Kim likes Lee" "Lee, I like _" "In god, we trust _"
       "Who likes Lee?" "Kim likes who?")
  (XP ?kind ?constituent ?wh ?x (gap nil) (gap nil) ?topic-sem)
  (clause (finite ? ?) ?x ? ?s (gap ?constituent) (gap nil) ?sem))

(rule (S ?s :sem) ==>
  ;; Commands have implied second-person subject
  (:ex "Give the dog a bone.")
  (:sem (command ?s))
  (:sem (listener ?x))
  (clause nonfinite ?x ext-subj ?s
          (gap (NP ? ? ?x)) (gap nil) ?sem))

(rule (S ?s (yes-no ?s ?sem)) ==>
  (:ex "Does Kim like Lee?" "Is he a doctor?")
  (aux-inv-S nil ?s ?sem))

(rule (S ?s :sem) ==>
  (:ex "Who does Kim like _?" "To whom did he give it _?"
       "What dog does Kim like _?")
  (XP ?slot ?constituent +wh ?x (gap nil) (gap nil) ?subj-sem)
  (aux-inv-S ?constituent ?s ?sem))

(rule (aux-inv-S ?constituent ?v :sem) ==>
  (:ex "Does Kim like Lee?" (who) "would Kim have liked")
  (aux (finite ?agr ?tense) ?needs-infl ?v ?aux-sem)
  (modifiers post aux ? () ?v (gap nil) (gap nil) ?mod)
  (clause ?needs-infl ?x int-subj ?v (gap ?constituent) (gap nil)
          ?clause-sem))

(rule (aux-inv-S ?ext ?v :sem) ==>
  (:ex "Is he a doctor?") 
  (verb ?be (finite ?agr ?) ((?role ?n ?xp) . ?slots) ?v ?sem)
  (:test (word ?be be))
  (subject ?agr ?x (?role ?n ?xp) int-subj
           (gap nil) (gap nil) ?subj-sem)
  (:sem (?role ?v ?x))
  (modifiers post verb ? ?slots ?v (gap ?ext) (gap nil) ?mod-sem))

(<- (slot-constituent (?role ?n (NP ?x))
                      (NP ?agr ?case ?x) ?x ?h))
(<- (slot-constituent (?role ?n (clause ?word ?infl))
                      (clause ?word ?infl ?v) ?v ?h))
(<- (slot-constituent (?role ?n (PP ?prep ?np))
                      (PP ?prep ?role ?np ?h) ?np ?h))
(<- (slot-constituent (?role ?n it)            (it ? ? ?x) ?x ?))
(<- (slot-constituent (manner 3 (advp ?x))     (advp ?v) ? ?v))
(<- (slot-constituent (?role ?n (VP ?infl ?x)) *** ? ?))
(<- (slot-constituent (?role ?n (Adj ?x))      *** ?x ?))
(<- (slot-constituent (?role ?n (P ?particle)) *** ? ?))

(rule (complement ?cat ?info (?role ?n ?xp) ?h ?gap1 ?gap2 :sem) 
  ==>
  ;; A complement is anything expected by a slot
  (:sem (?role ?h ?x))
  (:test (slot-constituent (?role ?n ?xp) ?constituent ?x ?h))
  (XP ?xp ?constituent ?wh ?x ?gap1 ?gap2 ?sem))

(rule (XP (PP ?prep ?np) (PP ?prep ?role ?np ?h) ?wh ?np
          ?gap1 ?gap2 ?sem) ==>
  (PP ?prep ?role ?wh ?np ?h ?gap1 ?gap2 ?sem))

(rule (XP (NP ?x) (NP ?agr ?case ?x) ?wh ?x ?gap1 ?gap2 ?sem) ==>
  (NP ?agr ?case ?wh ?x ?gap1 ?gap2 ?sem))

(rule (XP it (it ? ? ?x) -wh ?x ?gap ?gap t) ==>
  (:word it))

(rule (XP (clause ?word ?infl) (clause ?word ?infl ?v) -wh ?v
          ?gap1 ?gap2 ?sem) ==>
  (:ex (he thinks) "that she is tall")
  (opt-word ?word)
  (clause ?infl ?x int-subj ?v ?gap1 ?gap2 ?sem))

(rule (XP (?role ?n (advp ?v)) (advp ?v) ?wh ?v ?gap1 ?gap2 ?sem) 
  ==>
  (advp ?wh ?v ?gap1 ?gap2 ?sem))

(rule (opt-word ?word) ==> (:word ?word))
(rule (opt-word (?word)) ==> (:word ?word))
(rule (opt-word (?word)) ==>)

(rule (XP (VP ?infl ?x) *** -wh ?v ?gap1 ?gap2 ?sem) ==>
  (:ex (he promised her) "to sleep")
  (VP ?infl ?x ?subj-slot ?v ?gap1 ?gap2 ?sem))

(rule (XP (Adj ?x) *** -wh ?x ?gap ?gap ?sem) ==>
  (Adj ?x ?sem))

(rule (XP (P ?particle) *** -wh ?x ?gap ?gap t) ==>
  (prep ?particle t))

(rule (verb ?verb ?infl ?slots ?v :sem) ==>
  (:word ?verb)
  (:test (word ?verb verb ?infl ?senses)
         (member (?sem . ?subcats) ?senses)
         (member ?slots ?subcats)
         (tense-sem ?infl ?v ?tense-sem))
  (:sem ?tense-sem)
  (:sem (?sem ?v)))

(<- (tense-sem (finite ? ?tense) ?v (?tense ?v)))
(<- (tense-sem -ing ?v (progressive ?v)))
(<- (tense-sem -en  ?v (past-participle ?v)))
(<- (tense-sem infinitive ?v t))
(<- (tense-sem nonfinite ?v t))
(<- (tense-sem passive ?v (passive ?v)))

(rule (aux ?infl ?needs-infl ?v ?tense-sem) ==>
  (:word ?aux)
  (:test (word ?aux aux ?infl ?needs-infl)
         (tense-sem ?infl ?v ?tense-sem)))

(rule (aux (finite ?agr ?tense) nonfinite ?v (?sem ?v)) ==>
  (:word ?modal)
  (:test (word ?modal modal ?sem ?tense)))

(rule (noun ?agr ?slots ?x (?sem ?x)) ==>
  (:word ?noun)
  (:test (word ?noun noun ?agr ?slots ?sem)))

(rule (pronoun ?agr ?case ?wh ?x (?quant ?x (?sem ?x))) ==>
  (:word ?pro)
  (:test (word ?pro pronoun ?agr ?case ?wh ?sem)
         (if (= ?wh +wh) (= ?quant wh) (= ?quant pro))))

(rule (name ?agr ?name) ==>
  (:word ?name)
  (:test (word ?name name ?agr)))

(rule (adj ?x (?sem ?x)) ==>
  (:word ?adj)
  (:test (word ?adj adj ?sem)))

(rule (adj ?x ((nth ?n) ?x)) ==> (ordinal ?n))

(rule (art ?agr ?quant) ==>
  (:word ?art)
  (:test (word ?art art ?agr ?quant)))

(rule (prep ?prep t) ==>
  (:word ?prep)
  (:test (word ?prep prep)))

(rule (adverb ?wh ?x ?sem) ==>
  (:word ?adv)
  (:test (word ?adv adv ?wh ?pred)
         (if (= ?wh +wh)
             (= ?sem (wh ?y (?pred ?x ?y)))
             (= ?sem (?pred ?x)))))

(rule (cardinal ?n ?agr) ==>
  (:ex "five")
  (:word ?num)
  (:test (word ?num cardinal ?n ?agr)))

(rule (cardinal ?n ?agr) ==>
  (:ex "5")
  (:word ?n)
  (:test (numberp ?n)
         (if (= ?n 1)
             (= ?agr (- - + -))    ;3sing
             (= ?agr (- - - +))))) ;3plur

(rule (ordinal ?n) ==>
  (:ex "fifth")
  (:word ?num)
  (:test (word ?num ordinal ?n)))

(abbrev 1sing       (+ - - -))
(abbrev 1plur       (- + - -))
(abbrev 3sing       (- - + -))
(abbrev 3plur       (- - - +))
(abbrev 2pers       (- - - -))
(abbrev ~3sing      (? ? - ?))

(abbrev v/intrans   ((agt 1 (NP ?))))
(abbrev v/trans     ((agt 1 (NP ?)) (obj 2 (NP ?))))
(abbrev v/ditrans   ((agt 1 (NP ?)) (goal 2 (NP ?)) (obj 3 (NP ?))))
(abbrev v/trans2    ((agt 1 (NP ?)) (obj 2 (NP ?)) (goal 2 (PP to ?))))
(abbrev v/trans4    ((agt 1 (NP ?)) (obj 2 (NP ?)) (ben 2 (PP for ?))))
(abbrev v/it-null   ((nil 1 it)))
(abbrev v/opt-that  ((exp 1 (NP ?)) (con 2 (clause (that) (finite ? ?)))))
(abbrev v/subj-that ((con 1 (clause that (finite ? ?))) (exp 2 (NP ?))))
(abbrev v/it-that   ((nil 1 it) (exp 2 (NP ?))
                     (con 3 (clause that (finite ? ?)))))
(abbrev v/inf       ((agt 1 (NP ?x)) (con 3 (VP infinitive ?x))))
(abbrev v/promise   ((agt 1 (NP ?x)) (goal (2) (NP ?y))
                     (con 3 (VP infinitive ?x))))
(abbrev v/persuade  ((agt 1 (NP ?x)) (goal 2 (NP ?y))
                     (con 3 (VP infinitive ?y))))
(abbrev v/want      ((agt 1 (NP ?x)) (con 3 (VP infinitive ?x))))
(abbrev v/p-up      ((agt 1 (NP ?)) (pat 2 (NP ?)) (nil 3 (P up))))
(abbrev v/pp-for    ((agt 1 (NP ?)) (pat 2 (PP for ?))))
(abbrev v/pp-after  ((agt 1 (NP ?)) (pat 2 (PP after ?))))

(verb (ask) (query v/ditrans))
(verb (delete) (delete v/trans))
(verb (do did done doing does) (perform v/trans))
(verb (eat ate eaten) (eat v/trans))
(verb (give gave given giving) (give-1 v/trans2 v/ditrans)
      (donate v/trans v/intrans))
(verb (go went gone going goes))
(verb (have had had having has) (possess v/trans))
(verb (know knew known) (know-that v/opt-that) (know-of v/trans))
(verb (like) (like-1 v/trans))
(verb (look) (look-up v/p-up) (search v/pp-for)
      (take-care v/pp-after) (look v/intrans))
(verb (move moved moved moving moves)
      (self-propel v/intrans) (transfer v/trans2))
(verb (persuade) (persuade v/persuade))
(verb (promise) (promise v/promise))
(verb (put put put putting))
(verb (rain) (rain v/it-null))
(verb (saw) (cut-with-saw v/trans v/intrans))
(verb (see saw seen seeing) (understand v/intrans v/opt-that)
      (look v/trans) (dating v/trans))
(verb (sleep slept) (sleep v/intrans))
(verb (surprise) (surprise v/subj-that v/it-that))
(verb (tell told) (tell v/persuade))
(verb (trust) (trust v/trans ((agt 1 (NP ?)) (obj 2 (PP in ?)))))
(verb (try tried tried trying tries) (attempt v/inf))
(verb (visit) (visit v/trans))
(verb (want) (desire v/want v/persuade))

(word have    aux nonfinite -en)
(word have    aux (finite ~3sing present) -en)
(word has     aux (finite 3sing present) -en)
(word had     aux (finite ? past) -en)
(word having  aux -ing -en)

(word do      aux (finite ~3sing present) nonfinite)
(word does    aux (finite  3sing present) nonfinite)
(word did     aux (finite  ?     past)    nonfinite)

(word to      aux infinitive nonfinite)

(copula
  '((nil      ((nil 1 (NP ?x)) (nil 2 (Adj ?x))))
    (is-a     ((exp 1 (NP ?x)) (arg2 2 (NP ?y))))
    (is-loc   ((exp 1 (NP ?x)) (?prep 2 (PP ?prep ?)))))
  '((be       nonfinite -ing)
    (been     -en -ing)
    (being    -ing -en)
    (am       (finite 1sing present) -ing)
    (is       (finite 3sing present) -ing)
    (are      (finite 2pers present) -ing)
    (were     (finite (- - ? ?) past) -ing)   ; 2nd sing or pl
    (was      (finite (? - ? -) past) -ing))) ; 1st or 3rd sing

(word can    modal able      past)
(word could  modal able      present)
(word may    modal possible  past)
(word might  modal possible  present)
(word shall  modal mandatory past)
(word should modal mandatory present)
(word will   modal expected  past)
(word would  modal expected  present)
(word must   modal necessary present)

(word not not)

(noun destruction * destruction 
      (pat (2) (PP of ?)) (agt (2) (PP by ?)))
(noun beach)
(noun bone)
(noun box boxes)
(noun city cities)
(noun color)
(noun cube)
(noun doctor)
(noun dog dogs)
(noun enemy enemies)
(noun file)
(noun friend friends friend (friend-of (2) (PP of ?)))
(noun furniture *)
(noun hat)
(noun man men)
(noun saw)
(noun woman women)

(word I     pronoun 1sing (common nom) -wh speaker)
(word we    pronoun 1plur (common nom) -wh speaker+other)
(word you   pronoun 2pers (common   ?) -wh listener)
(word he    pronoun 3sing (common nom) -wh male)   
(word she   pronoun 3sing (common nom) -wh female)
(word it    pronoun 3sing (common   ?) -wh anything)
(word they  pronoun 3plur (common nom) -wh anything)

(word me    pronoun 1sing (common obj) -wh speaker)
(word us    pronoun 1plur (common obj) -wh speaker+other)
(word him   pronoun 3sing (common obj) -wh male)
(word her   pronoun 3sing (common obj) -wh female)
(word them  pronoun 3plur (common obj) -wh anything)

(word my    pronoun 1sing gen -wh speaker)
(word our   pronoun 1plur gen -wh speaker+other)
(word your  pronoun 2pers gen -wh listener)
(word his   pronoun 3sing gen -wh male)
(word her   pronoun 3sing gen -wh female)
(word its   pronoun 3sing gen -wh anything)
(word their pronoun 3plur gen -wh anything)
(word whose pronoun 3sing gen +wh anything)

(word who   pronoun ? (common ?) +wh person)
(word whom  pronoun ? (common obj) +wh person)
(word what  pronoun ? (common ?) +wh thing)
(word which pronoun ? (common ?) +wh thing)

(word who   rel-pro ? person)
(word which rel-pro ? thing)
(word that  rel-pro ? thing)
(word whom  rel-pro (common obj) person)

(word God   name 3sing)  (word Lynn  name 3sing)
(word Jan   name 3sing)  (word Mary  name 3sing)
(word John  name 3sing)  (word NY    name 3sing)
(word Kim   name 3sing)  (word LA    name 3sing)
(word Lee   name 3sing)  (word SF    name 3sing)

(word big   adj big)    (word bad   adj bad)
(word old   adj old)    (word smart adj smart)
(word green adj green)  (word red   adj red)
(word tall  adj tall)   (word fun   adj fun)

(word quickly adv -wh quickly)
(word slowly  adv -wh slowly)

(word where   adv +wh loc)
(word when    adv +wh time)
(word why     adv +wh reason)
(word how     adv +wh manner)

(word the   art 3sing the)
(word the   art 3plur group)
(word a     art 3sing a)
(word an    art 3sing a)
(word every art 3sing every)
(word each  art 3sing each)
(word all   art 3sing all)
(word some  art ?     some)

(word this  art 3sing this)
(word that  art 3sing that)
(word these art 3plur this)
(word those art 3plur that)

(word what  art ?     wh)
(word which art ?     wh)

;; This puts in numbers up to twenty, as if by
;; (word five cardinal 5 3plur)
;; (word fifth ordinal 5)

(dotimes (i 21)
  (add-word (read-from-string (format nil "~r" i))
            'cardinal i (if (= i 1) '3sing '3plur))
  (add-word (read-from-string (format nil "~:r" i)) 'ordinal i))

(word above prep)  (word about prep)  (word around prep)
(word across prep) (word after prep)  (word against prep)
(word along prep)  (word at prep)     (word away prep)
(word before prep) (word behind prep) (word below prep)
(word beyond prep) (word by prep)     (word down prep)
(word for prep)    (word from prep)   (word in prep)
(word of prep)     (word off prep)    (word on prep)
(word out prep)    (word over prep)   (word past prep)
(word since prep)  (word through prep)(word throughout prep)
(word till prep)   (word to prep)     (word under prep)
(word until prep)  (word up prep)     (word with prep)
(word without prep)

