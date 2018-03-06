# Chapter 21 {docsify-ignore}
<a id='page-715'></a>

A Grammar of English 

Prefer geniality to grammar. 

- Henry Watson Fowler 

The King's English 906) 

I I 1 he previous two chapters outline techniques for writing grammars and parsers based on 

I those grammars. It is quite straightforward to apply these techniques to applications 

JL like the CD player problem where input is limited to simple sentences like "Play 1 to 
8 without 3." But it is a major undertaking to write a grammar for unrestricted English input. 
This chapter develops a grammar that covers all the major syntactic constructions of English. It 
handles sentences of much greater complexity, such as "Kim would not have been persuaded 
by Lee to look after the dog." The grammar is not comprehensive enough to handle sentences 
chosen at random from a book, but when augmented by suitable vocabulary it is adequate for a 
wide variety of applications. 

This chapter is organized as a tour through the English language. We first cover noun 
phrases, then verb phrases, clauses, and sentences. For each category we introduce examples, 
analyze them linguistically, and finally show definite clause grammar rules that correspond to 
the analysis. 

<a id='page-716'></a>

As the last chapter should have made clear, analysis more often results in complication 
than in simplification. For example, starting with a simple rule like (S 
- -> . . VP), we soon find that we have to add arguments to handle agreement, semantics, 
and gapping information. Figure 21.1 lists the grammatical categories and 
their arguments. Note that the semantic argument, sem, is always last, and the gap 
accumulators, gapl and gap2, are next-to-last whenever they occur. All single-letter 
arguments denote metavariables; for example, each noun phrase (category NP) will 
have a semantic interpretation, sem, that is a conjunction of relations involving the 
variable x. Similarly, the h in modif i ers is a variable that refers to the head - the thing 
that is being modified. The other arguments and categories will be explained in turn, 
but it is handy to have this figure to refer back to. 

Category Arguments 
Preterminals 

name agr name 
verb verb inflection slots . sem 
rel-pro case type 

pronoun agr case wh . sem 
art agr quant 
adj X sem 
cardinal number agr 
ordinal number 
prep prep sem 
noun agr slots . sem 
aux inflection needs-inflection . sem 
adverb X sem 

Nonterminals 

S s sem 
aux-inv-S subject s sem 
clause inflection . int-subj . gapl gap2 sem 
subject agr . subj-slot int-subj gapl gap2 sem 
VP inflection . subject-slot . gapl gap2 vp 

NP agr case wh . gapl gap2 np 
NP2 agr case . gapl gap2 sem 

PP prep role wh np . gapl gap2 sem 
XP slot constituent wh . gapl gap2 sem 
Det agr wh . restriction sem 
rel-clause agr . sem 
modifiers pre/post cat info slots h gapl gap2 sem 
complement cat info slot h gapl gap2 sem 
adjunct pre/post cat info h gapl gap2 sem 
advp wh X gapl gap2 sem 

Figure 21.1: Grammatical Categories and their Arguments 

<a id='page-717'></a>
21.1 Noun Phrases 
The simplest noun phrases are names and pronouns, such as "Kim" and "them." 
The rules for these cases are simple: we build up a semantic expression from a name 
or pronoun, and since there can be no gap, the two gap accumulator arguments are 
the same (?gl). Person and number agreement is propagated in the variable ?agr, 
and we also keep track of the case of the noun phrase. English has three cases that 
are reflected in certain pronouns. In the first person singular, ". is the nominative or 
subjective case, "me" is the accusative or objective case, and "my" is the genitive case. To 
distinguish them from the genitive, we refer to the nominative and the objective cases 
as the common cases. Accordingly, the three cases will be marked by the expressions 
(common nom), (common obj), and gen, respectively. Many languages of the world 
have suffixes that mark nouns as being one case or another, but English does not. 
Thus, we use the expression (common ?) to mark nouns. 

We also distinguish between noun phrases that can be used in questions, like 
"who," and those that cannot. The ?wh variable has the value +wh for noun phrases 
like "who" or "which one" and - wh for nonquestion phrases. Here, then, are the rules 
for names and pronouns. The predicates name and pronoun are used to look up words 
in the lexicon. 

(rule (NP ?agr (common ?) -wh ?x ?gl ?gl (the ?x (name ?name ?x))) ==> 
(name ?agr ?name)) 

(rule (NP ?agr ?case ?wh ?x ?gl ?gl ?sem) ==> 
(pronoun ?agr ?case ?wh ?x ?sem)) 

Plural nouns can stand alone as noun phrases, as in "dogs," but singular nouns need 
a determiner, as in "the dog" or "Kim's friend's biggest dog." Plural nouns can also 
take a determiner, as in "the dogs." The category Det is used for determiners, and 
NP2 is used for the part of a noun phrase after the determiner: 

(rule (NP (---+) ?case -wh ?x ?gl ?g2 (group ?x ?sem)) ==> 
(:ex "dogs") ; Plural nouns don't need a determiner 
(NP2 ( +) ?case ?x ?gl ?g2 ?sem)) 

(rule (NP ?agr (common ?) ?wh ?x ?gl ?g2 ?sem) ==> 
(:ex "Every man" "The dogs on the beach") 
(Det ?agr ?wh ?x ?restriction ?sem) 
(NP2 ?agr (common ?) ?x ?gl ?g2 ?restriction)) 

Finally, a noun phrase may appear externally to a construction, in which case the 
noun phrase passed in by the first gap argument will be consumed, but no words 
from the input will be. An example is the u in "Whom does Kim like 

<a id='page-718'></a>

(rule (NP ?agr ?case ?wh ?x (gap (NP ?agr ?case ?x)) (gap nil) t) 
==> Gapped NP 
) 

Now we address the heart of the noun phrase, the NP2 category. The lone rule for NP2 
says that it consists of a noun, optionally preceded and followed by modifiers: 

(rule (NP2 ?agr (common ?) ?x ?gl ?g2 :sem) ==> 

(modifiers pre noun ?agr () ?x (gap nil) (gap nil) ?pre) 

(noun ?agr ?slots ?x ?noun) 

(modifiers post noun ?agr ?slots ?x ?gl ?g2 ?post)) 

21.2 Modifiers 
Modifiers are split into type types: Complements are modifiers that are expected by the 
head category that is being modified; they cannot stand alone. Adjuncts are modifiers 
that are not required but bring additional information. The distinction is clearest 
with verb modifiers. In "Kim visited Lee yesterday," "visited" is the head verb, "Lee" 
is a complement, and "yesterday" is an adjunct. Returning to nouns, in "the former 
mayor of Boston," "mayor" is the head noun, "of Boston" is a complement (although 
an optional one) and "former" is an adjunct. 

The predicate modi f i ers takes eight arguments, so it can be tricky to understand 
them all. The first two arguments tell if we are before or after the head (pre or 
post) and what kind of head we are modifying (noun, verb, or whatever). Next is 
an argument that passes along any required information - in the case of nouns, it 
is the agreement feature. The fourth argument is a list of expected complements, 
here called ?slots. Next is the metavariable used to refer to the head. The final 
three arguments are the two gap accumulators and the semantics, which work the 
same way here as we have seen before. Notice that the lexicon entry for each Noun 
can have a list of complements that are considered as postnoun modifiers, but there 
can be only adjuncts as prenoun modifiers. Also note that gaps can appear in the 
postmodifiers but not in the premodifiers. For example, we can have "What is Kevin 
the former mayor of where the answer might be "Boston." But even though 
we can construct a noun phrase like "the education president," where "education" 
is a prenoun modifier of "president," we cannot construct "* What is George the u 
president?," intending that the answer be "education." 

There are four cases for modification. First, a complement is a kind of modifier. 
Second, if a complement is marked as optional, it can be skipped. Third, an adjunct 
can appear in the input. Fourth, if there are no complements expected, then there 
need not be any modifiers at all. The following rules implement these four cases: 

<a id='page-719'></a>

(rule (modifiers ?pre/post ?cat ?info (?slot . ?slots) ?h 

?gl ?g3 :sem) ==> 
(complement ?cat ?info ?slot ?h ?gl ?g2 ?mod) 
(modifiers ?pre/post ?cat ?info ?slots ?h ?g2 ?g3 ?mods)) 

(rule (modifiers ?pre/post ?cat ?info ((? (?) ?) . ?slots) ?h 
?gl ?g2 ?mods) == > 
(modifiers ?pre/post ?cat ?info ?slots ?h ?gl ?g2 ?mods)) 

(rule (modifiers ?pre/post ?cat ?info ?slots ?h ?gl ?g3 :sem) ==> 
(adjunct ?pre/post ?cat ?info ?h ?gl ?g2 ?adjunct) 
(modifiers ?pre/post ?cat ?info ?slots ?h ?g2 ?g3 ?mods)) 

(rule (modifiers ???()? ?gl ?gl t) ==> ) 

We need to say more about the Ust of complements, or slots, that can be associated 
with words in the lexcion. Each slot is a list of the form i role number form), where 
the role refers to some semantic relation, the number indicates the ordering of the 
complements, and the form is the type of constituent expected: noun phrase, verb 
phrase, or whatever. The details will be covered in the following section on verb 
phrases, and compi ement will be covered in the section on XPs. For now, we give a 
single example. The complement list for one sense of the verb "visit" is: 

((agt 1 (NP ?)) (obj 2 (NP ?))) 

This means that the first complement, the subject, is a noun phrase that fills the agent 
role, and the second complement is also a noun phrase that fills the object role. 

21.3 Noun Modifiers 
There are two main types of prenoun adjuncts. Most common are adjectives, as 
in "big slobbery dogs." Nouns can also be adjuncts, as in "water meter" or "desk 
lamp." Here it is clear that the second noun is the head and the first is the modifier: 
a desk lamp is a lamp, not a desk. These are known as noun-noun compounds. In 
the following rules, note that we do not need to say that more than one adjective is 
allowed; this is handled by the rules for modi f i ers. 

(rule (adjunct pre noun ?info ?x ?gap ?gap ?sem) ==> 
(adj ?x ?sem)) 

(rule (adjunct pre noun ?info ?h ?gap ?gap :sem) ==> 
(:sem (noun-noun ?h ?x)) 
(noun ?agr () ?x ?sem)) 

After the noun there is a wider variety of modifiers. Some nouns have complements. 

<a id='page-720'></a>

which are primarily prepositional phrases, as in "mayor of Boston." These will be 
covered when we get to the lexical entries for nouns. Prepositional phrases can be 
adjuncts for nouns or verbs, as in "man in the middle" and "slept for an hour." We 
can write one rule to cover both cases: 

(rule (adjunct post ?cat ?info ?x ?gl ?g2 ?sem) ==> 
(PP ?prep ?prep ?wh ?np ?x ?gl ?g2 ?sem)) 

Here are the rules for prepositional phrases, which can be either a preposition 
followed by a noun phrase or can be gapped, as in "to whom are you speaking 
The object of a preposition is always in the objective case: "with him" not "*with he." 

(rule (PP ?prep ?role ?wh ?np ?x ?gl ?g2 :sem) ==> 
(prep ?prep t) 
(:sem (?role ?x ?np)) 
(NP ?agr (common obj) ?wh ?np ?gl ?g2 ?np-sem)) 

(rule (PP ?prep ?role ?wh ?np ?x 
(gap (PP ?prep ?role ?np ?x)) (gap nil) t) ==> ) 

Nouns can be modified by present participles, past participles, and relative clauses. 
Examples are "the man eating the snack," "the snack eaten by the man," and "the 
man that ate the snack," respectively. We will see that each verb in the lexicon is 
marked with an inflection, and that the marker - i ng is used for present participles 
while - en is used for past participles. The details of the clause will be covered later. 

(rule (adjunct post noun ?agr ?x ?gap ?gap ?sem) ==> 
(:ex (the man) "visiting me" (the man) "visited by me") 
(:test (member ?infl (-ing passive))) 
(clause ?infl ?x ? ?v (gap (NP ?agr ? ?x)) (gap nil) ?sem)) 

(rule (adjunct post noun ?agr ?x ?gap ?gap ?sem) ==> 
(rel-clause ?agr ?x ?sem)) 

It is possible to have a relative clause where it is an object, not the subject, that the 
head refers to: "the snack that the man ate." In this kind of relative clause the relative 
pronoun is optional: "The snack the man ate was delicious." The following rules say 
that if the relative pronoun is omitted then the noun that is being modified must be 
an object, and the relative clause should include a subject internally. The constant 
int-subj indicates this. 

(rule (rel-clause ?agr ?x :sem) ==> 
(:ex (the man) "that she liked" "that liked her" 
"that I know Lee liked") 

<a id='page-721'></a>
(opt-rel-pronoun ?case ?x ?int-subj ?rel-sem) 
(clause (finite ? ?) ? ?int-subj ?v 
(gap (NP ?agr ?case ?x)) (gap nil) ?clause-sem)) 

(rule (opt-rel-pronoun ?case ?x ?int-subj (?type ?x)) ==> 
(rword ?rel-pro) 

(:test (word ?rel-pro rel-pro ?case ?type))) 

(rule (opt-rel-pronoun (common obj) ?x int-subj t) ==> ) 

It should be noted that it is rare but not impossible to have names and pronouns 
with modifiers: "John the Baptist/' "lovely Rita, meter maid," "Lucy in the sky with 
diamonds," "Sylvia in accounting on the 42nd floor," "she who must be obeyed," 
Here and throughout this chapter we will raise the possibility of such rare cases, 
leaving them as exercises for the reader. 

21.4 Determiners 
We will cover three kinds of determiners. The simplest is the article: "a dog" or "the 
dogs." We also allow genitive pronouns, as in "her dog," and numbers, as in "three 
dogs." The semantic interpretation of a determiner-phrase is of the form (quantifier 
variable restriction). ... example A Si ?x (dog ?x)) or ((number 3) ?x (dog ?x)). 

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

These are the most important determiner types, but there are others, and there are 
pre- and postdeterminers that combine in restricted combinations. Predeterminers 
include all, both, half, double, twice, and such. Postdeterminers include every, 
many, several, and few. Thus, we can say "all her many good ideas" or "all the King's 
men." But we can not say "*all much ideas" or "*the our children." The details are 
complicated and are omitted from this grammar. 

<a id='page-722'></a>

21.5 Verb Phrases 
Now that we have defined modi f i ers, verb phrases are easy. In fact, we only need 
two rules. The first says a verb phrase consists of a verb optionally preceded and 
followed by modifiers, and that the meaning of the verb phrase includes the fact that 
the subject fills some role: 

(rule (VP ?infl ?x ?subject-slot ?v ?gl ?g2 :sem) ==> 
(:ex "sleeps" "quickly give the dog a bone") 
(modifiers pre verb ? () ?v (gap nil) (gap nil) ?pre-sem) 
(:sem (?role ?v ?x)) (:test (= ?subject-slot (?role 1 ?))) 
(verb ?verb ?infl (?subject-slot . ?slots) ?v ?v-sem) 
(modifiers post verb ? ?slots ?v ?gl ?g2 ?mod-sem)) 

The VP category takes seven arguments. The first is an inflection, which represents 
the tense of the verb. To describe the possibilities for this argument we need a quick 
review of some basic Unguistics. A sentence must have a finite verb, meaning a 
verb in the present or past tense. Thus, we say "Kim likes Lee," not "*Kim liking 
Lee." Subject-predicate agreement takes effect for finite verbs but not for any other 
tense. The other tenses show up as complements to other verbs. For example, the 
complement to "want" is an infinitive: "Kim wants to like Lee" and the complement 
to the modal auxiliary verb "would" is a nonf inite verb: "Kim would like Lee." If this 
were in the present tense, it would be "likes," not "like." The inflection argument 
takes on one of the forms in the table here: 

Expression Type Example 
(finite ?agr present) present tense eat, eats 
(finite ?agr past) past tense ate 
nonfinite nonfinite eat 
infinitive infinitive to eat 
-en past participle eaten 
-ing present participle eating 

The second argument is a metavariable that refers to the subject, and the third is 
the subject's complement slot. We adopt the convention that the subject slot must 
always be the first among the verb's complements. The other slots are handled by 
the postverb modifiers. The fourth argument is a metavariable indicating the verb 
phrase itself. The final three are the familiar gap and semantics arguments. As an 
example, if the verb phrase is the single word "slept," then the semantics of the verb 
phrase will be (and (past ?v) (sleep ?v)). Of course, adverbs, complements, 
and adjuncts will also be handled by this rule. 

The second rule for verb phrases handles auxiliary verbs, such as "have," "is" 
and "would." Each auxiliary verb (or aux) produces a verb phrase with a particular 

<a id='page-723'></a>

inflection when followed by a verb phrase with the required inflection. To repeat 
an example, "would" produces a finite phrase when followed by a nonfinite verb. 
"Have" produces a nonfinite when followed by a past participle. Thus, "would have 
liked" is a finite verb phrase. 

We also need to account for negation. The word "not" can not modify a bare main 
verb but can follow an auxiliary verb. That is, we can't say "*Kim not like Lee," but 
we can add an auxiliary to get "Kim does not like Lee." 

(rule (VP ?infl ?x ?subject-slot ?v ?gl ?g2 :sem) ==> 
(:ex "is sleeping" "would have given a bone to the dog." 
"did not sleep" "was given a bone by this old man") 

An aux verb, followed by a VP 
(aux ?infl ?needs-infl ?v ?aux) 
(modifiers post aux ? () ?v (gap nil) (gap nil) ?mod) 
(VP ?needs-infl ?x ?subject-slot ?v ?gl ?g2 ?vp)) 

(rule (adjunct post aux ? ?v ?gap ?gap (not ?v)) ==> 
(:word not)) 

21.6 Adverbs 
Adverbs can serve as adjuncts before or after a verb: "to boldly go," "to go boldly." 
There are some limitations on where they can occur, but it is difficult to come up 
with firm rules; here we allow any adverb anywhere. We define the category advp 
for adverbial phrase, but currently restrict it to a single adverb. 

(rule (adjunct ?pre/post verb ?info ?v ?gl ?g2 ?sem) ==> 
(advp ?wh ?v ?gl ?g2 ?sem)) 

(rule (advp ?wh ?v ?gap ?gap ?sem) ==> 
(adverb ?wh ?v ?sem)) 

(rule (advp ?wh ?v (gap (advp ?v)) (gap nil) t) ==> ) 

21.7 Clauses 
A clause consists of a subject followed by a predicate. However, the subject need not 
be realized immediately before the predicate. For example, in "Alice promised Bob 
to lend him her car" there is an infinitive clause that consists of the predicate "to lend 
him her car" and the subject "Alice." The sentence as a whole is another clause. In 

<a id='page-724'></a>

our analysis, then, a clause is a subject followed by a verb phrase, with the possibility 
that the subject will be instantiated by something from the gap arguments: 

(rule (clause ?infl ?x ?int-subj ?v ?gapl ?gap3 :sem) ==> 
(subject ?agr ?x ?subj-slot ?int-subj ?gapl ?gap2 ?subj-sem) 
(VP ?infl ?x ?subj-slot ?v ?gap2 ?gap3 ?pred-sem) 
(itest (subj-pred-agree ?agr ?infl))) 

There are now two possibilities for subject. In the first case it has already been 
parsed, and we pick it up from the gap list. If that is so, then we also need to find the 
agreement feature of the subject. If the subject was a noun phrase, the agreement will 
be present in the gap list. If it was not, then the agreement is third-person singular. 
An example of this is" That the Red Sox won surprises me," where the italicized phrase 
is a non-NP subject. The fact that we need to use "surprises" and not "surprise" 
indicates that it is third-person singular. We will see that the code (--->--) is used 
for this. 

(rule (subject ?agree ?x ?subj-slot ext-subj 
(gap ?subj) (gap nil) t) ==> 
Externally realized subject (the normal case for S) 
(rtest (slot-constituent ?subj-slot ?subj ?x ?) 

(if (= ?subj (NP ?agr ?case ?x)) 
(= ?agree ?agr) 
(= ?agree (-- + -))))) ;Non-NP subjects are 3sing 

In the second case we just parse a noun phrase as the subject. Note that the fourth 
argument to subject is either ext-subj or int-subj depending on if the subject is 
realized internally or externally. This will be important when we cover sentences in 
the next section. In case it was not already clear, the second argument to both clause 
and subject is the metavariable representing the subject. 

(rule (subject ?agr ?x (?role 1 (NP ?x)) int-subj ?gap ?gap ?sem) 
= => 
(NP ?agr (common nom) ?wh ?x (gap nil) (gap nil) ?sem)) 

Finally, the rules for subject-predicate agreement say that only finite predicates need 
to agree with their subject: 

(<- (subj-pred-agree ?agr (finite ?agr ?))) 
(<- (subj-pred-agree ? ?infl) (atom ?infl)) 

<a id='page-725'></a>
21.8 Sentences 
In the previous chapter we allowed only simple declarative sentences. The current 
grammar supports commands and four kinds of questions in addition to declarative 
sentences. It also supports thematic fronting: placing a nonsubject at the beginning of 
a sentence to emphasize its importance, as in "Smith he says his name is" or "Murder, 
she wrote" or "In God we trust." In the last example it is a prepositional phrase, not a 
noun phrase, that occurs first. It is also possible to have a subject that is not a noun 
phrase: "That the dog didn't hark puzzled Holmes." To support all these possibilities, 
we introduce a new category, XP, which stands for any kind of phrase. A declarative 
sentence is then just an XP followed by a clause, where the subject of the clause may 
or may not turn out to be the XP: 

(rule (S ?s :sem) ==> 

(:ex "Kim likes Lee" "Lee, I like _" "In god, we trust _" 

"Who likes Lee?" "Kim likes who?") 
(XP ?kind ?constituent ?wh ?x (gap nil) (gap nil) ?topic-sem) 
(clause (finite ? ?) ?x ? ?s (gap ?constituent) (gap nil) ?sem)) 

As it turns out, this rule also serves for two types of questions. The simplest kind 
of question has an interrogative noun phrase as its subject: "Who likes Lee?" or 
"What man likes Lee?" Another kind is the so-called echo question, which can be 
used only as a reply to another statement: if I tell you Kim likes Jerry Lewis, you 
could reasonably reply "Kim likes whoT Both these question types have the same 
structure as declarative sentences, and thus are handled by the same rule. 

The following table lists some sentences that can be parsed by this rule, showing 
the XP and subject of each. 

Sentence XP Subject 
Kim likes Lee Kim Kim 
Lee, Kim likes Lee Kim 
In god, we trust In god we 
That Kim likes Lee amazes That Kim likes Lee That Kim likes Lee 
Who likes Lee? Who Who 

The most common type of command has no subject at all: "Be quiet" or "Go to 
your room." When the subject is missing, the meaning is that the command refers 
toyou, the addressee of the command. The subject can also be mentioned explicitly, 
and it can be "you," as in "You be quiet," but it need not be: "Somebody shut the 
door" or "Everybody sing along." We provide a rule only for commands with subject 
omitted, since it can be difficult to distinguish a command with a subject from a 
declarative sentence. Note that commands are always nonfinite. 

<a id='page-726'></a>

(rule (S ?s :sem) ==> 

Commands have implied second-person subject 
(:ex "Give the dog a bone.") 
(:sem (command ?s)) 
(:sem (listener ?x)) 
(clause nonfinite ?x ext-subj ?s 

(gap (NP ? ? ?x)) (gap nil) ?sem)) 

Another form of command starts with "let," as in "Let me see what I can do" and 
"Let us all pray." The second word is better considered as the object of "let" rather 
than the subject of the sentence, since the subject would have to be "I" or "we." This 
kind of command can be handled with a lexical entry for "let" rather than with an 
additional rule. 

We now consider questions. Questions that can be answered by yes or no have 
the subject and auxiliary verb inverted: "Did you see him?" or "Should I have been 
doing this?" The latter example shows that it is only the first auxiliary verb that 
comes before the subject. The category a ux -i ..-S is used to handle this case: 

(rule (S ?s (yes-no ?s ?sem)) ==> 
(:ex "Does Kim like Lee?" "Is he a doctor?") 
(aux-inv-S nil ?s ?sem)) 

Questions that begin with a wh-phrase also have the auxihary verb before the subject, 
as in "Who did you see?" or "Why should I have been doing this?" The first 
constituent can also be a prepositional phrase: "For whom am I doing this?" The 
following rule parses an XP that must have the +wh feature and then parses an 
aux -i nv-S to arrive at a question: 

(rule (S ?s :sem) ==> 
(:ex "Who does Kim like _?" "To whom did he give it _? " 

"What dog does Kim like _?") 
(XP ?slot ?constituent +wh ?x (gap nil) (gap nil) ?subj-sem) 
(aux-inv-S ?constituent ?s ?sem)) 

A question can also be signaled by rising intonation in what would otherwise be a 
declarative statement: "You want some?" Since we don't have intonation information, 
we won't include this kind of question. 

The implementation for aux-inv-S is straightforward: parse an auxiliary and 
then a clause, pausing to look for modifiers in between. (So far, a "not" is the only 
modifier allowed in that position.) 

<a id='page-727'></a>

(rule (aux-inv-S ?constituent ?v :sem) ==> 
(:ex "Does Kim like Lee?" (who) "would Kim have liked") 
(aux (finite ?agr ?tense) ?needs-infl ?v ?aux-sem) 
(modifiers post aux ? () ?v (gap nil) (gap nil) ?mod) 
(clause ?needs-infl ?x int-subj ?v (gap ?constituent) (gap nil) 

?clause-sem)) 

There is one more case to consider. The verb "to be" is the most idiosyncratic in 
English. It is the only verb that has agreement differences for anything besides third-
person singular. And it is also the only verb that can be used in an a ux - i ..-S without 
a main verb. An example of this is "Is he a doctor?," where "is" clearly is not an 
auxihary, because there is no main verb that it could be auxiliary to. Other verb can 
not be used in this way: "*Seems he happy?" and"*Didtheyit?" are ungrammatical. 
The only possibiUty is "have," as in "Have you any wool?," but this use is rare. 

The following rule parses a verb, checks to see that it is a version of "be," and then 
parses the subject and the modifiers for the verb. 

(rule (aux-inv-S ?ext ?v :sem) ==> 
(:ex "Is he a doctor?") 
(verb ?be (finite ?agr ?) ((?role ?n ?xp) . ?slots) ?v ?sem) 
(rtest (word ?be be)) 
(subject ?agr ?x (?role ?n ?xp) int-subj 

(gap nil) (gap nil) ?subj-sem) 
(:sem (?role ?v ?x)) 
(modifiers post verb ? ?slots ?v (gap ?ext) (gap nil) ?mod-sem)) 

21.9 XPs 
All that remains in our grammar is the XP category. XPs are used in two ways: First, 
a phrase can be extraposed, as in "In god we trust," where "in god" will be parsed as 
an XP and then placed on the gap list until it can be taken off as an adjunct to "trust." 
Second, a phrase can be a complement, as in "He wants to be a fireman" where the 
infinitive phrase is a complement of "wants." 

As it turns out, the amount of information that needs to appear in a gap list 
is slightly different from the information that appears in a complement slot. For 
example, one sense of the verb "want" has the following complement list: 

((agt 1 (NP ?x)) (con 3 (VP infinitive ?x))) 

This says that the first complement (the subject) is a noun phrase that serves as the 
agent of the wanting, and the second is an infinitive verb phrase that is the concept of 

<a id='page-728'></a>

the wanting. The subject of this verb phrase is the same as the subject of the wanting, 
so in "She wants to go home," it is she who both wants and goes. (Contrast this to 
"He persuaded her to go home," where it is he that persuades, but she that goes.) 

But when we put a noun phrase on a gap list, we need to include its number and 
case as well as the fact that it is an NP and its metavariable, but we don't need to 
include the fact that it is an agent. This difference means we have two choices: either 
we can merge the notions of slots and gap lists so that they use a common notation 
containing all the information that either can use, or we need some way of mapping 
between them. I made the second choice, on the grounds that each notation was 
complicated enough without bringing in additional information. 

The relation slot-constituent maps between the slot notation used for complements 
and the constituent notation used in gap lists. There are eight types of 
complements, five of which can appear in gap lists: noun phrases, clauses, prepositional 
phrases, the word "it" (as in "it is raining"), and adverbial phrases. The three 
phrases that are allowed only as complements are verb phrases, particles (such as 
"up" in "look up the number"), and adjectives. Here is the mapping between the two 
notations. The *** indicates no mapping: 

(<- (slot-constituent (?role ?n (NP ?x)) 
(NP ?agr ?case ?x) ?x ?h)) 
(<- (slot-constituent (?role ?n (clause ?word ?infl)) 
(clause ?word ?infl ?v) ?v ?h)) 
(<- (slot-constituent (?role ?n (PP ?prep ?np)) 

(PP ?prep ?role ?np ?h) ?np ?h)) 
(<- (slot-constituent (?role ?n it) (it ? ? ?x) ?x ?)) 
(<- (slot-constituent (manner 3 (advp ?x)) (advp ?v) ? ?v)) 
(<- (slot-constituent (?role ?n (VP ?infl ?x)) *** ? ?)) 
(<- (slot-constituent (?role ?n (Adj ?x)) *** ?x ?)) 
(<- (slot-constituent (?role ?n (P ?particle)) *** ? ?)) 

We are now ready to define compi ement. It takes a slot descrption, maps it into a 
constituent, and then calls XP to parse that constituent: 

(rule (complement ?cat ?info (?role ?n ?xp) ?h ?gapl ?gap2 :sem) 

;; A complement is anything expected by a slot 
(:sem (?role ?h ?x)) 
(itest (slot-constituent (?role ?n ?xp) ?constituent ?x ?h)) 
(XP ?xp ?constituent ?wh ?x ?gapl ?gap2 ?sem)) 

The category XP takes seven arguments. The first two are the slot we are trying 
to fill and the constituent we need to fill it. The third is used for any additional 
information, and the fourth is the metavariable for the phrase. The last three supply 
gap and semantic information. 

<a id='page-729'></a>

Here are the first five XP categories: 

(rule (XP (PP ?prep ?np) (PP ?prep ?role ?np ?h) ?wh ?np 
?gapl ?gap2 ?sem) ==> 
(PP ?prep ?role ?wh ?np ?h ?gapl ?gap2 ?sem)) 

(rule (XP (NP ?x) (NP ?agr ?case ?x) ?wh ?x ?gapl ?gap2 ?sem) ==> 
(NP ?agr ?case ?wh ?x ?gapl ?gap2 ?sem)) 

(rule (XP it (it ? ? ?x) -wh ?x ?gap ?gap t) ==> 
(:word it)) 

(rule (XP (clause ?word ?infl) (clause ?word ?infl ?v) -wh ?v 

?gapl ?gap2 ?sem) ==> 
(:ex (he thinks) "that she is tall") 
(opt-word ?word) 

(clause ?infl ?x int-subj ?v ?gapl ?gap2 ?sem)) 

(rule (XP (?role ?n (advp ?v)) (advp ?v) ?wh ?v ?gapl ?gap2 ?sem) 

(advp ?wh ?v ?gapl ?gap2 ?sem)) 

The category opt-word parses a word, which may be optional. For example, one 

sense of "know" subcategorizes for a clause with an optional "that": we can say 

either "I know that he's here" or "I know he's here." The complement hst for "know" 

thuscontains the slot (con 2 (clause (that) (finite ? ?))). If the "that" had 

been obligatory, it would not have parentheses around it. 

(rule (opt-word ?word) ==> (:word ?word)) 
(rule (opt-word (?word)) ==> (iword ?word)) 
(rule (opt-word (?word)) ==>) 

Finally, here are the three XPs that can not be extraposed: 

(rule (XP (VP ?infl ?x) *** -wh ?v ?gapl ?gap2 ?sem) ==> 
(:ex (he promised her) "to sleep") 
(VP ?infl ?x ?subj-slot ?v ?gapl ?gap2 ?sem)) 

(rule (XP (Adj ?x) *** -wh ?x ?gap ?gap ?sem) ==> 
(Adj ?x ?sem)) 

(rule (XP (P ?particle) *** -wh ?x ?gap ?gap t) ==> 
(prep ?particle t)) 

<a id='page-730'></a>

21.10 Word Categories 
Each word category has a rule that looks words up in the lexicon and assigns the right 
features. The relation word is used for all lexicon access. We will describe the most 
complicated word class, verb, and just list the others. 

Verbs are complex because they often are polysemous - they have many meanings. 
In addition, each meaning can have several different complement lists. Thus, an 
entry for a verb in the lexicon will consist of the verb form, its inflection, and a list 
of senses, where each sense is a semantics followed by a list of possible complement 
lists. Here is the entry for the verb "sees," indicating that it is a present-tense verb with 
three senses. The understand sense has two complement lists, which correspond to 
"He sees" and "He sees that you are right." The 100 k sense has one complement list 
corresponding to "He sees the picture," and the dati ng sense, corresponding to "He 
sees her (only on Friday nights)," has the same complement list. 

> (?- (word sees verb ?infl ?senses)) 
?INFL = (FINITE (--+-) PRESENT) 
7SENSES = ((UNDERSTAND ((AGT 1 (NP ?3))) 

((EXP 1 (NP ?4)) 

(CON 2 (CLAUSE (THAT) (FINITE ?5 ?6))))) 
(LOOK ((AGT 1 (NP 17)) (OBJ 2 (NP ?8)))) 
(DATING ((AGT 1 (NP ?9)) (OBJ 2 (NP ?10))))) 

The category verb takes five arguments: the verb itself, its inflection, its complement 
list, its metavariable, and its semantics. The member relations are used to pick a sense 
from the list of senses and a complement Hst from the list of lists, and the semantics 
is built from semantic predicate for the chosen sense and the metavariable for the 
verb: 

(rule (verb ?verb ?infl ?slots ?v :sem) ==> 
(:word ?verb) 
(:test (word ?verb verb ?infl ?senses) 

(member (?sem . ?subcats) ?senses) 
(member ?slots ?subcats) 
(tense-sem ?infl ?v ?tense-sem)) 

(:sem ?tense-sem) 
(:sem (?sem ?v))) 

It is difficulty to know how to translate tense information into a semantic interpretation. 
Different applications will have different models of time and thus will want 
different interpretations. The relation tense-sem gives semantics for each tense. 
Here is a very simple definition of tense-sem: 

<a id='page-731'></a>
(<- (tense-sem (finite ? ?tense) ?v (?tense ?Y))) 
(<- (tense-sem -ing ?v (progressive ?v))) 
(<- (tense-sem -en ?v (past-participle ?v))) 
(<- (tense-sem infinitive ?v t)) 
(<- (tense-sem nonfinite ?v t)) 
(<- (tense-sem passive ?v (passive ?v))) 

Auxiliary verbs and modal verbs are listed separately: 

(rule (aux ?infl ?needs-infl ?v ?tense-sem) ==> 
(:word ?aux) 
(itest (word ?aux aux ?infl ?needs-infl) 

(tense-sem ?infl ?v ?tense-sem))) 

(rule (aux (finite ?agr ?tense) nonfinite ?v (?sem ?v)) ==> 
(:word ?modal) 
(:test (word ?modal modal ?sem ?tense))) 

Nouns, pronouns, and names are also listed separately, although they have much 
in common. For pronouns we use quantifier wh or pro, depending on if it is a wh-
pronoun or not. 

(rule (noun ?agr ?slots ?x (?sem ?x)) ==> 
(:word ?noun) 
(:test (word ?noun noun ?agr ?slots ?sem))) 

(rule (pronoun ?agr ?case ?wh ?x (?quant ?x (?sem ?x))) ==> 
(rword ?pro) 
(:test (word ?pro pronoun ?agr ?case ?wh ?sem) 

(if (= ?wh +wh) (= ?quant wh) (= ?quant pro)))) 

(rule (name ?agr ?name) ==> 
(iword ?name) 

(:test (word ?name name ?agr))) 

Here are the rules for the remaining word classes: 

(rule (adj ?x (?sem ?x)) ==> 
(:word ?adj) 
(:test (word ?adj adj ?sem))) 

(rule (adj ?x ((nth ?n) ?x)) ==> (ordinal ?n)) 

(rule (art ?agr ?quant) ==> 
(:word ?art) 
(:test (word ?art art ?agr ?quant))) 

<a id='page-732'></a>

(rule (prep ?prep t) ==> 
(:word ?prep) 
(:test (word ?prep prep))) 

(rule (adverb ?wh ?x ?sem) ==> 
(rword ?adv) 
(:test (word ?adv adv ?wh ?pred) 

(if (= ?wh +wh) 
(= ?sem (wh ?y (?pred ?x ?y))) 
(= ?sem (?pred ?x))))) 

(rule (cardinal ?n ?agr) ==> 
(:ex "five") 
(rword ?num) 
(rtest (word ?nuni cardinal ?n ?agr))) 

(rule (cardinal ?n ?agr) ==> 
(rex "5") 
(rword ?n) 
(rtest (numberp ?n) 

(if (= ?n 1) 
(= ?agr (-- + -)) ;3sing 
(= ?agr ( +))))) ;3plur 

(rule (ordinal ?n) ==> 
(rex "fifth") 
(rword ?num) 
(rtest (word ?num ordinal ?n))) 

21.11 The Lexicon 
The lexicon itself consists of a large number of entries in the word relation, and it 
would certainly be possible to ask the lexicon writer to make a long list of word facts. 
But to make the lexicon easier to read and write, we adopt three useful tools. First, 
we introduce a system of abbreviations. Common expressions can be abbreviated 
with a symbol that will be expanded by word. Second, we provide the macros verb 
and noun to cover the two most complex word classes. Third, we provide a macro 
word that makes entries into a hash table. This is more efficient than compiling a 
word relation consisting of hundreds of Prolog clauses. 

The implementation of these tools is left for the next section; here we show the 
actual lexicon, starting with the list of abbreviations. 

The first set of abbreviations defines the agreement features. The obvious way to 
handle agreement is with two features, one for person and one for number. So first-
person singular might be represented (1 si ng). A problem arises when we want 

<a id='page-733'></a>

to describe verbs. Every verb except "be" makes the distinction only between third-
person singular and all the others. We don't want to make five separate entries in the 
lexicon to represent all the others. One alternative is to have the agreement feature be 
a set of possible values, so all the others would be a single set of five values rather than 
five separate values. This makes a big difference in cutting down on backtracking. 
The problem with this approach is keeping track of when to intersect sets. Another 
approach is to make the agreement feature be a list of four binary features, one each 
for first-person singular, first-person plural, third-person singular, and third-person 
plural. Then "all the others" can be represented by the list that is negative in the third 
feature and unknown in all the others. There is no way to distinguish second-person 
singular from plural in this scheme, but English does not make that distinction. Here 
are the necessary abbreviations: 

(abbrev Ising (+---)) 
(abbrev Iplur (-+ - -)) 
(abbrev 3sing (--+-)) 
(abbrev Splur (---+)) 
(abbrev 2pers (--- -)) 
(abbrev ~3sing (??-?)) 

The next step is to provide abbreviations for some of the common verb complement 
lists: 

(abbrev v/intrans ((agt 1 (NP ?)))) 
(abbrev v/trans ((agt 1 (NP ?)) (obj 2 (NP ?)))) 
(abbrev v/ditrans ((agt 1 (NP ?)) (goal 2 (NP ?)) (obj 3 (NP ?)))) 
(abbrev v/trans2 ((agt 1 (NP ?)) (obj 2 (NP ?)) (goal 2 (PP to ?)))) 
(abbrev v/trans4 ((agt 1 (NP ?)) (obj 2 (NP ?)) (ben 2 (PP for ?)))) 
(abbrev v/it-null ((nil 1 it))) 
(abbrev v/opt-that ((exp 1 (NP ?)) (con 2 (clause (that) (finite ? ?))))) 
(abbrev v/subj-that ((con 1 (clause that (finite ? ?))) (exp 2 (NP ?)))) 
(abbrev v/it-that ((nil 1 it) (exp 2 (NP ?)) 

(con 3 (clause that (finite ? ?))))) 
(abbrev v/inf ((agt 1 (NP ?x)) (con 3 (VP infinitive ?x)))) 
(abbrev v/promise ((agt 1 (NP ?x)) (goal (2) (NP ?y)) 

(con 3 (VP infinitive ?x)))) 
(abbrev v/persuade ((agt 1 (NP ?x)) (goal 2 (NP ?y)) 

(con 3 (VP infinitive ?y)))) 
(abbrev v/want ((agt 1 (NP ?x)) (con 3 (VP infinitive ?x)))) 
(abbrev v/p-up ((agt 1 (NP ?)) (pat 2 (NP ?)) (nil 3 (P up)))) 
(abbrev v/pp-for ((agt 1 (NP ?)) (pat 2 (PP for ?)))) 
(abbrev v/pp-after ((agt 1 (NP ?)) (pat 2 (PP after ?)))) 

<a id='page-734'></a>

Verbs 

The macro verb allows us to list verbs in the form below, where the spellings of each 
tense can be omitted if the verb is regular: 

(verb (base past-tense past-participle present-participle present-plural) 
{semantics complement-list,..) ...) 

For example, in the following list "ask" is regular, so only its base-form spelling is 
necessary. "Do," on the other hand, is irregular, so each form is spelled out. The 
haphazard list includes verbs that are either useful for examples or illustrate some 
unusual complement list. 

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
(verb (rain) (rain v/it-nulD) 
(verb (saw) (cut-with-saw v/trans v/intrans)) 
(verb (see saw seen seeing) (understand v/intrans v/opt-that) 

(look v/trans)(dating v/trans)) 
(verb (sleep slept) (sleep v/intrans)) 
(verb (surprise) (surprise v/subj-that v/it-that)) 
(verb (tell told) (tell v/persuade)) 
(verb (trust) (trust v/trans ((agt 1 (NP ?)) (obj 2 (PP in ?))))) 
(verb (try tried tried trying tries) (attempt v/inf)) 
(verb (visit) (visit v/trans)) 
(verb (want) (desire v/want v/persuade)) 

<a id='page-735'></a>

Auxiliary Verbs 

Auxiliary verbs are simple enough to be described directly with the word macro. Each 
entry lists the auxiliary itself, the tense it is used to construct, and the tense it must 
be followed by. The auxiliaries "have" and "do" are listed, along with "to," which is 
used to construct infinitive clauses and thus can be treated as if it were an auxiliary. 

(word have aux nonfinite -en) 
(word have aux (finite ~3sing present) -en) 
(word has aux (finite 3sing present) -en) 
(word had aux (finite ? past) -en) 
(word having aux -ing -en) 

(word do aux (finite ~3sing present) nonfinite) 
(word does aux (finite 3sing present) nonfinite) 
(word did aux (finite ? past) nonfinite) 

(word to aux infinitive nonfinite) 

The auxiliary "be" is special: in addition to its use as both an auxiliary and main 
verb, it also is used in passives and as the main verb in aux-inverted sentences. The 
function copul a is used to keep track of all these uses. It will be defined in the next 
section, but you can see it takes two arguments, a list of senses for the main verb, and 
a list of entries for the auxiliary verb. The three senses correspond to the examples 
"He is a fool," "He is a Republican," and "He is in Indiana," respectively. 

(copula 

'((nil ((nil 1 (NP ?x)) (nil 2 (Adj ?x)))) 
(is-a ((exp 1 (NP ?x)) (arg2 2 (NP ?y)))) 
(is-loc ((exp 1 (NP ?x)) (?prep 2 (PP ?prep ?))))) 

'((be nonfinite -ing) 
(been -en -ing) 
(being -ing -en) 
(am (finite Ising present) -ing) 
(is (finite 3sing present) -ing) 
(are (finite 2pers present) -ing) 
(were (finite (--??) past) -ing) ; 2nd sing or pi 
(was (finite (?-?-) past) -ing))) ; 1st or 3rd sing 

Following are the modal auxiliary verbs. Again, it is difficult to specify semantics 
for them. The word "not" is also listed here; it is not an auxiliary, but it does modify 
them. 

<a id='page-736'></a>

(word can modal able past) 
(word could modal able present) 
(word may modal possible past) 
(word might modal possible present) 
(word shall modal mandatory past) 
(word should modal mandatory present) 
(word will modal expected past) 
(word would modal expected present) 
(word must modal necessary present) 

(word not not) 

Nouns 

No attempt has been made to treat nouns seriously. We list enough nouns here to 
make some of the examples work. The first noun shows a complement list that is 
sufficient to parse "the destruction of the city by the enemy." 

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

Pronouns 

Here we list the nominative, objective, and genitive pronouns, followed by interrogative 
and relative pronouns. The only thing missing are reflexive pronouns, such as 
"myself." 

<a id='page-737'></a>

(word I pronoun Ising (common nom) -wh speaker) 
(word we pronoun Iplur (common nom) -wh speaker+other) 
(word you pronoun 2pers (common ?) -wh 1istener) 
(word he pronoun 3sing (common nom) -wh male) 
(word she pronoun 3s ing (common nom) -wh female) 
(word it pronoun 3s ing (common ?) -wh anything) 
(word they pronoun 3plur (common nom) -wh anything) 

(word me pronoun Ising (common obj) -wh speaker) 
(word us pronoun Iplur (common obj) -wh speaker+other) 
(word him pronoun 3sing (common obj) -wh male) 
(word her pronoun 3sing (common obj) -wh female) 
(word them pronoun 3plur (common obj) -wh anything) 

(word my pronoun Ising gen -wh speaker) 
(word our pronoun Iplur gen -wh speaker+other) 
(word your pronoun 2pers gen -wh 1istener) 
(word his pronoun 3sing gen -wh male) 
(word her pronoun 3sing gen -wh female) 
(word its pronoun 3s ing gen -wh anything) 
(word their pronoun 3plur gen -wh anything) 
(word whose pronoun 3sing gen +wh anything) 

(word who pronoun ? (common ?) +wh person) 
(word whom pronoun ? (common obj) +wh person) 
(word what pronoun ? (common ?) +wh thing) 
(word which pronoun ? (common ?) +wh thing) 

(word who rel-pro ? person) 
(word which rel-pro ? thing) 
(word that rel-pro ? thing) 
(word whom rel-pro (common obj) person) 

Names 

The following names were convenient for one example or another: 

(word God name 3sing) (word Lynn name 3sing) 
(word Jan name 3sing) (word Mary name 3sing) 
(word John name 3sing) (word NY name 3sing) 
(word Kim name 3sing) (word LA name 3sing) 
(word Lee name 3sing) (word SF name 3sing) 

<a id='page-738'></a>

Adjectives 

Here are a few adjectives: 

(word big adj big) (word bad adj bad) 
(word old adj old) (word smart adj smart) 
(word green adj green) (word red adj red) 
(word tal l adj tall ) (word fun adj fun) 

Adverbs 

The adverbs covered here include interrogatives: 

(word quickly adv -wh quickly) 
(word slowly adv -wh slowly) 

(word where adv +wh loc) 
(word when adv +wh time) 
(word why adv +wh reason) 
(word how adv +wh manner) 

Articles 

The common articles are listed here: 

(word the art 3sing the) 
(word the art Splur group) 
(word a art Ssing a) 
(word an art Ssing a) 
(word every art Ssing every) 
(word each art Ssing each) 
(word all art Ssing all) 
(word some art ? some) 

(word this art Ssing this) 
(word that art Ssing that) 
(word these art Splur this) 
(word those art Splur that) 

(word what art ? wh) 
(word which art ? wh) 

<a id='page-739'></a>

Cardinal and Ordinal Numbers 

We can take advantage of format's capabilities to fill up the lexicon. To go beyond 
20, we would need a subgrammar of numbers. 

This puts in numbers up to twenty, as if by 

(word five cardinal 5 3plur) 

(word fifth ordinal 5) 

(dotimes (i 21) 
(add-word (read-from-string (format nil "~r" i)) 
'cardinal i (if (= i 1) 'Ssing 'Splur)) 
(add-word (read-from-string (format nil "~:r" i)) Ordinal i)) 

Prepositions 

Here is a fairly complete list of prepositions: 

(word above prep) (word about prep) (word around prep) 
(word across prep) (word after prep) (word against prep) 
(word along prep) (word at prep) (word away prep) 
(word before prep) (word behind prep) (word below prep) 

(word beyond prep) (word by prep) (word down prep) 
(word for prep) (word from prep) (word in prep) 
(word of prep) (word off prep) (word on prep) 
(word out prep) (word over prep) (word past prep) 

(word since prep) (word through prep)(word throughout prep) 

(word till prep) (word to prep) (word under prep) 

(word until prep) (word up prep) (word with prep) 

(word without prep) 

21.12 Supporting the Lexicon 
This section describes the implementation of the macros word, verb, noun, and 
abbrev. Abbreviations are stored in a hash table. The macro abbrev and the functions 
get-abbrev and clear-abbrevs define the interface. We will see how to expand 
abbreviations later. 

<a id='page-740'></a>

(defvar *abbrevs* (make-hash-table)) 

(defmacro abbrev (symbol definition) 
"Make symbol be an abbreviation for definition." 

'(setf (gethash '.symbol *abbrevs*) '.definition)) 
(defun clear-abbrevs () (clrhash *abbrevs*)) 
(defun get-abbrev (symbol) (gethash symbol *abbrevs*)) 

Words are also stored in a hash table. Currently, words are symbols, but it might 
bea better idea to use strings for words, since then we could maintain capitalization 
information. The macro word or the function add-word adds a word to the lexicon. 
When used as an index into the hash table, each word returns a list of entries, where 
the first element of each entry is the word's category, and the other elements depend 
on the category. 

(defvar *words* (make-hash-table :size 500)) 

(defmacro word (word cat &rest info) 
"Put word, with category and subcat info, into lexicon." 
'(add-word '.word '.cat ..(mapcar #'kwote info))) 

(defun add-word (word cat &rest info) 
"Put word, with category and other info, into lexicon." 
(push (cons cat (mapcar #'expand-abbrevs-and-variables info)) 

(gethash word *words*)) 
word) 

(defun kwote (x) (list 'quote x)) 

The function expand-abbrevs-and-variables expands abbreviations and substitutes 
variable structures for symbols beginning with ?. This makes it easier to make 
a copy of the structure, which will be needed later. 

(defun expand-abbrevs-and-variables (exp) 
"Replace all variables in exp with vars, and expand abbrevs." 
(let ((bindings nil)) 

(labels 
((expand (exp) 

(cond 
((lookup exp bindings)) 
((eq exp '?) (?)) 
((variable-p exp) 

(let ((var (?))) 
(push (cons exp var) bindings) 
var)) 

((consp exp) 
(reuse-cons (expand (first exp)) 

<a id='page-741'></a>
(expand (rest exp)) 
exp)) 
(t (multiple-value-bind (expansion found?) 
(get-abbrev exp) 

(if found? 
(expand-abbrevs-and-variables expansion) 
exp)))))) 

(expand exp)))) 

Now we can store words in the lexicon, but we need some way of getting them out. 
The function word/. takes a word (which must be instantiated to a symbol) and a 
category and optional additional information and finds the entries in the lexicon for 
that word that unify with the category and additional information. For each match, 
it calls the supplied continuation. This means that word/. is a replacement for a long 
list of word facts. There are three differences: word/n hashes, so it will be faster; it is 
incremental (you can add a word at a time without needing to recompile); and it can 
not be used when the word is unbound. (It is not difficult to change it to handle an 
unbound word using maphash, but there are better ways of addressing that problem.) 

(defun word/n (word cat cont &rest info) 
"Retrieve a word from the lexicon." 
(unless (unbound-var-p (deref word)) 

(let ((old-trail (fil 1-pointer nrail*))) 
(dolist (old-entry (gethash word *words*)) 
(let ((entry (deref-copy old-entry))) 

(when (and (consp entry) 
(unify! cat (first entry)) 
(unify! info (rest entry))) 

(funcall cont))) 
(undo-bindings! old-trail))))) 

Note that word/n does not follow our convention of putting the continuation last. 
Therefore, we will need the following additional functions: 

(defun word/2 (w cat cont) (word/n w cat cont)) 
(defun word/3 (w cat a cont) (word/n w cat cont a)) 
(defun word/4 (w cat a b cont) (word/n w cat cont a b)) 
(defun word/5 (w cat a b c cont) (word/n w cat cont a b c)) 
(defun word/6 (w cat a b c d cont) (word/n w cat cont a bed)) 

We could create the whole lexicon with the macro word, but it is convenient to create 
specific macros for some classes. The macro noun is used to generate two entries, one 
for the singular and one for the plural. The arguments are the base noun, optionally 
followed by the plural (which defaults to the base plus "s"), the semantics (which 

<a id='page-742'></a>

defaults to the base), and a list of complements. Mass nouns, like "furniture," have 
only one entry, and are marked by an asterisk where the plural would otherwise be. 

(defmacro noun (base &rest args) 
"Add a noun and its plural to the lexicon." 
*(add-noun-form '.base ,(mapcar #'kwote args))) 

(defun add-noun-form (base &optional (plural (symbol base 's)) 
(sem base) &rest slots) 

(if (eq plural '*) 
(add-word base 'noun *? slots sem) 
(progn 

(add-word base 'noun '3sing slots sem) 
(add-word plural 'noun '3plur slots sem)))) 

Verbs are more complex. Each verb has seven entries: the base or nonfinite, the 
present tense singular and plural, the past tense, the past-participle, the present-
participle, and the passive. The macro verb automatically generates all seven entries. 
Verbs that do not have all of them can be handled by individual calls to word. We 
automatically handle the spelling for the simple cases of adding "s," "ing," and "ed," 
and perhaps stripping a trailing vowel. More irregular spellings have to be specified 
explicitly. Here are three examples of the use of verb: 

(verb (do did done doing does) (perform v/trans)) 
(verb (eat ate eaten) (eat v/trans)) 

(verb (trust) (trust v/trans ((agt 1 (NP ?)) (obj 2 (PP in ?))))) 

And here is the macro definition: 

(defmacro verb ((base &rest forms) &body senses) 
"Enter a verb into the lexicon." 

'(add-verb '.senses '.base .(mapcar #'kwote (mklist forms)))) 
(defun add-verb (senses base &optional 
(past (symbol (strip-vowel base) 'ed)) 
(past-part past) 
(pres-part (symbol (strip-vowel base) 'ing)) 
(plural (symbol base 's))) 

"Enter a verb into the lexicon." 
(add-word base 'verb 'nonfinite senses) 
(add-word base 'verb '(finite ~3sing present) senses) 
(add-word past 'verb '(finite ? past) senses) 
(add-word past-part 'verb '-en senses) 
(add-word pres-part 'verb '-ing senses) 
(add-word plural 'verb '(finite 3sing present) senses) 
(add-word past-part 'verb 'passive 

<a id='page-743'></a>
(mapcar #'passivize-sense 
(expand-abbrevs-and-vari ables senses)))) 

This uses a few auxiliary functions. First, stri p-vowel removes a vowel if it is the 
last character of the given argument. The idea is that for a verb like "fire," stripping 
the vowel yields "fir," from which we can get "fired" and "firing" automatically. 

(defun strip-vowel (word) 
"Strip off a trailing vowel from a string." 
(let* ((str (string word)) 

(end (- (length str) 1))) 

(if (vowel-p (char str end)) 
(subseq str 0 end) 
str))) 

(defun vowel-p (char) (find char "aeiou" :test #'char-equal)) 

We also provide a function to generate automatically the passive sense with the 
proper complement list(s). The idea is that the subject slot of the active verb becomes 
an optional slot marked by the preposition "by," and any slot that is marked with 
number 2 can be promoted to become the subject: 

(defun passivize-sense (sense) 
The first element of sense is the semantics; rest are slots 
(cons (first sense) (mapcan #*passivize-subcat (rest sense)))) 

(defun passivize-subcat (slots) 

"Return a list of passivizations of this subcat frame." 
Whenever the 1 slot is of the form (?any 1 (NP ?)), 
demote the 1 to a (3), and promote any 2 to a 1. 

(when (and (eql (slot-number (first slots)) 1) 
(starts-with (third (first slots)) 'NP)) 
(let ((old-1 .(,(first (first slots)) (3) (PP by ?)))) 

(loop for slot in slots 
when (eql (slot-number slot) 2) 
collect '((.(first slot) 1 .(third slot)) 

,@(remove slot (rest slots)) 
.old-1))))) 

(defun slot-number (slot) (first-or-self (second slot))) 

Finally, we provide a special function just to define the copula, "be." 

<a id='page-744'></a>

(defun copula (senses entries) 
"Copula entries are both aux and main verb." 
They also are used in passive verb phrases and aux-inv-S 

(dolist (entry entries) 
(add-word (first entry) 'aux (second entry) (third entry)) 
(add-word (first entry) 'verb (second entry) senses) 
(add-word (first entry) 'aux (second entry) 'passive) 
(add-word (first entry) 'be))) 

The remaining functions are used for testing, debugging, and extending the grammar. 
First, we need functions to clear everything so that we can start over. These functions 
can be placed at the top of the lexicon and grammar files, respectively: 

(defun clear-lexicon () 
(clrhash *words*) 
(clear-abbrevs)) 

(defun clear-grammar () 
(clear-examples) 
(clear-db)) 

Testing could be done with run-exampl es, but it is convenient to provide another 
interface, the macro try (and its corresponding function, try-dcg). Both macro and 
function can be invoked three ways. With no argument, all the examples stored by 
: ex are run. When the name of a category is given, all the examples for that category 
alone are run. Finally, the user can supply both the name of a category and a list of 
words to test whether those words can be parsed as that category. This option is only 
available for categories that are listed in the definition: 

(defmacro try (&optional cat &rest words) 
"Tries to parse WORDS as a constituent of category CAT. 
With no words, runs all the :ex examples for category. 
With no cat. runs all the examples." 
'(try-dcg '.cat '.words)) 

(defun try-dcg (&optional cat words) 
"Tries to parse WORDS as a constituent of category CAT. 
With no words, runs all the :ex examples for category. 
With no cat. runs all the examples." 
(if (null words) 

(run-examples cat) 

(let ((args '((gap nil) (gap nil) ?sem .words ()))) 
(mapc #'test-unknown-word words) 
(top-level-prove 

(ecase cat 
(np '((np ? ? ?wh ?x .args))) 

<a id='page-745'></a>
(vp '((vp ?infl ?x ?sl ?v ,@args))) 
(pp '((pp ?prep ?role ?wh ?x ,@args))) 
(xp *((xp ?slot ?constituent ?wh ?x .args))) 
(s *((s ? ?sem .words ()))) 
(rel-clause '((rel-clause ? ?x ?sem .words ()))) 
(clause '((clause ?infl ?x ?int-subj ?v ?gl ?g2 

?sem .words ())))))))) 

(defun test-unknown-word (word) 
"Print a warning message if this is an unknown word." 
(unless (or (gethash word *words*) (numberp word)) 

(warn ""&Unknown word: ~a" word))) 

21.13 Other Primitives 
To support the -.test predicates made in various grammar rules we need definitions 
of the Prolog predicates i f, member, =, numberp, and atom. They are repeated here: 

(<- (if ?test ?then) (if ?then ?else (fail))) 
(<- (if ?test ?then ?else) (call ?test) ! (call ?then)) 
(<- (if ?test ?then ?else) (call ?else)) 

(<- (member ?item (?item . ?rest))) 

(<- (member ?item (?x . ?rest)) (member ?item ?rest)) 

(<- (= ?x ?x)) 

(defun numberp/1 (x cont) 
(when (numberp (deref x)) 
(funcall cont))) 

(defun atom/1 (x cont) 
(when (atom (deref x)) 
(funcall cont))) 

(defun cal 1/1 (goal cont) 
"Try to prove goal by calling it." 
(deref goal) 
(apply (make-predicate (first goal) 

(length (args goal))) 
(append (args goal) (list cont)))) 

<a id='page-746'></a>

21.14 Examples 
Here are some examples of what the parser can handle. I have edited the output 
by changing variable names like ? 168 to more readable names like ?J. The first 
two examples show that nested clauses are supported and that we can extract a 
constituent from a nested clause: 

> (try S John promised Kim to persuade Lee to sleep) 

?SEM = (AND (THE ?J (NAME JOHN ?J)) (AGT ?P ?J) 
(PAST ?P) (PROMISE ?P) 
(GOAL ?P ?K) (THE ?K (NAME KIM ?K)) 
(CON ?P ?PER) (PERSUADE ?PER) (GOAL ?PER ?L) 
(THE ?L (NAME LEE ?L)) (CON ?PER ?S) (SLEEP ?S)); 

> (try S Who did John promise Kim to persuade to sleep) 

?SEM = (AND (WH ?W (PERSON ?W)) (PAST ?P) 
(THE ?J (NAME JOHN ?J)) (AGT ?P ?J) 
(PROMISE ?P) (GOAL ?P ?K) 
(THE ?K (NAME KIM ?K)) (CON ?P ?PER) 
(PERSUADE ?PER) (GOAL ?PER ?W) 
(CON ?PER ?S) (SLEEP ?S)); 

In the next example, the "when" can be interpreted as asking about the time of any of 
the three events: the promising, the persuading, or the sleeping. The grammar finds 
all three. 

> (try S When did John promise Kim to persuade Lee to sleep) 

?SEM = (AND (WH ?W (TIME ?S ?W)) (PAST ?P) 
(THE ?J (NAME JOHN ?J)) (AGT ?P ?J) 
(PROMISE ?P) (GOAL ?P ?K) 
(THE ?K (NAME KIM ?K)) (CON ?P ?PER) 
(PERSUADE ?PER) (GOAL ?PER ?L) 
(THE ?L (NAME LEE ?L)) (CON ?PER ?S) 
(SLEEP ?S)); 

?SEM = (AND (WH ?W (TIME ?PER ?W)) (PAST ?P) 
(THE ?J (NAME JOHN ?J)) (AGT ?P ?J) 
(PROMISE ?P) (GOAL ?P ?K) 
(THE ?K (NAME KIM ?K)) (CON ?P ?PER) 
(PERSUADE ?PER) (GOAL ?PER ?L) 
(THE ?L (NAME LEE ?L)) (CON ?PER ?S) 
(SLEEP ?S)); 

<a id='page-747'></a>
?SEM = (AND (WH ?W (TIME ?P ?W)) (PAST ?P) 
(THE ?J (NAME JOHN ?J)) (AGT ?P ?J) 
(PROMISE ?P) (GOAL ?P ?K) 
(THE ?K (NAME KIM ?K)) (CON ?P ?PER) 
(PERSUADE ?PER) (GOAL ?PER ?L) 
(THE ?L (NAME LEE ?L)) (CON ?PER ?S) 
(SLEEP ?S)). 

The next example shows auxiliary verbs and negation. It is ambiguous between 
an interpretation where Kim is searching for Lee and one where Kim is looking at 
something unspecified, on Lee's behalf. 

> (try S Kim would not have been looking for Lee) 

?SEM = (AND (THE ?K (NAME KIM ?K)) (AGT ?S ?K) 
(EXPECTED ?S) (NOT ?S) (PAST-PARTICIPLE ?S) 
(PROGRESSIVE ?S) (SEARCH ?S) (PAT ?S ?L) 
(PAT ?S ?L) (THE ?L (NAME LEE ?L))); 

?SEM = (AND (THE ?K (NAME KIM ?K)) (AGT ?2 ?K) 
(EXPECTED ?2) (NOT ?2) (PAST-PARTICIPLE ?LOOK) 
(PROGRESSIVE ?LOOK) (LOOK ?LOOK) (FOR ?LOOK ?L) 
(THE ?L (NAME LEE ?L))); 

The next two examples are unambiguous: 

> (try s It should not surprise you that Kim does not like Lee) 

?SEM = (AND (MANDATORY ?2) (NOT ?2) (SURPRISE ?2) (EXP ?2 ?YOU) 
(PRO ?YOU (LISTENER ?YOU)) (CON ?2 ?LIKE) 
(THE ?K (NAME KIM ?K)) (AGT ?LIKE ?K) 
(PRESENT ?LIKE) (NOT ?LIKE) (LIKE-1 ?LIKE) 
(OBJ ?LIKE ?L) (THE ?L (NAME LEE ?L))); 

> (try s Kim did not want Lee to know that the man knew her) 

?SEM = (AND (THE ?K (NAME KIM ?K)) (AGT ?W ?K) (PAST ?W) 
(NOT ?W) (DESIRE ?W) (GOAL ?W ?L) 
(THE ?L (NAME LEE ?L)) (CON ?W ?KN) 
(KNOW-THAT ?KN) (CON ?KN ?KN2) 
(THE ?M (MAN ?M)) (AGT ?KN2 ?M) (PAST ?KN2) 
(KNOW-OF ?KN2) (OBJ ?KN2 ?HER) 
(PRO ?HER (FEMALE ?HER))). 

The final example appears to be unambiguous, but the parser finds four separate 
parses. The first is the obvious interpretation where the looking up is done quickly, 
and the second has quickly modifying the surprise. The last two interpretations are 
the same as the first two; they are artifacts of the search process. A disambiguation 
procedure should be equipped to weed out such duplicates. 

<a id='page-748'></a>

> (try s That Kim looked her up quickly surprised me) 

?SEM = (AND (THE ?K (NAME KIM ?K)) (AGT ?LU1 ?K) (PAST ?LU1) 
(LOOK-UP ?LU1) (PAT ?LU1 ?H) (PRO ?H (FEMALE ?H)) 
(QUICKLY ?LU1) (CON ?S ?LU1) (PAST ?S) (SURPRISE ?S) 
(EXP ?S ?ME1) (PRO ?ME1 (SPEAKER ?ME1))); 

?SEM = (AND (THE ?K (NAME KIM ?K)) (AGT ?LU2 ?K) (PAST ?LU2) 
(LOOK-UP ?LU2) (PAT ?LU2 ?H) (PRO ?H (FEMALE ?H)) 
(CON ?S ?LU2) (QUICKLY ?S) (PAST ?S) (SURPRISE ?S) 
(EXP ?S ?ME2) (PRO ?ME2 (SPEAKER ?ME2))); 

?SEM = (AND (THE ?K (NAME KIM ?K)) (AGT ?LU3 ?K) (PAST ?LU3) 
(LOOK-UP ?LU3) (PAT ?LU3 ?H) (PRO ?H (FEMALE ?H)) 
(QUICKLY ?LU3) (CON ?S ?LU3) (PAST ?S) (SURPRISE ?S) 
(EXP ?S ?ME3) (PRO ?ME3 (SPEAKER ?ME3))); 

?SEM = (AND (THE ?K (NAME KIM ?K)) (AGT ?LU4 ?K) (PAST ?LU4) 
(LOOK-UP ?LU4) (PAT ?LU4 ?H) (PRO ?H (FEMALE ?H)) 
(CON ?S ?LU4) (QUICKLY ?S) (PAST ?S) (SURPRISE ?S) 
(EXP ?S ?ME4) (PRO ?ME4 (SPEAKER ?ME4))): 

21.15 History and References 
Chapter 20 provides some basic references on natural language. Here we will concentrate 
on references that provide: 

1. A comprehensive grammar of English. 
2. A complete implementation. 
There are a few good textbooks that partially address both issues. Both Winograd 
(1983) and Allen (1987) do a good job of presenting the major grammatical features of 
English and discuss implementation techniques, but they do not provide actual code. 

There are also a few textbooks that concentrate on the second issue. Ramsey and 
Barrett (1987) and Walker et al. (1990) provide chapter-length implementations at 
about the same level of detail as this chapter. Both are recommended. Pereira and 
Shieber 1987 and Gazdar and Mellish 1989 are book-length treatments, but because 
they cover a variety of parsing techniques rather than concentrating on one in depth, 
they are actually less comprehensive. 

Several linguists have made serious attempts at addressing the first issue. The 
largest is the aptly namedA Comprehensive Grammar of Contemporary English by Quirk, 
Greenbaum, Leech and Svartik (1985). More manageable (although hardly concise) 
is their abridged edition, A Concise Grammar of Contemporary English. Both editions 
contain a gold mine of examples and facts about the English langauge, but the authors 

<a id='page-749'></a>
do not attempt to write rigorous rules. Harris (1982) and Huddleston (1984) offer 
less complete grammars with greater linguistic rigor. 

Naomi Sager (1981) presents the most complete computerized grammar ever 
published. The grammar is separated into a simple, neat, context-free component 
and a rather baroque augmentation that manipulates features. 

21.16 Exercises 
&#9635; Exercise 21.1 [m] Change the grammar to account better for mass nouns. The current 
grammar treats mass nouns by making them vague between singular and plural, 
which is incorrect. They should be treated separately, since there are determiners 
such as "much" that work only with mass nouns, and other determiners such as 
"these" that work only with plural count nouns. 

&#9635; Exercise 21.2 [m] Change the grammar to make a distinction between attributive 
and predicative adjectives. Most adjectives fall into both classes, but some can be used 
only attributively, as in "an utter fool" but not" * the fool is utter." Other adjectives can 
only be used predicatively, as in "the woman was loath to admit it" but not "*a loath 
(to admit it) woman." 

&#9635; Exercise 21.3 Pi] Implement complement lists for adjectives, so that "loath" would 
take an obligatory infinitive complement, and "proud" would take an optional (PP 
of) complement. In connection to the previous exercise, note that it is rare if not 
impossible for attributive adjectives to take complements: "he is proud," "he is proud 
of his country" and "a proud citizen" are all acceptable, but "*a proud of his country 
citizen" is not. 

&#9635; Exercise 21.4 [m] Add rules to advp to allow for adverbs to modify other adverbs, 
as in "extremely likely" or "very strongly." 

&#9635; Exercise 21.5 [h] Allow adverbs to modify adjectives, as in "very good" or "really 
delicious." The syntax will be easy, but it is harder to get a reasonable semantics. 
While you're at it, make sure that you can handle adjectives with so-called nonintersective 
semantics. Some adjectives can be handled by intersective semantics: a red 
circle is something that is red and is a circle. But for other adjectives, this model 
does not work: a former senator is not something that is former and is a senator - 
former senator is not a senator at all. Similarly, a toy elephant is not an elephant. 

<a id='page-750'></a>

The semantics should be represented by something closer to ((toy elephant) ?x) 
rather than (and (toy ?x) (elephant ?x)). 
&#9635; Exercise 21.6 [m] Write a function that notices punctuation instead of ignoring it. 
It should work something like this: 
> (string->words "Who asked Lee. Kim and John?") 
(WHO ASKED LEE I.I KIM AND JOHN l?l ) 

&#9635; Exercise 21.7 [m] Change the grammar to allow optional punctuation marks at the 
end of sentences and before relative clauses. 

&#9635; Exercise 21.8 [m] Change the grammar to allow conjunction with more than two 
elements, using commas. Can these rules be generated automatically by conj rule? 

&#9635; Exercise 21.9 [h] Make a distinction between restrictive and nonrestrictive relative 
clauses. In "The truck that has 4-wheel drive costs $5000," the italicized relative clause 
is restrictive. It serves to identify the truck and thus would be part of the quantifier's 
restriction. The complete sentence might be interpreted as: 
(and (the ?x (and (truck ?x) (4-wheel-drive ?x))) 
(costs ?x $5000)) 
Contrast this to "The truck, which has 4-wheel drive, costs $5000." Here the relative 
clause is nonrestrictive and thus belongs outside the quantifier's restriction: 
(and (the ?x (truck ?x)) 
(4-wheel-drive ?x)(cost s ?x $5000)) 

