# Table of Contents

- [Table of Contents](#table-of-contents)
- [13.03.19](#130319)
	- [Bsp 8](#bsp-8)
	- [Bsp 11](#bsp-11)
	- [Bsp 12](#bsp-12)
	- [Bsp 13](#bsp-13)
	- [Bsp 14](#bsp-14)
	- [Bsp 15](#bsp-15)
	- [Bsp 16](#bsp-16)
	- [Bsp 17](#bsp-17)
	- [Bsp 18](#bsp-18)
	- [Bsp 19](#bsp-19)
- [20.03.19](#200319)
	- [Bsp 20](#bsp-20)
	- [Bsp 21](#bsp-21)
	- [Bsp 22](#bsp-22)
	- [Bsp 23](#bsp-23)
	- [Bsp 24](#bsp-24)
	- [Bsp 25](#bsp-25)
	- [Bsp 26](#bsp-26)
	- [Bsp 27](#bsp-27)
	- [Bsp 28](#bsp-28)
	- [Bsp 29](#bsp-29)
- [27.03.19](#270319)
	- [Bsp 30](#bsp-30)
	- [Bsp 31](#bsp-31)
	- [Bsp 32](#bsp-32)
	- [Bsp 33](#bsp-33)
	- [Bsp 34](#bsp-34)
	- [Bsp 35](#bsp-35)
	- [Bsp 36](#bsp-36)
	- [Bsp 37](#bsp-37)
- [03.04.19](#030419)
	- [Bsp 39](#bsp-39)
	- [Bsp 40](#bsp-40)
	- [Bsp 41](#bsp-41)
	- [Bsp 42](#bsp-42)
	- [Bsp 43](#bsp-43)
	- [Bsp 44](#bsp-44)
	- [Bsp 45](#bsp-45)
	- [Bsp 46](#bsp-46)
- [10.04.19](#100419)
	- [Bsp 47*](#bsp-47)
	- [Bsp 48](#bsp-48)
	- [Bsp 49](#bsp-49)
	- [Bsp 50](#bsp-50)
	- [Bsp 51](#bsp-51)
	- [Bsp 52](#bsp-52)
	- [Bsp 53](#bsp-53)
	- [Bsp 54](#bsp-54)
	- [Bsp 55](#bsp-55)
	- [Bsp 56](#bsp-56)
	- [Bsp 57*](#bsp-57)
- [08.05.19](#080519)
	- [Bsp 58](#bsp-58)
	- [Bsp 59](#bsp-59)
	- [Bsp 60](#bsp-60)
	- [Bsp 61](#bsp-61)
	- [Bsp 62-68](#bsp-62---68)
	- [Bsp 69](#bsp-69)
- [15.05.19](#150519)
	- [Bsp 63*](#bsp-63)
	- [Bsp 64](#bsp-64)
	- [Bsp 65](#bsp-65)
	- [Bsp 66](#bsp-66)
	- [Bsp 67](#bsp-67)
	- [Bsp 68](#bsp-68)
- [22.05.19](#220519)
	- [Bsp 62](#bsp-63)
	- [Bsp 70](#bsp-70)
	- [Bsp 71](#bsp-71)
	- [Bsp 72](#bsp-72)


# 13.03.19

## Bsp 8
```
:- bruder_von(B,P).
:/- bruder_von(B,B).

:- weiblich(B), bruder_von(B,P).
:/- weiblich(B), männlich(B).

:- bruder_von(marie_antoinette,marie_antoinette).
%(Error) Weil zu speziell, nur dif(B,P) bleibt übrig, nochmal space, männlich

bruder_von(B,P) :-
	kind_von(B,E),
	kind_von(P,E),
	dif(B,P),
	männlich(B).
```

-> zeigt Lösungen zwei mal, falls Vater UND Mutter gleich sind

!= -> Fehler gegenüber der Referenzimplementierung

Wie wird ein fehlerhaftes Faktum lokalisiert -> Es findet sich eine Lösung bei einer negativen Zusicherung

## Bsp 11
```
:-kind_vonvater_vonmutter(K,V,M).
:/-kind_vonvater_vonmutter(K,V,K).
:/-kind_vonvater_vonmutter(K,K,M).

kind_vonvater_vonmutter(K,V,M) :-
	kind_von(K,V),
	männlich(V),
	kind_von(K,M),
	weiblich(M).
```

## Bsp 12
```
:- kind_von(K,E),kind_von2(K,E).
kind_von2 ist teilmenge
	
kind_von2(K,E) :-
	kind_vonvater_vonmutter(K,E,_).
kind_von2(K,E) :-
	kind_vonvater_vonmutter(K,_,E).
```
## Bsp 13

*deutsch_englich*
```
deutsch_english(handy,cellphone).
deutsch_english(mobiltelefon,cellphone).
deutsch_english(handlich,handy).
deutsch_english(baby,baby).
:-deutsch_english(handy,E).
:-deutsch_english(D,cellphone).
:-deutsch_english(D,E).
:-deutsch_english(D,D).
```
oder
*de_en*
```
:- de_en(D,E).
de_en('Hallo', 'Hello').
de_en(handy,mobile_phone).
de_en(praktisch,handy).
```

*staat_org*
```
-> staat_org
   staat_gehört_zu
```

## Bsp 14

```
größer_als
kleiner_als
n_n_summe
zahlen_summe
funktion_variable_ableitung

9:unsortiert_aufsteigend
text_binärcode
```

Code aus alter Mitschrift (Vowi)
```
argumentreihenfolge
größer_kleiner(G,K).
add = zahl_zahl_zahl(X,Z,SUMME).
computesum([])... zahlenliste_summe
sortiere -> zahlenliste_aufsteigend
```

## Bsp 15

```
:- V = a.
:- V = s(0).
:- V = s(X1), X1=0.

:- V = f(V,g(a)).
:- V = f(V,X1), X1 = g(a).
:- V = f(V,X1), X1 = g(X2), X2 = a.
```

6-8
```
:- f(A,B)=f(C,D).
:- X=Y,X=f(A,B),Y=f(C,D).
```

## Bsp 16
```
:- U = g(a), U=g(b).
:-f(g(W),U) = f(V,g(a)).
```

## Bsp 17

```
:- natürlichezahlsx(X).
:- dif(X,0), natürlichezahlsx(X).
:/-& natürlichezahlsx(X), false.

natürlichezahlsx(0).
natürlichezahlsx(s(0)).
natürlichezahlsx(s(s(0))).

natürlichezahlsx(0).
natürlichezahlsx(s(X)) :-
	natürlichezahlsx(X).

:- geradesx(X).
:/-& geradesx(X),false.
:- X=0,geradesx(X).
:/-& geradesx(X),geradesx(s(X)). %findet trotzdem auch Fehler, obwohl es eine Endlosableitung ist!!!

geradesx(0).
geradesx(s(s(X))):-
	geradesx(X).

%geradesundungeradesx hilft wieder dabei, Fehler zu entdecken -> es sollte keine Zahl geben die gerade UND ungerade ist
```

## Bsp 18

```
%Wenn etwas terminieren soll, schreibt man eine negative Zusicherung mit einem false als letztes.
%Sollte es unendlich sein, meldet sich diese Zusicherung.
%Beispiel für einen Zyklus: A Kind von B, B Kind von C und C Kind von A
:- vorfahre_von(V,N).
:/- vorfahre_von(V,N), false.

vorfahre_von(V,N) :-
	kind_von(N,V).
vorfahre_von(V,N) :-
	kind_von(P,V),
	vorfahre_von(P,N).
```

## Bsp 19

```
:- vorfahre_von_abstand(V,N,X). %unendlich große Lösungsmenge, kann aber trotzdem terminieren (tut es in diesem Fall auch)
:- X = s(_), vorfahre_von_abstand(V,N,X). %endliche Lösungsmenge

vorfahre_von_abstand(P,P,0).
vorfahre_von_abstand(V,N,s(0)) :-
	kind_von(N,V).
vorfahre_von_abstand(V,N,S(X)) :-
	kind_von(P,V),
	vorfahre_von_abstand(P,N,X).
```

# 20.03.19

## Bsp 20

```
:- nat_nat_summe(A,B,C).
:/-& nat_nat_summe(A,B,C), false.

:- nat_nat_summe(s(0), s(0), C).

nat_nat_summe(0,A,A). %A müsste eigentlich als Zahl definiert werden, allerdings terminiert es dann nicht
nat_nat_summe(s(A),B,s(C)) :-
  nat_nat_summe(A,B,C).

e1(E) :-
  nat_nat_summe(X,E,X).

:- e1(E).
:/-& e1(E), false. %Nichttermination dokumentieren
:/-& dif(E,0), e1(E).

nat_nat_summe(E,X,X).
```

## Bsp 21

```
:- würfel(W).
:- W = w(1,1,1), würfel(W).
:- W = w(1,2,3), würfel(W).
:- W = w(1,4,2), würfel(W).
:- W = w(_,4,_), würfel(W).
:/- würfel(W, false).
:/- würfel(w(1,4,2)).
:/- würfel(w(_,4_,_)).

würfel(w(A,B,C)) :-
  holzart(A),
  holzart(B),
  holzart(C).

%ODER (Fehler vielleicht deutlich ersichtlicher)
würfel(W) :-
  W = w(A,B,C),
  holzart(A),
  holzart(B),
  holzart(C).

holzart(1).
holzart(2).
holzart(3).
```

## Bsp 22

```
:- Us = [V].
:- Us = [V|[]].

:- Us = [a,b,c,d].
:- Us = [a|[b,c,d]].
:- Us = [a,b|[c,d]].
:- Us = [a|[b|[c,d]]].
:- Us = [a|[b|[c,d|[]]]].
```

## Bsp 23

```
:- Us = [V].
:- Us = [V,X1], X1 = [].

:- Us = [a].
:- Us = [X1], X1 = a.
:- Us = [X1|X2], X1 = a, X2 = [].

:- Us = [a,b].
:- Us = [X1,b], X1 = a.
:- Us = [X1,X2], X1 = a, X2 = b.
:- Us = [X1|[X2]], X1 = a, X2 = b.
:- Us = [X1|X3], X1 = a, X2 = b, X3 = [X2|[]].
:- Us = [X1|X3], X1 = a, X2 = b, X3 = [X2|X4], X4 = [].
```

## Bsp 24

```
liste([]).
liste([_X|Xs]) :-
  liste(Xs).

:- parkettspalte(Ws) <<< liste(Ws), würfelliste(Ws).

:- würfelliste(Ws).
:/-& würfelliste(Ws), false. %Prüft auf Nichttermination -> sollte doch Termination auftreten beschwert sie sich

:- Ws = [_,_,_|_], würfelliste(Ws).
:/-& Ws = [_,_,_|_], würfelliste(Ws), false.

:- Ws = [_,_,_], würfelliste(Ws).
:/-$ Ws = [_,_,_], würfelliste(Ws), false.
:- Ws = [_,_,_,_,_], würfelliste(Ws).
:/-$ Ws = [_,_,_,_,_], würfelliste(Ws), false.

:- Ws = [w(1,1,1),w(1,1,1),w(1,1,1)], würfelliste(Ws).

%Probiert man ...
:- Ws = [w(4,1,1),w(1,1,1),w(1,1,1)], würfelliste(Ws).
%macht es das nicht nur zu
:- Ws = [w(4,_,_),_,_], würfelliste(Ws).
%sondern sogar zu:
:- Ws = [w(4,_,_)|_], würfelliste(Ws).

würfelliste([]).
würfelliste([W|Ws]) :-
  würfel(W),
  würfelliste(Ws).

%Wenn man die beiden Zeilen in Würfelliste vertauscht, werden die Lüsungen schüner, allerdings führt es zu einer Endlosableitung
```

## Bsp 25

```
:- allegleich(Xs).
:/-& allegleich(Xs), false.

:- Xs = [_,_,_|[]], allegleich(Xs).
:- Xs = [any0,any0,any0], allegleich(Xs).
:/- Xs = [any0,any1,any0], allegleich(Xs).
:/- Xs = [any0,any1|_], allegleich(Xs).
:/- dif(V0,V1), Xs = [V0,V1|_], allegleich(Xs).

:- Xs = [_,_,_|_], allegleich(Xs).
:/- Xs = [_,_,_], allegleich(Xs), false.

allegleich([]).
allegleich([E|Es]) :-
  e_es(E,Es),
  allegleich(Es).

e_es(_E,[]).
e_es(E, [E|_]).

%%%

:- allewürfelgleich(Ws).
:- parkettspalte(Ws) <<< allewürfelgleich(Ws).
:/-& allewürfelgleich(Ws), false.

:- Ws = [_,_,_,_,_], allewürfelgleich.
:/- Ws = [_,_,_,_,_], allewürfelgleich, false.

:- Ws = [w(1,1,1),w(1,1,1),w(1,1,1),w(1,1,1),w(1,1,1)], allewürfelgleich(Ws).
:/- dif(W1,W2), Ws = [W1,W2], allewürfelgleich(Ws).
:/- Ws = [w(1,1,1),w(2,1,1),w(1,1,1),w(1,1,1),w(1,1,1)], allewürfelgleich(Ws).
:/- Ws = dif(V0,V1), [w(V0,_,_),w(V1,_,_)], allewürfelgleich(Ws).

allewürfelgleich(Ws) :-
  allegleich(Ws),
  würfelliste(Ws).

%Wäre es besser, wenn man die beiden Zeilen in allewürfelgleich vertauscht?
%Lösung anschauen ->
```

## Bsp 26

nicht angesprochen

## Bsp 27

```
:- dwortteile(Ringe).
:- harsdörffer(Ringe) <<< dwortteile(Ringe). %Entgegen den Erwartungen, permutiert Prolog zuerst R5 und nicht R1
:- Ringe = ['Ab', 'A', a, b, thum], dwortteile(Ringe).

:- ring1(R1). %49 Lösungen

dwortteile([R1,R2,R3,R4,R5]) :-
  ring1(R1),
  ring2(R2),
  ring3(R3),
  ring4(R4),
  ring5(R5).
```

## Bsp 28

```
:- keinelement_von(X,Xs).
:/-& keinelement_von(X,Xs), false.

:- Xs = [_,_,_], keinelement_von(X,Xs).
:/-& Xs = [_,_,_], keinelement_von(X,Xs), false.

:- Xs = [any0,any1,any2], X = any3, keinelement_von(X,Xs).
:/- Xs = [X|_], keinelement_von(X,Xs).
:/- Xs = [_,X|_], keinelement_von(X,Xs).
:/- keinelement_von(X, [_|franz]).
:/- keinelement_von(X, franz).

%Beispiel für eine Liste
%keinelement_von([]).
%keinelement_von(X,[_E|Es]) :-
%  keinelement_von(X,Es).

%Implementierung
keinelement_von([]).
keinelement_von(X,[E|Es]) :-
  dif(X,E),
  keinelement_von(X,Es).

%%%

:- alleunterschiedlich(Es).
:/-& alleunterschiedlich(Es).

:- alleunterschiedlich([A,B,C]).
:/- alleunterschiedlich([A,B,C]).

:/- Es = [a,b,c,d,c], alleunterschiedlich(Es).
:/- Es = [_,_,V0,_,V0], alleunterschiedlich(Es).
```

## Bsp 29

2., 3., 4. & 7. sind Endlosableitung -> jeweils ein '&' zum Pfeil

Beim 2. Block überall A-F generieren und dann untersuchen, was sie produzieren

Das Faktum hat keinen Einfluss auf Termination (und wird deswegen rausgestrichen).
Erste Variante und Variante A unterscheiden sich nicht.
Finden von Lösungen `=/=` Termination.

# 27.03.19

## Bsp 30

```
:- nat_nat_nat_summe(A,B,C,D).
:/-& nat_nat_nat_summe(A,B,C,D), false.

:- nat_nat_nat_summe(A,B,C,s(0)).
%:/- nat_nat_nat_summe(A,B,C,s(0)), false.  <- Terminiert nicht mit der ersten Version!
%Niemand interessiert sich für das D obwohl das D eigentlich Termination garantieren könnte

:- nat_nat_nat_summe(s(0),s(0),s(0),N). <- Terminiert nicht mit der zweiten Version!
% A und B kommen nicht vor, obwohl diese für Termination sorgen könnte.
% D muss im ersten Ziel sein, A muss im ersten Ziel sein, damit es terminieren könnte
:/- nat_nat_nat_summe(s(0),s(0),s(0),N), false.

:/- nat_nat_nat_summe(0,0,0,s(N)).

% Version 1
%nat_nat_nat_summe(A,B,C,D) :-
	%nat_nat_summe(A,B,AB),
	%nat_nat_summe(AB,C,D).

% Version 2
%nat_nat_nat_summe(A,B,C,D) :-
	%nat_nat_summe(AB,C,D),
	%nat_nat_summe(A,B,AB).

%nat_nat_nat_summe(A,B,C,D) :-
	%nat_nat_summe(A,_,D), % <- geht das? Was kommt da rein?
	%nat_nat_summe(AB,C,D),
	%nat_nat_summe(A,B,AB).

nat_nat_nat_summe(A,B,C,D) :-
	nat_nat_summe(A,BC,D),
	nat_nat_summe(B,C,BC).

:- nat_nat_summe(s(0), BC, D).
@@ % D = s(BC).
```

## Bsp 31
(Hier geht es nur darum zu erinnern, dass es eine Referenzimplementierung gibt, mehr hat er nicht dazu gesagt)
Fragen an die Referenzimplementierung stellen

## Bsp 32

```
:- liste_gleichlang(Xs, Ys).
:/- liste_gleichlang(_, any0).
:/-& liste_gleichlang(Xs, Ys), false.

:- Xs = [_,_], liste_gleichlang(Xs,Ys).
:/- Xs = [_,_], liste_gleichlang(Xs,Ys), false.

:- Ys = [_,_], liste_gleichlang(Xs, Ys).
:/- Ys = [_,_], liste_gleichlang(Xs, Ys), false.

:/-& liste_gleichlang(Xs, [a|Xs]). % terminiert einfach nicht, schafft Prolog nicht
:/-& liste_gleichlang(Xs, [a|Xs]), false.

:- liste_gleichlang([a,b],[s(0),j_II]).
:/- liste_gleichlang([a],[s(0),j_II]).
:/- liste_gleichlang([_],[_,_|_]).
:/- liste_gleichlang([],[_|_].
:/- liste_gleichlang([a,b], [s(0)]).
:/- liste_gleichlang([_,_|_],[_]).
:/- liste_gleichlang([_|_],[]).
:/- liste_gleichlang([], [_|_]).

%~liste_gleichlang([], []).~
%liste_gleichlang([_|Xs], [_|Ys]) :- % fängt nicht Listen ungleicher Länge ab! siehe :- liste_gleichlang(Xs, [a|Xs]).
	%liste_gleichlang(Xs, Ys).

%liste_gleichlang(Xs, [a|Xs]) % Lösung wird jetzt zwar gefunden (also es terminiert), aber das wollen wir nicht. Fakten hinzufügen löst nicht unser Problem
%~liste_gleichlang([], []).~
%liste_gleichlang([_|Xs], [_|Ys]) :- % fängt nicht Listen ungleicher Länge ab! siehe :- liste_gleichlang(Xs, [a|Xs]).
	%liste_gleichlang(Xs, Ys).
	
%~liste_gleichlang(spezialterm, spezialterm)~ % hinzufügen von Termen verursacht nicht-termination
%~liste_gleichlang([], []).~
%liste_gleichlang([_|Xs], [_|Ys]) :- % fängt nicht Listen ungleicher Länge ab! siehe :- liste_gleichlang(Xs, [a|Xs]).
	%liste_gleichlang(Xs, Ys).

%sx(spezialterm) :-
	%sx(spezialterm).
%sx(X) :-
	%dif(X, spezialterm).
%:- liste_gleichlang(Xs, Ys), sx(Xs).

liste_gleichlang([], []).
liste_gleichlang([_|Xs], [_|Ys]) :-
	liste_gleichlang(Xs, Ys).

:- ous_gleichlangmit(OUs, Xs).
:/-& ous_gleichlangmit(OUs, Xs), false.

:- OUs = [_,_,_], ous_gleichlangmit(OUs, Xs).
:/-& OUs = [_,_,_], ous_gleichlangmit(OUs, Xs), false.

:- Xs = [_,_], OUs = [_,_,_], ous_gleichlangmit(OUs, Xs).
:/- Xs = [_,_], OUs = [_,_,_], ous_gleichlangmit(OUs, Xs), false.

:/- ous_gleichlangmit([o([1]), o([3,4]),o([3,4])],[_,_]).

:- ous_gleichlangmit([o([1,2]), o([3,4]),o([3,4])],[_,_]).
:/- ous_gleichlangmit([o([1,2]), o([4]),o([3,4])],[_,_]).
:/- ous_gleichlangmit([o([_,_]), o([_])|_],_).
:/-& ous_gleichlangmit([o([_,_|_]), o([_])|_],_). % Terminiert zwar nicht mit Version 1, ist aber ok, kann man zum terminieren bringen (hat er gemacht, siehe Version 2)

%%% Version 1 START (beste Version, laut Prof)
ous_gleichlangmit([],_Xs).
ous_gleichlangmit([OU|OUs], Xs) :-
	ou_(OU, Ys),
	liste_gleichlang(Ys, Xs),
	ous_gleichlangmit(OUs, Xs).

ou_(o(E), E).
ou_(u(E), E).
%%% Version 1 END

%%% Version 2, versaut wo anders, auch wenn das ursprüngliche Problem von 1 behoben worden ist
ous_gleichlangmit_term([],_Xs).
ous_gleichlangmit_term([OU], Xs) :-
	ou_(OU, Ys),
	liste_gleichlang(Ys, Xs).
ous_gleichlangmit_term([OU|OUs], Xs) :-
	OUs = [OU2|_],
	ou_(OU, Ys),
	ou_(OU2, Ys2),
	liste_gleichlang(Ys, Ys2),
	liste_gleichlang(Ys, Xs),
	ous_gleichlangmit(OUs, Xs).

% Es gibt immer Vor und Nachteile, wenn man Spezialfälle abfängt

:- ous_quadrat(OUs). % Aufzählen von den Lösungen ist nicht "fair"
:- liste(OUs), ous_quadrat(OUs). % Hier "faire" Aufzählung (Vergleiche Lösungen)
:/-& ous_quadrat(OUs), false.

:- OUs = [_,_,_], ous_quadrat(OUs).
:/- OUs = [_,_,_], ous_quadrat(OUs), false.

:- ous_quadrat([o([1,2]),o([3,4])]).
:/- ous_quadrat([o([1,2]),o([3])]).
:/- ous_quadrat([o([_,_]),o([_])|_]).

ous_quadrat(OUs) :-
	ous_gleichlangmit(OUs, OUs).

:- ous_abwechselnd(OUs).
:/-& ous_abwechselnd(OUs), false.
:- ous_abwechselnd([o(etwas),u(nochetwas)]).
:/- ous_abwechselnd([o(etwas),u(nochetwas)]).
:/- ous_abwechselnd([u(_)|_]).
:/- ous_abwechselnd([o(_),o(_)|_]).
:/- ous_abwechselnd([_,o(_)|_]).

:- OUs = [_,_,_], ous_abwechselnd(OUs).
:/- OUs = [_,_,_], ous_abwechselnd(OUs), false.

ous_abwechselnd([]).
ous_abwechselnd([o(_)|UOs]) :-
	uos_abwechselnd(UOs).

uos_abwechselnd([]).
uos_abwechselnd([u(_)|OUs]) :-
	ous_abwechselnd(OUs).

:- ous_mitwürfel123(OUs). % Wieder unfair
:- liste(OUs), ous_mitwürfel123(OUs). % Wieder unfair, bisschen "besser"
:/-& ous_mitwürfel123(OUs), false.
:- ous_mitwürfel123([u([1,2,3])]).
:/- ous_mitwürfel123([u([1,3,3])]).
:/- ous_mitwürfel123([u([_,3,_]|_)|_]).

:- OUs = [u([w(1,2,3)]),u([]),o([w(1,2,3),w(1,2,3)])], ous_mitwürfel123(OUs).
:- OUs = [u([W]),u([]),o([_,_])], ous_mitwürfel123(OUs).
:/- OUs = [u([W]),u([]),o([_,_])], ous_mitwürfel123(OUs), false.

ous_mitwürfel123([]).
ous_mitwürfel123([OU|OUs]) :-
	ou_(OU, Ws),
	allegleich([w(1,2,3)|Ws]),
	ous_mitwürfel123(OUs).

:- parkettfläche(Parkett).
:/-& parkettfläche(Parkett, false).
:- parkett(Parkett) <<< Parkett = [_,_|_], parkettfläche(Parkett).
% Die Anfrage terminiert zwar irgendwann nicht, aber das soll es ja auch nicht, also egal

:- Parkett = [_,_,_,_,_,_], parkettfläche(Parkett).
:/- Parkett = [_,_,_,_,_,_], parkettfläche(Parkett), false

parkettfläche(Parkett) :-
	ous_abwechselnd(Parkett),
	ous_quadrat(Parkett),
	ous_mitwürfel123(Parkett).

% ous_mitwürfel123 als erstes Ziel ist nicht gut -> Endlosableitungen

:- Parkett = [_,_,_,_,_,_], ous_quadrat(Parkett). % Hat viele Antworten, worunter unsere gesuchte ist
:- Parkett = [_,_,_,_,_,_], ous_abwechselnd(Parkett). % Hat weniger Antworten, deswegen bei Parkett ous_abwechselnd zuerst
:- Parkett = [_,_,_,_,_,_], ous_abwechselnd(Parkett), ous_quadrat(Parkett). % Eine Lösung
```

## Bsp 33

```
:- zugweg_nach(Städte, Nach).
:/-& zugweg_nach(Städte, Nach), false.
:- Städte = [_,_,_], zugweg_nach(Städte, Nach).
:/- Städte = [_,_,_], zugweg_nach(Städte, Nach), false.
:- Städte = [edinburg,aberdeen,edinburg], Nach = edinburg zugweg_nach(Städte, Nach).
:- streckenkarte(Städte) <<< Städte = [edinburg,aberdeen,edinburg], Nach = edinburg zugweg_nach(Städte, Nach).

:- streckenkarte(Weg) <<< Weg = [fina, _,_,_,_,_,_,_, marseille], zugweg_nach(Weg, marseille). % fina kann anders sein, walzerstadt bei sich selbst nachschauen
:/-& streckenkarte(Weg) <<< Weg = [fina, _,_,_,_,_,_,_, marseille], zugweg_nach(Weg, marseille), false.

:/- zugweg_nach([], N).
:/- dif(S,N), zugweg_nach([S], N).

:- streckevon_nach([szczecin], szczecin).

% Zusätzliche Zusicherungen (nicht nötig)
:- Städte = [_,_,_|_], zugweg_nach(Städte,Nach).
:- Städte = [_,_,_,_,_,_|_], zugweg_nach(Städte,Nach).
:- Städte = [endinburg,aberdeen,edinburg], zugweg_nach(Städte,Nach).

:/- Städte = [aberdeen], Nach=edinburg, zugweg_nach(Städte,Nach).
:/- Städte = [], zugweg_nach(Städte,Nach).

:- Städte = [_,_,_], zugweg_nach(Städte,Nach).
:/- Städte = [_,_,_], zugweg_nach(Städte,Nach),false.

:- Städte = [_,_,_,_,_,_,_], zugweg_nach(Städte,Nach).
:/-$ Städte = [_,_,_,_,_,_,_], zugweg_nach(Städte,Nach),false.


:- Städte = [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_], zugweg_nach(Städte,Nach).
:/-$ Städte = [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_], zugweg_nach(Städte,Nach),false.
%Zu groß


:/- Städte = [V0,V0|_],zugweg_nach(Städte,Nach).
:/- Städte = [x,edinburg], Nach= edinburg, zugweg_nach(Städte,Nach).

zugweg_nach([S], S) :-
	stadt(S).
zugweg_nach([S1|Städte], Nach) :-
	Städte = [S2|_],
	streckevon_nach(S1, S2),
	zugweg_nach(Städte, Nach).

:- regzugweg_nach(Weg, Nach).
:/-& regzugweg_nach(Weg, Nach), false.

:- Weg = [_,_,_], regzugweg_nach(Weg, Nach).
:/- Weg = [_,_,_], regzugweg_nach(Weg, Nach), false.

regzugweg_nach([S],S) :-
	stadt(S).
regzugweg_nach([S1|Städte], Nach) :-
	Städte = [S2|_],
	regstreckevon_nach(S1, S2),
	regzugweg_nach(Städte, Nach).
```

## Bsp 34

```
:- regzugwegzyklenfrei_nach(Weg, Nach).
:/-& regzugwegzyklenfrei_nach(Weg, Nach), false. % Sollte jetzt terminieren können, weil keine Zyklen mehr sind

regzugwegzyklenfrei_nach(Weg, Nach) :-  % Sollte ja eigentlich terminieren, aber nicht wenn wir regzugweg_nach verwenden (weil regzugweg_nach nicht terminiert)!
	alleunterschiedlich(Weg),
	regzugweg_nach(Weg, Nach).

:- alleunterschiedlich(Weg). % Schauen ob alle Lösungen fair aufgezählt werden. Ja werden sie.

```

## Bsp 35

```
:- regzugweg_nach_außer(Weg, Nach, Außer).
:/-& regzugweg_nach_außer(Weg, Nach, Außer), false.
:- regzugweg_nach_außer(Weg, Nach, []).
:/-& regzugweg_nach_außer(Weg, Nach, []), false.
:- Weg [_,_,_,_], regzugweg_nach_außer(Weg, Nach, []).
:/- Weg [_,_,_,_], regzugweg_nach_außer(Weg, Nach, []), false.

:/- regzugweg_nach_außer([_,_|_],_,[linz,munich,nürnberg,prague,salzburg,würzburg]).
% Wenn der Weg mindestens zwei Städte enthält (Walzerstadt + noch irgendeine andere), sollte es eigentlich nicht gehen.
%Hier werden alle Städte rausgenommen aus den Möglichkeiten (die Außer Liste), deswegen sollte es keine Lösung geben.

regzugweg_nach_außer([S], S, Außer) :-
	stadt(S),
	keinelement_von(S, Außer).
regzugweg_nach_außer([S1|Städte], Nach, Außer) :-
	Städte = [S2|_],
	keinelement_von(S1, Außer),
	regstreckevon_nach(S1, S2),
	regstreckevon_nach_außer(Städte, Nach, Außer).
```

## Bsp 36

```
:- regzugwegzyklenfrei_nach2(Weg, Nach).
:/- regzugwegzyklenfrei_nach2(Weg, Nach), false.
:/- Weg = [A,_,A], regzugwegzyklenfrei_nach2(Weg, Nach).

regzugwegzyklenfrei_nach2(Weg, Nach) :-
	regzugwegzyklenfrei_nach2_außer(Weg, Nach, []).

regzugwegzyklenfrei_nach2_außer([S], S, Außer) :-
	stadt(S),
	keinelement_von(S, Außer).
regzugwegzyklenfrei_nach2_außer([S1|Städte], Nach, Außer) :-
	Städte = [S2|_],
	keinelement_von(S1, Außer),
	regstreckevon_nach(S1, S2),
	regzugwegzyklenfrei_nach2_außer(Städte, Nach, [S1|Außer]).
%	keinelement_von(S1, Städte).
```

## Bsp 37

Erstes Argument ist das NichtTerminal und zweites ist die freie Variable.

```
:- phrase(lied, L)

% Gegenteil behaupten und anschauen was passiert
%:/- phrase(lied, L)
%In der Erklärung sieht man wie sich Lösungen ergeben

:- L = "abc", L = [a,b,c]
:- L = [a,b,c]
```

Hier sieht man, dass das Nichtterminal "lied" verschiedene Zeilen (Strings) von Sätzen erzeugt.

Wieso können in der Erklärung Teile des Programmtexte durchgestrichen werden?
Wir haben die Lösungsmenge reduziert und offensichtlich ist der reduzierte Teil wichtig für die Lösung

Welche Lesart wird hier angewandt?
Eine Spezialisierung der originalen Grammatik.

```
:- phrase(quatrain1, "hier bin im am Radar").
:- phrase(quatrain1, "wenn ich nicht hier bin, bin ich nicht hier").
:- phrase(quatrain1, "bin ich bin ich").
```

ad `:- phrase(quatrain1, "hier bin im am Radar").`
"hier bin ich am Radar" ist ein falscher Beginn für quatrain1.
Sternchen in Erklärung bedeutet "beliebige Sequenz".

ad `:- phrase(quatrain1, "wenn ich nicht hier bin, bin ich nicht hier").`
Die situation muss mit einem der vier Varianten enden, was hier nicht der Fall ist

ad `:- phrase(quatrain1, "bin ich bin ich").`
Beginnt nicht mit dem richtigen Text und endet nicht mit dem richtigen Text.

```
:- phrase(wort1, W).
:- phrase(wort2, W).

:- phrase(reim, Satz).
:/- Satz = "Auf Angst und Fleiß, folgt Lust und Preis", phrase(reim, Satz).
:/- Satz = "Auf Angst und Fleiß, folgt Lust und Preiß", phrase(reim, Satz).
```

Sätze aus wort1 und wort2 machen wie in der Angabe
Referenzimplemetierung verwenden (indem man `:/- phrase(reim, Satz)`) und es sich erklären lässt.

```
reim -->
    "Auf ",
    wort1,
    " und Fleiß, folgt ",
    wort2,
    " und Preiß".
```

# 03.04.19

## Bsp 39

```
:- phrase(expr,Xs).
:/-& phrase(expr,Xs), false.
:- phrase(expr,"(1+(1+1))").
:- phrase(expr,"((1+1)+1)").
:- phrase(expr,"1").
:/- phrase(expr,"2").
:/- phrase(expr,['2'|_]).
:- phrase(expr,"(1+1)").
:/- phrase(expr,"(1+1").
:/- phrase(expr,['(','1',+'1']).
:/- phrase(expr,['(','1',+'1'|_]).
:- phrase(expr,"(1*1)").
:- phrase(expr,['(','1',*,'1'|_]).
:/- phrase(expr,"(1+ 1)").

:- Xs=[_,_,_,_,_], phrase(expr,Xs);
:/- Xs=[_,_,_,_,_], phrase(expr,Xs), false;

:- text(Text) <<< list_length(Text,17), phrase(expr,Text).

:- dif(A,B), phrase(expr,[A,B|_]).
:/-& dif(A,B), phrase(expr,[A,B|_]), false.

expr -->
	"1".
expr -->
	"(",
	expr,
	"+",
	expr,
	")".
```

## Bsp 40


```
:- syntaxbaum(Baum) <<< is_expr(Baum).

is_expr(knoten(1,[])).
is_expr(knoten(+,[ExprL,ExprR])) :-
	is_expr(ExprL),
	is_expr(ExprR).

:- phrase(expr(knoten(+,[knoten(1,[]),knoten(1,[])])),"(1+1)").
:- syntaxbaum(Expr) <<< list_length(Text,N),phrase(expr(Expr),Text).

:- phrase(expr(Expr), Text).
:/- phrase(expr(Expr), Text), false.

:- Text="(1+(1+1))", phrase(expr(Expr),Text).
:- Expr=knoten(+,[knoten(1,[]),knoten(+,[knoten(1,[]),knoten(1,[])])]), phrase(expr(Expr),Text). %not sure if it's correct

expr(knoten(1,[])) -->
	"1".
expr(knoten(+,[ExprL,ExprR])) -->
	"(",
	expr(ExprL),
	"+",
	expr(ExprR),
	")".
```

Kann '(' und direkt danach ')' vorkommen?

```
:- list_length(Text,N), phrase(expr(Expr),Text), phrase(textwithbrackets, Text), false.

:/-& list_length(Text,N), phrase(expr(Expr),Text), phrase(textwithnilpair, Text).
:/-& list_length(Text,N), phrase(expr(Expr),Text), phrase(textwithnilpair, Text), false.

textwithnilpair -->
	...,
	"()",
	... .
	
textwithbrackets -->
	...,
	"))))",
	... .
```

## Bsp 41
```
expr_knotennamen(Expr, Knotennamen) :-
	phrase(knotennamen(Expr), Knotennamen).

knotennamen(knoten(N, [])) -->
	[N].
knotennamen(knoten(N, [ExprL,ExprR])) -->
	[N],
	knotennamen(ExprL),
	knotennamen(ExprR).

```

## Bsp 42

Wieder Zusicherungen schreiben, in denen nur Expr und nur Knotennamen vorkommt.
```
:- expr_knotennamenrpn(Expr,Knotennamen).
:/-& expr_knotennamenrpn(Expr,Knotennamen), false.

:- list_length(Knotennamen, N), expr_knotennamenrpn(Expr,Knotennamen).

:/-& phrase(knotennamenrpn(_),[]).

:- Knotennamen = [_,_|_], expr_knotennamenrpn(Expr, Knotennamen).
:/-& Knotennamen = [_,_|_], expr_knotennamenrpn(Expr, Knotennamen), false.
:- Knotennamen = [any0,any1,any2], Expr = knoten(any0,[knoten(any1,[]),knoten(any2,[])]), expr_knotennamenrpn(Expr, Knotennamen).

expr_knotennamenrpn(knoten(N,[])) -->
	[N].
expr_knotennamenrpn(knoten(N, [L,R])) -->
	knotennamen(R),
	knotennamen(L),
	[N].
```

## Bsp 43

```
:- expr_knotenname(Expr,Name).
:/- expr_knotenname(Expr,Name), false.
:- expr_knotenname(knoten(n,nonlist),Name).

:- Expr = knoten(any0,[knoten(any1,[]),knoten(any2,[])]), expr_knotenname(Expr, any0).

:- expr_knotenname(knoten(+, [knoten(1,[]), knoten(1,[])]),1).
:- expr_knotenname(knoten(+, [knoten(1,[]), knoten(1,[])]),+).
:- expr_knotenname(knoten(+, [knoten(1,[]), knoten(2,[])]),1).
:- expr_knotenname(knoten(+, [knoten(1,[]), knoten(1,[])]),2).

expr_knotenname(knoten(N,_),N).
expr_knotenname(knoten(_,[L,_]),N) :-
	expr_knotenname(L,N).
expr_knotenname(knoten(_,[_,R]),N) :-
	expr_knotenname(R,N).
```

## Bsp 44

```
:- phrase(seq(Cs), Es).
:/-& phrase(seq(Cs), Es), false.

:- Cs = [_,_,_], phrase(seq(Cs), Es).
:/- Cs = [_,_,_], phrase(seq(Cs), Es), false.

:/- Cs = "abc", Es = "xbc", phrase(seq(Cs),Es).
:/- dif(V0,V1), Cs = [V0|_], Es = [V1|_], phease(seq(Cs),Es).

:- phrase(seq("abc"), "abc").
:/- phrase(seq("abc"), "abe").

seq([]) -->
	[].
seq([E|Es]) -->
	[E],
	seq[Es].

:- phrase(invseq(Cs),Es).
:/-& phrase(invseq(Cs),Es), false.

:- Cs = [_,_,_], phrase(invseq(Cs), Es).
:/-& Cs = [_,_,_], phrase(invseq(Cs), Es), false.

invseq([]) -->
	[].
invseq([E|Es]) -->
	invseq(Es),
	[E].

:- invseq2(Xs,Ys).

invseq2p(Xs, Ys) :-
	phrase(invseq2(Xs,Ys),Ys).

invseq2([], _) -->
	[].
invseq2([E|Es], [_|L]) -->
	invseq(Es, L),
	[E].

:- phrase(palindrom, Xs).
:/-& phrase(palindrom, Xs), false.

:- Xs=[_,_,_,_,_], phrase(palindrom, Xs).

:- phrase(palindrom, "t").
:- phrase(palindrom, "tt").
:- phrase(palindrom, "otto").
:- phrase(palindrom, "tacocat").
:- phrase(palindrom, "stets").

```

Wenn man optional nicht einbaut, können nur Palindrome mit gerade Länge aufgezählt werden.

```
palindrom -->
	seq(Xs),
	optional,
	invseq(Xs).

:- phrase(optional,_).

optional -->
	[].
optional -->
	[_].
```

Versuchen wir es also ohne Hilfsprädikat.


```
:- phrase(palindrom2,Xs).

palindrom2 -->
	[].
palindrom2 -->
	[_].
palindrom2 -->
	[E],
	palindrom2,
	[E].
```

## Bsp 45

```
:- listen_zusammen(Xss, Es). %Xss ist eine Liste von Listen
:/-& listen_zusammen(Xss, Es), false.

:- Xss = [_,_,_], listen_zusammen(Xss, Es).
:/-& Xss = [_,_,_], listen_zusammen(Xss, Es), false.

:- Es = [_,_,_,_], Xss = [_,_,_], listen_zusammen(Xss, Es).
:/-& Es = [_,_,_,_], Xss = [_,_,_], listen_zusammen(Xss, Es), false.

:- listen_zusammen([[a|_]|_],[b|_]).
:- listen_zusammen([[1,2],[3],[4,5]],Es).

seqq([]) -->
	[].
seqq([Es|Ess]) -->
	seq(Es),
	seqq(Ess).

listen_zusammen(Xss, Es) :-
	phrase(seqq(Xss), Es).
```

## Bsp 46

Es sind bereits ein paar Zusicherungen gegeben ...

```
zahlenpaarD(P).

%zahlenpaarD(paar(X,Y)) :-
%	natürlichezahlsx(Z),
%	größer_als(Z,X),
%	größer_als(Z,Y).

zahlenpaarD(paar(X,Y)) :-
	natürlichezahlsx(Z),
	nat_nat_summe(X,Y,Z).

zahlenpaar(paar(X,Y)) :-
	natürlichezahlsx(X),
	natürlichezahlsx(Y).
```

# 10.04.19

## Bsp 47

*Bei diesem Beispiel handelt es sich um eine eigene Lösung und nicht um die Mitschrift aus der VO. Alle Angaben ohne Gewähr.*

```
:- geladen(^^40).

:- phrase(eingerückteexpr(Expr), E).
:/-& phrase(eingerückteexpr(Expr), E), false.

:- Expr = knoten(+,A), phrase(eingerückteexpr(Expr), E).
:/-& Expr = knoten(+,A), phrase(eingerückteexpr(Expr), E), false.
:- Expr = knoten(A,[]), phrase(eingerückteexpr(Expr), E).
:/- Expr = knoten(A,[]), phrase(eingerückteexpr(Expr), E), false.

:- phrase(eingerückteexpr(knoten(1,[])),"1").
:/- Text = "(1+1)",  text_eingerückt(Text,Text).
:- text(LayoutText) <<< Text = "(1+1)", dif(Text,LayoutText), text_eingerückt(Text,LayoutText).
:- text(LayoutText) <<< list_length(Text,N), text_eingerückt(Text,LayoutText).
:- text(LayoutText) <<< text_eingerückt("((1+1)+(1+1))", LayoutText).

text_eingrückt(Text,Eingrückt) :-
	phrase(expr(Expr), Text),
	phrase(eingrückteexpr(Expr), Eingrückt).

eingerückteexpr(E) -->
  eingerückteexprHelper(K, 0).

:- phrase(eingerückteexprHelper(K, E), Text).
:/-& phrase(eingerückteexprHelper(K, E), Text), false.

eingerückteexprHelper(knoten(1,[]), E) -->
  einrückung(E),
  "1".
eingerückteexprHelper(knoten(+,[A,B]), E) -->
  einrückung(E),
  "(",
  zeilenumbruch,
  einrückung(E),
  eingerückteexprHelper(A, s(E)),
	zeilenumbruch,
	einrückung(E),
	"+",
  zeilenumbruch,
  einrückung(E),
  eingerückteexprHelper(B, s(E)),
  zeilenumbruch,
  einrückung(E),
  ")".

:- phrase(zeilenumbruch, Layout).
:/- phrase(zeilenumbruch, Layout), false.
zeilenumbruch -->
  "\n".

:- phrase(einrückung(Ein), E).
:/-& phrase(einrückung(Ein), E), false.
einrückung(0) -->
  [].
einrückung(s(A)) -->
  einrückung(A),
  " ".

```
Dadurch, dass mit 53-56 begonnen wurde und 48-52 am Ende kam, sind 51&52 relativ kurz.

## Bsp 48

```
:- phrase(basen, Bs).
:/-& phrase(basen, Bs), false.

:- list_length(Bs, N), phrase(basen, Bs). %faires aufzählen

:- Bs = [_,_,_,_], phrase(basen, Bs).
:/- Bs = [_,_,_,_], phrase(basen, Bs), false.
:- Bs = [_,_,_,_,_,_,_,_], phrase(basen, Bs).
:/-$ Bs = [_,_,_,_,_,_,_,_], phrase(basen, Bs), false.

:- phrase(basen, "ACTAA").
:- phrase(basen, "ACXAA").
:- phrase(basen, [_,_,"X"|_]).

basen -->
	"".
basen -->
	base,
	basen.

base -->
	"A".
base -->
	"C".
base -->
	"G".
base -->
	"T".
```

## Bsp 49

```
:- phrase(tandemrepeat(Xs), Ys).
:/-& phrase(tandemrepeat(Xs), Ys), false.
:- Xs=[_,_,_,_], phrase(tandemrepeat(Xs), Ys).
:/-& Xs=[_,_,_,_], phrase(tandemrepeat(Xs), Ys), false.

:- Ys=[_,_,_,_], phrase(tandemrepeat(Xs), Ys).
:/-& Ys=[_,_,_,_], phrase(tandemrepeat(Xs), Ys), false.
:/-& Ys=[_,_,_,_,_], phrase(tandemrepeat(Xs), Ys), false.

:- phrase(tandemrepeat("ACT"), "ACTACT").
:/- phrase(tandemrepeat("CCT"), "ACTACT").
:/- dif(V0,V1), phrase(tandemrepeat([V0|_]), [V1|_]).
:/- phrase(tandemrepeat([]), Bs).

tandemrepeat(Xs) -->
	{Xs = [_|_]}, %Zusicherung innerhalb der Grammatik
	seq(Xs),
	seq(Xs).

:- phrase(substr_tandemrepeat(Xs), Ys).
:/-& phrase(substr_tandemrepeat(Xs), Ys), false.
:- phrase(substr_tandemrepeat(Xs), "GGGGACTACTCCCC").
:/- phrase(substr_tandemrepeat(Xs), "GGGGACTACTCCCC"), false.
:- phrase(substr_tandemrepeat("ACT"), ['G','G'|_]).
:/- phrase(substr_tandemrepeat("ACT"), ['G','G'|_]), false.

substr_tandemrepeat(Xs) -->
	seq(_),
	tandemrepeat(Xs),
	seq(_).

:- Xs=Xs, genom(G), phrase(substr_tandemrepeat(Xs), G).
:- Xs=[_,_,_,_,_,_], genom(G), phrase(substr_tandemrepeat(Xs),G).
:- Xs=[_,_,_,_,_,_|_], genom(G), phrase(substr_tandemrepeat(Xs),G).
:/- Xs=[_,_,_,_,_,_,_], genom(G), phrase(substr_tandemrepeat(Xs),G).
:-$ Xs=[_,_,_,_,_,_,_|_], genom(G), phrase(substr_tandemrepeat(Xs),G).
```

## Bsp 50

```
:- phrase(komplseq(Xs), Bs).
:/- phrase(komplseq(Xs), Bs), false.
:- phrase(komplseq(Xs), "CGCTAA").
:/- phrase(komplseq(Xs), "CGCTAA"), false.
:- phrase(komplseq("TTAGCG"), "CGCTAA").
:/- phrase(komplseq("TTAGCG"), "CGCTAA"), false.
:/- phrase(komplseq("TTGGCG"), "CGCTAA").

:- base_kompl(B, K).

base_kompl('A', 'T').
base_kompl('C', 'G').
base_kompl('G', 'C').
base_kompl('T', 'A').

komplseq([]) -->
	[].
komplseq([B|Bs]) -->
	{base_kompl(B,C)},
	komplseq(Bs),
	[C].

:- phrase(einfachschleife_(Bindung,Knick), Bs).
:/-& phrase(einfachschleife_(Bindung,Knick), Bs), false.
:- phrase(einfachschleife_(Bindung,Knick), "TTAGCGAAACGCTAA").
:/- phrase(einfachschleife_(Bindung,Knick), "TTAGCGAAACGCTAA"), false.
:- phrase(einfachschleife_("TTAGCG","AAA"), Bs).
:/- phrase(einfachschleife_("TTAGCG","AAA"), Bs), false.
:- phrase(einfachschleife_("TTAGCG","AAA"), "TTAGCGAAACGCTAA").
:/- phrase(einfachschleife_("TTAGCG","AAA"), "TTAGCGAAACGCTAA"), false.

einfachschleife_(Bindung, Knick) -->
	{Bindung = [_,_,_|_]},
	{Knick = [_,_,_|_]},
	seq(Bindung),
	seq(Knick),
	komplseq(Bindung).
```

## Bsp 51

```
:- rnafaltung(F) <<< ist_faltung(F).
:- rnafaltung(F) <<< F=[_,_|_], ist_faltung(F).
:- rnafaltung(F) <<< F=[_,_,_,_|_], ist_faltung(F).
:- rnafaltung(F) <<< F=[_,_,_,B|_], B=bindung("CCC",[_,_,_,_|_], ist_faltung(F)).
```

## Bsp 52

```
substr_einfachfaltung([frei(As),bindung(Bindung, [frei(Knick)]), frei(Bs)]) -->
	seq(As),
	einfachschleife_(Bindung, Knick),
	seq(Bs).
```

## Bsp 53

X soll größer gleich 0 sein

`:- X #>= 0.`

X soll größer gleich 0 und kleiner 9 sein

`:- X #>= 0, X #< 9.`

Der Rest erklärt sich dann mittlerweile von selbst

```
:- X #>= 0, X #< 9, X mod 2 #= 1.
:- X #>= 0, X #< 9, X mod 2 #= 1, X #>= 6.
:/- X #>= 0, X #< 9, X mod 2 #= 1, X #= 10.
```

Geforderte Punkte a-f:

```
:- X #= 1+1.

:- X+Y #= 2.

:- X+X #= 2.
```

Wenn man nicht einschränkt, dass die Anzahl der Hasen und Fasane >=0 sind, gibt es unendlich viele Lösungen:

```
:- F*2+H*4 #= 24, F+H #= 9.
:- F*2+H*4 #= 24, F+H #= 9, F #>= 0.
:- F*2+H*4 #= 24, F+H #= 9, H #>= 0.
:- F*2+H*4 #= 24, F+H #= 9, F #>= 0, H #>= 0.

:- X+Y #= 2, X+Y #= 3.
:- Fs = [A,B,C], Fs ins 0..1, A #\= B, A #\= C, B #\= C.
:/- Fs = [A,B,C], Fs ins 0..1, A #\= B, A #\= C, B #\= C, labeling_zs([], Fs).
```

Verschiedene Lösungsmengen obwohl es gleich aussieht..

```
:- X in 0..2.
:- X in 0..2, dif(X,1).
:- X in 0..2, X #\= 1.
```

## Bsp 54

```
:- keinelement_vonzs(X, Zs).
:/- keinelement_vonzs(X, Zs), false.

:- Zs = [_,_,_], keinelement_vonzs(X, Zs).
:- Zs = [4711,4711,4711], X=4712, keinelement_vonzs(X, Zs).
:/- Zs = [4712,4711,4711], X=4712, keinelement_vonzs(X, Zs).
:/- Zs = [4712|_], X=4712, keinelement_vonzs(X, Zs).

keinelement_vonzs(_X, []).
keinelement_vonzs(X, [Z|Zs]) :-
	X #\= Z,
	keinelement_vonzs(X, Zs).

:- zalleunterschiedlich(Zs).
:/-& zalleunterschiedlich(Zs), false.
:- Zs = [_,_,_,_], zalleunterschiedlich(Zs).

zalleunterschiedlich([]).
zalleunterschiedlich([Z|Zs]) :-
	keinelement_vonzs(Z, Zs),
	zalleunterschiedlich(Zs).

:- Zs = [_,_,_], Zs ins 0..1, zalleunterschiedlich(Zs). & Scheinlösung
```

```
:- Zs=[X|_], keinelement_vonzs(Zs,X). %Scheinlösung im Bezug auf Beispiel oben

:- X #>= 0, labeling_zs([], [X]). % Weigert sich, da unendlicher Wertebereich
```

## Bsp 55

```
buchstabe_wert('I',1).
buchstabe_wert('V',5).
buchstabe_wert('U',5).
buchstabe_wert('X',10).
buchstabe_wert('L',50).
buchstabe_wert('C',100).
buchstabe_wert('D',500).
buchstabe_wert('M',1000).
buchstabe_wert(Bs, 0) :-
	keinelement_von(Bs, "IVUXLCDM").

chronogramm_jahr([],0).
chronogramm_jahr([B|Bs], J) :-
	J #>= 0,
	buchstabe_wert(B, W),
	J #= J1+W,
	chronogramm_jahr(Bs, J1).
%Oder
chronogramm_jahr([B|Bs], J) :-
	buchstabe_wert(B, W),
	J #= J1+W,
	J1 #>= 0,
	chronogramm_jahr(Bs, J1).
```

## Bsp 56

```
:- hrätsel(Xs).
:/- hrätsel(Xs), false.

%  X1      X5
%  |       |
%  X2--X4--X6
%  |       |
%  X3      X7

:- hrätsel_(Xs, Zs).
:/-$ hrätsel_(Xs, Zs), false.

hrätsel(Xs) :-
	hrätsel_(Xs, Zs),
	labeling_zs([], Zs).

hrätsel_(Xs, Zs) :-
	Xs = [X1,X2,X3,X4,X5,X6,X7],
	Zs = [X1,X2,X3,X4,X5,X6,X7],
	Xs ins 1..7,
	zalleunterschiedlich(Xs),
	X1+X2+X3 #= S,
	X2+X4+X6 #= S,
	X5+X6+X7 #= S.

Xs = [A,B,C], Xs ins 1..3.
Xs = [A,B,C], Xs ins 1..3, keineelement_vonzs(2,Xs).
```

## Bsp 57

*Bei diesem Beispiel handelt es sich um eine eigene Lösung und nicht um die Mitschrift aus der VO. Alle Angaben ohne Gewähr.*

```
%Ja, man soll hier wirklich das '¡' verwenden
:-¡ phrase(hfigur(_),_).
:/-¡ phrase(hfigur(_),_), false.
:- Zs = [1,2,3,4,5,6,7], phrase(hfigur(Zs), Cs).
:/- Zs = [1,2,3,4,5,6,7], phrase(hfigur(Zs), Cs), false.
:- Zs = [1,2,3,4,5,6,7], Cs = "1     5\n|     |\n2--4--6\n3     7", phrase(hfigur(Zs), Cs). %Kann als Lösung von oben gewonnen werden

hfigur([X1,X2,X3,X4,X5,X6,X7]) -->
	integer_mitstellen(X1,0),
	integer_mitstellen(X5,6), %6 deshalb, weil die Ziffer zu diesen 6 Zeichen zählt | vergleichbar mit "padleft" aus vielen Programmiersprachen
	"\n|     |\n",
	integer_mitstellen(X2,0),
	"--",
	integer_mitstellen(X4,0),
	"--",
	integer_mitstellen(X6,0),
	"\n|     |\n",
	integer_mitstellen(X3,0),
	integer_mitstellen(X7,6),
```

# 08.05.19

## Bsp 58

Durch verneinen der vorgegebenen Kette ("CCCTTGTCGAT..") eine Lösung von der Referenzimplementierung geben lassen

Erkenntnisse: `bindung("CGA", [frei("TGG")])` ist erlaub, hingegen `bindung("CG", [frei("TGG")])` aber nicht, weil Bindung mindestens 3 Zeichen haben muss

```
%Einrückung nur wegen der Übersicht

tRNA(Descr) -->
	{Descr = [frei(F1), bindung(B1, Faltung), frei(F9)]},
	{Faltung = [frei(F2), bindung(B2, [frei(F3)]), frei(F4), bindung(B3, [frei(F5)]), frei(F6), bindung(B4, [frei(F7)]), frei(F8)]},
	{B1 = [_,_,_|_]},
	{B2 = [_,_,_|_]},
	{B3 = [_,_,_|_]},
	{B4 = [_,_,_|_]},
	{F3 = [_,_,_|_]},
	{F5 = [_,_,_,_,_|_]},
	{F7 = [_,_,_|_]},
	{Inner = [_,_,_,_,_, _,_,_,_,_, _,_,_,_,_, _,_,_,_,_, _,_,_,_,_, _,_,_,_|_]}, %Inner muss mindestens 29 Zeichen haben
	seq(F1),
	seq(B1),
	seq(Inner),
	komplseq(B1),
	{phrase(inner(F2,F3,F4,F5,F6,F7,F8,B2,B3,B4), Inner)},
	seq(F9).

inner(F2,F3,F4,F5,F6,F7,F8,B2,B3,B4) -->
	seq(F2),
	seq(B2),
		seq(F3),
	komplseq(B2),
	seq(F4),
	seq(B3),
		seq(F5), %Anticodon
	komplseq(B3),
	seq(F6),
	seq(B4),
		seq(F7),
	komplseq(B4),
	seq(F8),
```

Damit die vorgegebene Lösung (und die Verneinung) ohne $ funktionieren, war eine Korrektur bei komplseq notwendig, wo die Zeile in den geschwungenen Klammern in die mittlere Zeile verschoben wurde.

```
komplseq([B|Bs]) -->
	komplseq(Bs),
	{base_kompl(B,C)},
	[C].
```

## Bsp 59

Constraints (Kernsuche) -> kostet nichts ; kann prüfen, ob es überhaupt Lösungen gibt

Labeling (Tatsächliche Wertsuche) -> liefert Ergebnisse, kann aber auch sehr lang brauchen

```
:- magischesquadrat3(M).
:/- magischesquadrat3(M), false.

:- magquad(M) <<< magischesquadrat3(M).

:- magischesquadrat3_(M, Zs).
:/- magischesquadrat3_(M, Zs), false.

:/- M = [[1,2,_],_,_], magischesquadrat3_(M, Zs).
:- M = [[1,_,_],_,_], magischesquadrat3_(M, Zs). % Scheinlösung (nur vor der 1. Optimierung)
:/- M = [[1,_,_],_,_], magischesquadrat3_(M, Zs), labeling_zs([], Zs).
:- M = [[1,3,_],_,_], magischesquadrat3_(M, Zs). % Scheinlösung (nur vor der 1. Optimierung)
:/- M = [[1,3,_],_,_], magischesquadrat3_(M, Zs), labeling_zs([], Zs).

magischesquadrat3(M) :-
	magischesquadrat3_(M, Zs),
	labeling_zs([], Zs).

magischesquadrat3_(M, Zs) :-
	M = [[A1, A2, A3], [B1, B2, B3], [C1, C2, C3]],
	Zs = [A1, A2, A3, B1, B2, B3, C1, C2, C3],
	Es = [A1, A2, A3, B1, B2, B3, C1, C2, C3],
	Es ins 1..9,
	zalleunterschiedlich(Es),
	A1 mod 2 #= 0, % Optimierung 3
	A3 mod 2 #= 0, % Optimierung 3
	C1 mod 2 #= 0, % Optimierung 3
	C3 mod 2 #= 0, % Optimierung 3
	S #= 15, % Optimierung 1
	B2 #= 5, % Optimierung 2
	A1+A2+A3 #= S,
	B1+B2+B3 #= S,
	C1+C2+C3 #= S,
	A1+B1+C1 #= S,
	A2+B2+C2 #= S,
	A3+B3+C2 #= S,
	A1+B2+C3 #= S,
	A3+B2+C1 #= S.
```

Lösungen künstlich länger gestalten, um die Laufzeit zu messen: (wird bei der gemessenen Zeitangabe automatisch wieder weggerechnet)

```
:/-$ exptrue(10^1), magischesquadrat3(M).
:/-$ exptrue(10^2), magischesquadrat3(M).
:/-$ exptrue(10^3), magischesquadrat3(M).
```

## Bsp 60

```
:- magischesquadrat3nred(M).
:/- magischesquadrat3nred(M), false.
:- magischesquadrat3nred_(M, Zs).
:/- magischesquadrat3nred_(M, Zs), false.

:- exptrue(10^3), magischesquadrat3nred(M).

magischesquadrat3nred(M) :-
	magischesquadrat3nred_(M, Zs),
	labeling_zs([], Zs).

magischesquadrat3nred_(M, Zs) :-
	magischesquadrat3_(M, Zs), %Kernrelation verwenden, um auch hier das labeling auszulagern
	M = [[A1,_,A3],_,[C1,_,C3]],
	A1 #< A3,
	A1 #< C1,
	A1 #< C3,
	C1 #< A3.
```

## Bsp 61

```
:- liste_aufsteigend(Xs, Ys).
:/-& liste_aufsteigend(Xs, Ys), false.

:- liste_aufsteigend([3,2,1], [1,2,3]).
:/- liste_aufsteigend([3,2,1,1], [1,2,3]).
:/- liste_aufsteigend([_,_,_,_], [_,_,_]).
:- liste_aufsteigend([3,2,1,1], [1,1,2,3]).

:- L = [_,_,_], liste_aufsteigend(L, K).
:/- L = [_,_,_], liste_aufsteigend(L, K), false.
:- K = [_,_,_], liste_aufsteigend(L, K).
:/- K = [_,_,_], liste_aufsteigend(L, K), false.

liste_aufsteigend(L, Ls) :-
	liste_gleichlang(L, Ls),
	phrase(aufsteigend(L), Ls).

aufsteigend([]) -->
	[].
aufsteigend([X|Xs]) -->
	{es_x_klgls_grs(Xs, X, KlGls, Grs)},
	aufsteigend(KlGls),
	[X],
	aufsteigend(Grs).

es_x_klgls_grs([], _, [], []).
es_x_klgls_grs([E|Es], X, [E|KlGls], Grs) :-
	E #=< X,
	es_x_klgls_grs(Es, E, KlGls, Grs).
es_x_klgls_grs([E|Es], X, KlGls, [E|Grs]) :-
	E #> X,
	es_x_klgls_grs(Es, E, KlGls, Grs).
```

## Bsp 62 - 68

Ist am 08.05.2019 nicht behandelt worden ¯\_(ツ)_/¯

also ...

![nothing to do here](https://i.ibb.co/pvCG1Bx/Nothing-To-See-Here.png)

## Bsp 69

```
:- damen_brettgröße_(Ds, 8, Zs), labeling_zs([], Zs).
:- nqueens(Ds) <<< damen_brettgröße_(Ds, 8, Zs), labeling_zs([], Zs).
:-$ nqueens(Ds) <<< damen_brettgröße_(Ds, 40, Zs), labeling_zs([], Zs).
:-$ nqueens(Ds) <<< damen_brettgröße_(Ds, 20, Zs), labeling_zs([ff], Zs). %ff = first failure
:-$ nqueens(Ds) <<< damen_brettgröße_(Ds, 40, Zs), labeling_zs([], Zs).
:-$ nqueens(Ds) <<< damen_brettgröße_(Ds, 40, Zs), labeling_zs([ff], Zs). %findet mit ff sofort eine Lösung, ohne ff (mit klassischer Tiefensuche) kann man lange warten
:-$ nqueens(Ds) <<< damen_brettgröße_(Ds, 120, Zs), labeling_zs([ff], Zs).

:-$ nqueens(Ds) <<< damen_brettgröße_(Ds, 120, Zs), queenslabeling(Zs). %Gibt die dazugehörigen Sachen für queenslabeling verlinkt | seq und iseq (invseq) haben wir bereits selbst gemacht, den Rest muss man kopieren
```

# 15.05.19

## Bsp 63

*Bei diesem Beispiel handelt es sich um eine eigene Lösung und nicht um die Mitschrift aus der VO. Alle Angaben ohne Gewähr.*

```
:- phrase(papierfaltung(X), LRs).
:/- phrase(papierfaltung(X), LRs), false.
:- phrase(papierfaltung(s(s(s(X)))), LRs).
:/- phrase(papierfaltung(s(s(s(X)))), LRs), false.

:- lrs(LRs) <<< phrase(papierfaltung(s(0)), LRs).
:- lrs(LRs) <<< natürlichezahlsx(X), phrase(papierfaltung(X), LRs).

papierfaltung(s(0)) -->
	"r".
papierfaltung(s(X)) -->
	{phrase(papierfaltung(X), A)},
	A,
	"r",
	kompl(A).

:- phrase(kompl(A), B).
:- list_length(LRs, N), phrase(kompl(X), LRs).

kompl([]) -->
	"".
kompl([E|Es]) -->
	{faltung_kompl(E,F)},
	kompl(Es),
	[F].

faltung_kompl('l','r').
faltung_kompl('r','l').
```

## Bsp 64

```
:- is_task(T).
:/- is_task(T), false.

:- task_nichtüberschneidend(T1, T2).
:/- task_nichtüberschneidend(T1, T2), false.

task_nichtüberschneidend(T1, T2) :-
	T1 = task(_, B1, D1),
	T2 = task(_, B2, D2),
	D1 #>= 0,
	D2 #>= 0,
	B1 + D1 #=< B2.
task_nichtüberschneidend(T1, T2) :-
	T1 = task(_, B1, D1),
	T2 = task(_, B2, D2),
	D1 #>= 0,
	D2 #>= 0,
	B2 + D2 #=< B1.

:- [B1,D1,B2,D2] ins -3..3, T1 = task(a,B1,D1), T2 = task(b,B2,D2), task_nichtüberschneidend(T1,T2).
```

Man sieht: es gibt Redundanzen

Sie treten dann auf, wenn D1 und D2 0 sind und B1=B2 gilt

```
% Bei N=3 gibt es 8 (statt 6) Ansätze, weil *A vor B*, *B vor C* und *C vor A* z.B. als Scheinlösung sich hineingeschummelt hat
:- list_length(Ts, N), nichtgleichzeitigetasks(Ts).

nichtgleichzeitigetasks([]).
nichtgleichzeitigetasks([T|Ts]) :-
	task_nichtüberschneidend(Ts,T),
	nichtgleichzeitigetasks(Ts).

tasks_nichtüberschneidend([], S).
tasks_nichtüberschneidend([T|Ts], S) :-
	task_nichtüberschneidend(T, S),
	tasks_nichtüberschneidend(Ts, S).
```

## Bsp 65

```
:- ressourcenverwendung_plan(RV, Ts).
:/-& ressourcenverwendung_plan(RV, Ts), false.

:- Ts = [_,_,_], ressourcenverwendung_plan(RV, Ts).
:/-& Ts = [_,_,_], ressourcenverwendung_plan(RV, Ts), false.
:- RV = [_,_,_,_], Ts = [_,_,_], ressourcenverwendung_plan(RV, Ts).
:/-& RV = [_,_,_,_], Ts = [_,_,_], ressourcenverwendung_plan(RV, Ts), false.

ressourcenverwendung_plan([], _).
ressourcenverwendung_plan([_-Ns|RVs], Ts) :-
	namen_tasks_inplan(Ns, Tasks, Ts),
	nichtgleichzeitigetasks(Tasks),
	ressourcenverwendung_plan(RVs, Ts).

namen_tasks_inplan([], [], _).
namen_tasks_inplan([N|Ns], [Task|Tasks], Plan) :-
	Task = task(N, _, _),
	element_von(Task, Plan),
	namen_tasks_inplan(Ns, Tasks, Plan).

element_von(E, [E|_]).
element_von(E, [_|Es]) :-
	element_von(E, Es).
```

## Bsp 66

```
:- db_tasks(DB, Tasks).
:- db_resources(DB, Rs).

:- db_resplan_(DB, Plan, Zs).
:/-$ db_resplan_(DB, Plan, Zs), false.

:- plangraph(DB-Plan) <<< db_resplan_(DB, Plan, Zs), labeling_zs([], Zs).

:- tdds_tasks_bs(TDDs, Tasks, Bs).

tdds_tasks_bs([],[],[]).
tdds_tasks_bs([TDD|TDDs], [task(N,B,D)|Tasks], [B|Bs]) :-
	TDD = task_descr_duration(N, _, D),
	tdds_tasks_bs(TDDs, Tasks, Bs).

db_resplan_(DB, Plan, Zs) :-
	db_tasks(DB, TDDs),
	tdds_tasks_bs(TDDs, Plan, Zs),
	Zs ins 0..200,
	db_resources(DB, RVs),
	ressourcenverwendung_plan(RVs, Plan).


```

## Bsp 67

```
:- otyp_folgen_plan(OTyp, Folgen, Plan).
:/-& otyp_folgen_plan(OTyp, Folgen, Plan), false.
:- Folgen = [_], Plan = [_,_], otyp_folgen_plan(OTyp, Folgen, Plan).
:/- Folgen = [_], Plan = [_,_], otyp_folgen_plan(OTyp, Folgen, Plan), false.
:- Folgen = [_,_,_], Plan = [_,_,_,_], otyp_folgen_plan(OTyp, Folgen, Plan).
:/- Folgen = [_,_,_], Plan = [_,_,_,_], otyp_folgen_plan(OTyp, Folgen, Plan), false.

otyp_folgen_plan(_, [], _).
otyp_folgen_plan(OTyp, [Folge|Folgen], Plan) :-
	Folge = dist(N1,N2,D),
	Task1 = task(N1,_,_),
	Task2 = task(N2,_,_),
	element_von(Task1, Plan),
	element_von(Task2, Plan),
	otyp_t1_t2_d(Task1, Task2, D),
	otyp_folgen_plan(OTyp, Folgen, Plan).

otyp_t1_t2_d(M:EA1-EA2, task(_, B1, D1), task(_, B2, D2), D) :-
	ea_b_d_z(EA1, B1, D1, Z1),
	ea_b_d_z(EA2, B2, D2, Z2),
	m_z1_z2_e(M, Z1, Z2, 0).

m_z1_z2_e(min, Z1, Z2, D) :-
	Z2-Z1 #>= D.
m_z1_z2_e(max, Z1, Z2, D) :-
	Z2-Z1 #=< D.

ea_b_d_z(e, B, D, Z) :-
	z #= B+D
ea_b_d_z(a, B, _, B).


```

## Bsp 68

```
:- db_otyp_folgen(DB, OTyp, Folgen).
:- db_otyp_folgen(big, OTyp, Folgen).

:- db_plan_max_(DB, Plan, Max, Zs).
:/- db_plan_max_(DB, Plan, Max, Zs), false.

:-$ brückengraph(DB-Plan) <<< Max in 0..200, labeling_zs([], [Max]), db_plan_max_(DB, Plan, Max, Zs), labeling_zs([], Zs).

:-$ brückengraph(DB-Plan) <<< Max in 0..200, DB = big, labeling_zs([], [Max]), db_plan_max_(DB, Plan, Max, Zs), labeling_zs([], Zs).

db_plan_max_(DB, Plan, Max, Zs) :-
	db_tasks(DB, TDDs),
	tdds_tasks_bs(TDDs, Plan, Zs),
	Zs ins 0..200,
	db_resources(DB, RVs),
	max_ofzs(Max, Zs),
	db_otyp_plan(DB, min:a-a, Plan),
	db_otyp_plan(DB, min:a-e, Plan),
	db_otyp_plan(DB, min:e-a, Plan),
	db_otyp_plan(DB, min:e-e, Plan),
	db_otyp_plan(DB, max:a-a, Plan),
	db_otyp_plan(DB, max:a-e, Plan),
	db_otyp_plan(DB, max:e-a, Plan),
	db_otyp_plan(DB, max:e-e, Plan),
	ressourcenverwendung_plan(RVs, Plan).

db_otyp_plan(DB, OTyp, Plan) :-
	db_otyp_folgen(DB, OTyp, Folgen),
	otyp_folgen_plan(OTyp, Folgen, Plan).
```

# 22.05.19

## Bsp 62

```
ausdruck_liste(Expr, L) :-
	phrase(ausdruck(Expr), L).

ausdruck(Xs) -->
	seq(Xs).
ausdruck(?) -->
	[_].
ausdruck(X+_Y) -->
	ausdruck(X)
ausdruck(_X+Y) -->
	ausdruck(_Y)
ausdruck(X*Y) -->
	ausdruck(X),
	ausdruck(Y).
ausdruck({_X}) -->
	[].
ausdruck({X}) -->
	ausdruck(X),
	ausdruck({X}).
```

## Bsp 70

```
ist_matrix([]) :-
ist_matrix([Xs|Xss]) :-
	allegleichlang_mit(Xss, Xs).

allegleichlang_mit([], _).
allegleichlang_mit([Xs|Xss], Es) :-
	liste_gleichlang(Xs, Es),
	allegleichlang_mit(Xss, Es).
```

## Bsp 71

```
:- Xss = [_,_], matrix_transponiert(Xss, Yss). %2xn Matrix
:/-& Xss = [_,_], matrix_transponiert(Xss, Yss), false.

:- Xss = [_,_], Yss = [_], matrix_transponiert(Xss, Yss). %2x1 Matrix
:/- Xss = [_,_], Yss = [_], matrix_transponiert(Xss, Yss), false.

:- matrix_transponiert([[1,2,3],[4,5,6]], Yss).
:/- matrix_transponiert([[1,2,3],[4,5,6]], Yss), false.
:- matrix_transponiert(Xss, [[1,4],[2,5],[3,6]]).
:/- matrix_transponiert(Xss, [[1,4],[2,5],[3,6]]), false.

:- matrix_spalte_rest(Xss, Xs, Yss).
:/-& matrix_spalte_rest(Xss, Xs, Yss), false.

matrix_spalte_rest([], [], []).
matrix_spalte_rest([[E|Xs]|Xss], [E|Es], [Xs|Yss]) :-
	matrix_spalte_rest(Xss, Es, Yss).

matrix_transponiert(Xss, []) :-
	nils(Xss).
matrix_transponiert(Xss, [Es|XssT]) :- %um es korrekt zu machen, müsste man das Verpacken in eine andere Definition die prüft, ob beide Listen zumindest 1 Element haben
	matrix_spalte_rest(Xss, Es, Yss),
	matrix_transponiert(Yss, XssT).

nils([]).
nils([[]|Nils]) :-
	nils(Nils).
```

## Bsp 72

Wenn ich das richtig verstanden habe, ist dieses Beispiel ein einfacher Fragebogen.

Wer bis hier gelesen hat, hat sich noch ein besonderes Goodie verdient:

![potato](https://pics.me.me/sorry-for-the-long-post-heres-a-potato-14024311.png)
