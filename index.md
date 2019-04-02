# 13.03.19

## Bsp 8
```
bruder_von(B,P) :-
	kind_von(B,E),
	kind_von(P,E),
	dif(B,P),
	männlich(B).
```

-> zeigt Lösungen zwei mal, falls Vater UND Mutter gleich sind

!= -> Fehler gegenüber der Referenzimplementierung

Wie wird ein fehlerhaftes Faktum lokalisiert -> Es findet sich eine Lösung bei einer negativen Zusicherung

## Bsp 13

### de_en
```
:- de_en(D,E).
de_en('Hallo', 'Hello').
de_en(handy,mobile_phone).
de_en(praktisch,handy).
```

### bin_text
bin_text

### staat_org
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
:/- würfel(W, false.
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
%I hope it's correct
liste([]).
liste(_X|Xs) :-
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

```
%2., 3., 4. & 7. sind Endlosableitung -> jeweils ein '&' zum Pfeil

%beim 2. Block überall A-F generieren und dann untersuchen, was sie produzieren
```

# 27.03.19

## Beispiel 29

Das Faktum hat keinen Einfluss auf Termination (und wird deswegen rausgestrichen).
Erste Variante und Variante A unterscheiden sich nicht.
Finden von Lösungen =/= Termination.

## Beispiel 30

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

## Beispiel 31
(Hier geht es nur darum zu erinnern, dass es eine Referenzimplementierung gibt, mehr hat er nicht dazu gesagt)
Fragen an die Referenzimplementierung stellen

## Beispiel 32

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

## Beispiel 33

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

:- regzugwegzyklenfrei_nach(Weg, Nach).
:/-& regzugwegzyklenfrei_nach(Weg, Nach), false. % Sollte jetzt terminieren können, weil keine Zyklen mehr sind

regzugwegzyklenfrei_nach(Weg, Nach) :-  % Sollte ja eigentlich terminieren, aber nicht wenn wir regzugweg_nach verwenden (weil regzugweg_nach nicht terminiert)!
	alleunterschiedlich(Weg),
	regzugweg_nach(Weg, Nach).

:- alleunterschiedlich(Weg). % Schauen ob alle Lösungen fair aufgezählt werden. Ja werden sie.

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

## Beispiel 36

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

## Beispiel 37

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
    "und Fleiß, ",
    "folgt ",
    wort2,
    " und Preiß".
```

# 03.04.2019
## Bsp 38