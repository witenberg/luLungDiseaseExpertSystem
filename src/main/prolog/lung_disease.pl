% Szczegółowa baza wiedzy chorób płuc

% Definicje chorób i ich wymagań
choroba(zapalenie_pluc, [
    objaw(goraczka), 
    objaw(mokry_kaszel), 
    objaw(dusznosc), 
    objaw(bol_w_klatce_piersiowej)
]).

choroba(rak_pluc, [
    objaw(kaszel_z_krwia),
    objaw(utrata_masy_ciala), 
    objaw(trudnosci_w_oddychaniu),
    czynnik_ryzyka(palenie_tytoniu)
]).

choroba(przewlekla_obturacyjna_choroba_pluc, [
    objaw(dusznosc),
    objaw(swistajacy_oddech), 
    objaw(kaszel),
    czynnik_ryzyka(palenie_tytoniu)
]).

choroba(gruzlica, [
    objaw(goraczka), 
    objaw(pocenie_nocne),
    objaw(utrata_masy_ciala),
    objaw(kaszel)
]).

choroba(rak_oskrzeli, [
    objaw(chrypka),
    objaw(bol_w_klatce_piersiowej),
    objaw(kaszel_z_krwia),
    czynnik_ryzyka(narazenie_na_azbestowe)
]).

choroba(astma, [
    objaw(swistajacy_oddech),
    objaw(dusznosc),
    objaw(kaszel),
    objaw(bol_w_klatce_piersiowej)
]).

choroba(zapalenie_oskrzeli, [
    objaw(mokry_kaszel),
    objaw(dusznosc),
    objaw(goraczka),
    objaw(bol_w_klatce_piersiowej)
]).

choroba(odma_oplucnowa, [
    objaw(dusznosc),
    objaw(bol_w_klatce_piersiowej),
    objaw(szybkie_oddychanie)
]).

choroba(alergiczne_zapalenie_pluc, [
    objaw(dusznosc),
    objaw(kaszel),
    objaw(goraczka),
    czynnik_ryzyka(narazenie_na_alergeny)
]).

% Główny predykat diagnostyczny
diagnozuj(Choroba, Objawy, CzynnikiRyzyka) :-
    choroba(Choroba, Wymagane),
    findall(X, (member(objaw(X), Wymagane), member(X, Objawy)), PasujaceObjawy),
    findall(Y, (member(czynnik_ryzyka(Y), Wymagane), member(Y, CzynnikiRyzyka)), PasujaceCzynniki),

    PasujaceObjawy \= [],  % Musi być przynajmniej jeden pasujący objaw
    length(PasujaceObjawy, LO),
    length(Wymagane, LW),
    LO >= LW // 2.  % Warunek częściowego dopasowania (co najmniej połowa objawów i czynników)

% Pomocniczy predykat member
member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

% Dynamiczne definicje objawów i czynników (dodawane przez Java)
:- dynamic objaw/1.
:- dynamic czynnik_ryzyka/1.
