% Szczegółowa baza wiedzy chorób płuc

% Definicje chorób i ich wymagań z wagami
choroba(zapalenie_pluc, [
    objaw(goraczka, 0.8), 
    objaw(mokry_kaszel, 0.9), 
    objaw(dusznosc, 0.7), 
    objaw(bol_w_klatce_piersiowej, 0.6)
]).

choroba(rak_pluc, [
    objaw(kaszel_z_krwia, 1.0),
    objaw(utrata_masy_ciala, 0.8), 
    objaw(trudnosci_w_oddychaniu, 0.7),
    czynnik_ryzyka(palenie_tytoniu, 0.9)
]).

choroba(przewlekla_obturacyjna_choroba_pluc, [
    objaw(dusznosc, 0.9),
    objaw(swistajacy_oddech, 0.8), 
    objaw(kaszel, 0.7),
    czynnik_ryzyka(palenie_tytoniu, 0.9)
]).

choroba(gruzlica, [
    objaw(goraczka, 0.8), 
    objaw(pocenie_nocne, 0.7),
    objaw(utrata_masy_ciala, 0.8),
    objaw(kaszel, 0.9)
]).

choroba(rak_oskrzeli, [
    objaw(chrypka, 0.7),
    objaw(bol_w_klatce_piersiowej, 0.8),
    objaw(kaszel_z_krwia, 0.9),
    czynnik_ryzyka(narazenie_na_azbestowe, 0.8)
]).

choroba(astma, [
    objaw(swistajacy_oddech, 0.9),
    objaw(dusznosc, 0.8),
    objaw(kaszel, 0.7),
    objaw(bol_w_klatce_piersiowej, 0.6)
]).

choroba(zapalenie_oskrzeli, [
    objaw(mokry_kaszel, 0.9),
    objaw(dusznosc, 0.7),
    objaw(goraczka, 0.8),
    objaw(bol_w_klatce_piersiowej, 0.6)
]).

choroba(odma_oplucnowa, [
    objaw(dusznosc, 0.9),
    objaw(bol_w_klatce_piersiowej, 0.8),
    objaw(szybkie_oddychanie, 0.7)
]).

choroba(alergiczne_zapalenie_pluc, [
    objaw(dusznosc, 0.8),
    objaw(kaszel, 0.7),
    objaw(goraczka, 0.6),
    czynnik_ryzyka(narazenie_na_alergeny, 0.9)
]).

% Nowe choroby płuc
choroba(sarkoidoza, [
    objaw(dusznosc, 0.8),
    objaw(suchy_kaszel, 0.7),
    objaw(zmeczenie, 0.6),
    objaw(bol_w_klatce_piersiowej, 0.7),
    objaw(obrzek_wezlow_chlonnych, 0.8)
]).

choroba(idiopatyczne_wloknienie_pluc, [
    objaw(dusznosc, 0.9),
    objaw(suchy_kaszel, 0.8),
    objaw(zmeczenie, 0.7),
    objaw(trzeszczenia_u_podstawy_pluc, 0.8),
    czynnik_ryzyka(wiek_powyzej_50, 0.7)
]).

choroba(rozedma_pluc, [
    objaw(dusznosc, 0.9),
    objaw(zmeczenie, 0.7),
    objaw(utrata_masy_ciala, 0.6),
    objaw(kaszel, 0.8),
    czynnik_ryzyka(palenie_tytoniu, 0.9)
]).

choroba(zwloknienie_pluc_azbestowe, [
    objaw(dusznosc, 0.8),
    objaw(suchy_kaszel, 0.7),
    objaw(bol_w_klatce_piersiowej, 0.8),
    czynnik_ryzyka(narazenie_na_azbestowe, 0.9)
]).

choroba(pylica_pluc, [
    objaw(dusznosc, 0.8),
    objaw(suchy_kaszel, 0.7),
    objaw(bol_w_klatce_piersiowej, 0.7),
    czynnik_ryzyka(zakazone_srodowisko_pracy, 0.9)
]).

choroba(rak_mezotelioma, [
    objaw(dusznosc, 0.8),
    objaw(bol_w_klatce_piersiowej, 0.7),
    objaw(suchy_kaszel, 0.7),
    objaw(zmeczenie, 0.6),
    objaw(utrata_masy_ciala, 0.7),
    czynnik_ryzyka(narazenie_na_azbestowe, 0.9)
]).

choroba(niedodma_pluc, [
    objaw(dusznosc, 0.8),
    objaw(kaszel, 0.7),
    objaw(bol_w_klatce_piersiowej, 0.7),
    objaw(sinica, 0.8)
]).

choroba(krwioplucie, [
    objaw(kaszel_z_krwia, 0.9),
    objaw(odpluwanie_krwi, 0.9)
]).

choroba(zatorowosc_plucna, [
    objaw(dusznosc, 0.9),
    objaw(bol_w_klatce_piersiowej, 0.8),
    objaw(przyspieszony_oddech, 0.7),
    objaw(kaszel, 0.6),
    objaw(krwioplucie, 0.7),
    czynnik_ryzyka(zakrzepica_zylna, 0.8)
]).

choroba(obrzek_pluc, [
    objaw(dusznosc, 0.9),
    objaw(kaszel_z_piana, 0.8),
    objaw(trudnosci_w_oddychaniu, 0.8),
    objaw(niepokój, 0.6)
]).

choroba(rak_pluc_drobnokomorkowy, [
    objaw(kaszel, 0.7),
    objaw(dusznosc, 0.8),
    objaw(chrypka, 0.7),
    objaw(utrata_masy_ciala, 0.8),
    objaw(zmeczenie, 0.7),
    czynnik_ryzyka(palenie_tytoniu, 0.9)
]).

choroba(rak_pluc_niedrobnokomorkowy, [
    objaw(kaszel, 0.7),
    objaw(kaszel_z_krwia, 0.8),
    objaw(dusznosc, 0.8),
    objaw(bol_w_klatce_piersiowej, 0.7),
    objaw(chrypka, 0.7),
    czynnik_ryzyka(palenie_tytoniu, 0.9),
    czynnik_ryzyka(wiek_powyzej_50, 0.7)
]).

choroba(mukowiscydoza, [
    objaw(mokry_kaszel, 0.8),
    objaw(dusznosc, 0.7),
    objaw(nawracajace_infekcje_pluc, 0.9),
    objaw(trudnosci_w_przybieraniu_na_wadze, 0.8)
]).

choroba(nadcisnienie_plucne, [
    objaw(dusznosc, 0.9),
    objaw(zmeczenie, 0.7),
    objaw(bol_w_klatce_piersiowej, 0.8),
    objaw(zawroty_glowy, 0.6),
    objaw(omdlenia, 0.7)
]).

choroba(bronchiektazje, [
    objaw(mokry_kaszel, 0.9),
    objaw(odkrztuszanie_duzych_ilosci_plwociny, 0.8),
    objaw(zmeczenie, 0.7),
    objaw(dusznosc, 0.8),
    objaw(nawracajace_infekcje_pluc, 0.8)
]).

choroba(zapalenie_pluc_aspiracyjne, [
    objaw(dusznosc, 0.8),
    objaw(kaszel, 0.7),
    objaw(goraczka, 0.8),
    objaw(bol_w_klatce_piersiowej, 0.7),
    czynnik_ryzyka(zaburzenia_polykania, 0.9)
]).

% Nowy predykat do obliczania ważonego dopasowania
dopasowanie(Choroba, Objawy, CzynnikiRyzyka, Dopasowanie) :-
    choroba(Choroba, Wymagane),
    findall(X-W, (member(objaw(X, W), Wymagane)), WymaganeObjawyZWagami),
    findall(Y-W, (member(czynnik_ryzyka(Y, W), Wymagane)), WymaganeCzynnikiZWagami),

    % Obliczanie sumy wag dla pasujących objawów
    findall(W, (member(X-W, WymaganeObjawyZWagami), member(X, Objawy)), WagiPasujacychObjawow),
    sum_list(WagiPasujacychObjawow, SumaWagiObjawow),

    % Obliczanie sumy wag dla pasujących czynników ryzyka
    findall(W, (member(Y-W, WymaganeCzynnikiZWagami), member(Y, CzynnikiRyzyka)), WagiPasujacychCzynnikow),
    sum_list(WagiPasujacychCzynnikow, SumaWagiCzynnikow),

    % Obliczanie sumy wszystkich wymaganych wag
    findall(W, member(_-W, WymaganeObjawyZWagami), WszystkieWagiObjawow),
    findall(W, member(_-W, WymaganeCzynnikiZWagami), WszystkieWagiCzynnikow),
    sum_list(WszystkieWagiObjawow, SumaWszystkichWagObjawow),
    sum_list(WszystkieWagiCzynnikow, SumaWszystkichWagCzynnikow),

    % Obliczanie końcowego dopasowania
    SumaWszystkichWag is SumaWszystkichWagObjawow + SumaWszystkichWagCzynnikow,
    SumaWagiPasujacych is SumaWagiObjawow + SumaWagiCzynnikow,

    (SumaWszystkichWag > 0 ->
        Dopasowanie is (SumaWagiPasujacych / SumaWszystkichWag) * 100;
        Dopasowanie is 0
    ).

% Predykat pomocniczy do sumowania listy
sum_list([], 0).
sum_list([H|T], Sum) :-
    sum_list(T, Sum1),
    Sum is H + Sum1.

% Nowy predykat diagnostyczny
diagnozuj(Choroba, Objawy, CzynnikiRyzyka, Dopasowanie) :-
    dopasowanie(Choroba, Objawy, CzynnikiRyzyka, Dopasowanie),
    Dopasowanie > 0.  % Filtruje tylko te choroby, które mają jakiekolwiek dopasowanie.


% Pomocniczy predykat member
member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

% Dynamiczne definicje objawów i czynników (dodawane przez Java)
:- dynamic objaw/1.
:- dynamic czynnik_ryzyka/1.


choroba_info(Choroba, Opis, Leczenie) :-
    choroba_opis(Choroba, Opis),
    choroba_leczenie(Choroba, Leczenie).

% Opisy chorób dla mechanizmu wyjaśniającego
choroba_opis(zapalenie_pluc, "Zapalenie płuc to infekcja, która powoduje zapalenie pęcherzyków płucnych. Może być spowodowane przez bakterie, wirusy lub grzyby.").
choroba_opis(rak_pluc, "Rak płuc to nowotwór złośliwy charakteryzujący się niekontrolowanym wzrostem komórek w tkance płucnej. Jest to jeden z najczęstszych nowotworów na świecie.").
choroba_opis(przewlekla_obturacyjna_choroba_pluc, "Przewlekła obturacyjna choroba płuc (POChP) to postępująca choroba płuc charakteryzująca się trwałym ograniczeniem przepływu powietrza.").
choroba_opis(gruzlica, "Gruźlica to zakaźna choroba wywołana przez bakterię Mycobacterium tuberculosis, która najczęściej atakuje płuca.").
choroba_opis(rak_oskrzeli, "Rak oskrzeli to nowotwór złośliwy rozwijający się w drzewie oskrzelowym.").
choroba_opis(astma, "Astma to przewlekła choroba zapalna dróg oddechowych charakteryzująca się nawracającymi objawami, odwracalną obturacją dróg oddechowych i skurczem oskrzeli.").
choroba_opis(zapalenie_oskrzeli, "Zapalenie oskrzeli to stan zapalny błony śluzowej oskrzeli, często spowodowany infekcją.").
choroba_opis(odma_oplucnowa, "Odma opłucnowa to obecność powietrza w jamie opłucnej, która powoduje zapadnięcie się płuca.").
choroba_opis(alergiczne_zapalenie_pluc, "Alergiczne zapalenie płuc to reakcja immunologiczna płuc na alergeny wziewne.").
choroba_opis(sarkoidoza, "Sarkoidoza to choroba wielonarządowa charakteryzująca się tworzeniem się ziarniniaków, najczęściej w płucach i węzłach chłonnych.").
choroba_opis(idiopatyczne_wloknienie_pluc, "Idiopatyczne włóknienie płuc to przewlekła, postępująca choroba płuc o nieznanej przyczynie, charakteryzująca się bliznowaceniem tkanki płucnej.").
choroba_opis(rozedma_pluc, "Rozedma płuc to trwałe powiększenie przestrzeni powietrznych dystalnie od oskrzelików końcowych, z destrukcją ścian pęcherzyków płucnych.").
choroba_opis(zwloknienie_pluc_azbestowe, "Zwłóknienie płuc azbestowe to choroba płuc spowodowana wdychaniem włókien azbestu.").