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

% Nowe choroby płuc
choroba(sarkoidoza, [
    objaw(dusznosc),
    objaw(suchy_kaszel),
    objaw(zmeczenie),
    objaw(bol_w_klatce_piersiowej),
    objaw(obrzek_wezlow_chlonnych)
]).

choroba(idiopatyczne_wloknienie_pluc, [
    objaw(dusznosc),
    objaw(suchy_kaszel),
    objaw(zmeczenie),
    objaw(trzeszczenia_u_podstawy_pluc),
    czynnik_ryzyka(wiek_powyzej_50)
]).

choroba(rozedma_pluc, [
    objaw(dusznosc),
    objaw(zmeczenie),
    objaw(utrata_masy_ciala),
    objaw(kaszel),
    czynnik_ryzyka(palenie_tytoniu)
]).

choroba(zwloknienie_pluc_azbestowe, [
    objaw(dusznosc),
    objaw(suchy_kaszel),
    objaw(bol_w_klatce_piersiowej),
    czynnik_ryzyka(narazenie_na_azbestowe)
]).

choroba(pylica_pluc, [
    objaw(dusznosc),
    objaw(suchy_kaszel),
    objaw(bol_w_klatce_piersiowej),
    czynnik_ryzyka(zakazone_srodowisko_pracy)
]).

choroba(rak_mezotelioma, [
    objaw(dusznosc),
    objaw(bol_w_klatce_piersiowej),
    objaw(suchy_kaszel),
    objaw(zmeczenie),
    objaw(utrata_masy_ciala),
    czynnik_ryzyka(narazenie_na_azbestowe)
]).

choroba(niedodma_pluc, [
    objaw(dusznosc),
    objaw(kaszel),
    objaw(bol_w_klatce_piersiowej),
    objaw(sinica)
]).

choroba(krwioplucie, [
    objaw(kaszel_z_krwia),
    objaw(odpluwanie_krwi)
]).

choroba(zatorowosc_plucna, [
    objaw(dusznosc),
    objaw(bol_w_klatce_piersiowej),
    objaw(przyspieszony_oddech),
    objaw(kaszel),
    objaw(krwioplucie),
    czynnik_ryzyka(zakrzepica_zylna)
]).

choroba(obrzek_pluc, [
    objaw(dusznosc),
    objaw(kaszel_z_piana),
    objaw(trudnosci_w_oddychaniu),
    objaw(niepokój)
]).

choroba(rak_pluc_drobnokomorkowy, [
    objaw(kaszel),
    objaw(dusznosc),
    objaw(chrypka),
    objaw(utrata_masy_ciala),
    objaw(zmeczenie),
    czynnik_ryzyka(palenie_tytoniu)
]).

choroba(rak_pluc_niedrobnokomorkowy, [
    objaw(kaszel),
    objaw(kaszel_z_krwia),
    objaw(dusznosc),
    objaw(bol_w_klatce_piersiowej),
    objaw(chrypka),
    czynnik_ryzyka(palenie_tytoniu),
    czynnik_ryzyka(wiek_powyzej_50)
]).

choroba(mukowiscydoza, [
    objaw(mokry_kaszel),
    objaw(dusznosc),
    objaw(nawracajace_infekcje_pluc),
    objaw(trudnosci_w_przybieraniu_na_wadze)
]).

choroba(nadcisnienie_plucne, [
    objaw(dusznosc),
    objaw(zmeczenie),
    objaw(bol_w_klatce_piersiowej),
    objaw(zawroty_glowy),
    objaw(omdlenia)
]).

choroba(bronchiektazje, [
    objaw(mokry_kaszel),
    objaw(odkrztuszanie_duzych_ilosci_plwociny),
    objaw(zmeczenie),
    objaw(dusznosc),
    objaw(nawracajace_infekcje_pluc)
]).

choroba(zapalenie_pluc_aspiracyjne, [
    objaw(dusznosc),
    objaw(kaszel),
    objaw(goraczka),
    objaw(bol_w_klatce_piersiowej),
    czynnik_ryzyka(zaburzenia_polykania)
]).

% Dopasowanie choroby z podanymi objawami i czynnikami ryzyka
dopasowanie(Choroba, Objawy, CzynnikiRyzyka, Dopasowanie) :-
    choroba(Choroba, Wymagane),
    findall(X, (member(objaw(X), Wymagane)), WymaganeObjawy),
    findall(Y, (member(czynnik_ryzyka(Y), Wymagane)), WymaganeCzynniki),

    intersection(WymaganeObjawy, Objawy, PasujaceObjawy),
    intersection(WymaganeCzynniki, CzynnikiRyzyka, PasujaceCzynniki),
    
    length(WymaganeObjawy, LiczbaWymaganychObjawow),
    length(WymaganeCzynniki, LiczbaWymaganychCzynnikow),
    length(PasujaceObjawy, LiczbaPasujacychObjawow),
    length(PasujaceCzynniki, LiczbaPasujacychCzynnikow),

    LiczbaWymaganychTotal is LiczbaWymaganychObjawow + LiczbaWymaganychCzynnikow,
    LiczbaPasujacychTotal is LiczbaPasujacychObjawow + LiczbaPasujacychCzynnikow,

    (LiczbaWymaganychTotal > 0 ->
        Dopasowanie is (LiczbaPasujacychTotal / LiczbaWymaganychTotal) * 100;
        Dopasowanie is 0
    ).

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