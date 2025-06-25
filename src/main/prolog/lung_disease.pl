% Szczegółowa baza wiedzy chorób płuc

% --- CHOROBY INFEKCYJNE I ZAPALNE ---

choroba(zapalenie_pluc, [
    objaw_kluczowy(goraczka, 0.9),
    objaw_kluczowy(kaszel, 0.9),
    objaw_kluczowy(dusznosc, 0.8),
    objaw(bol_w_klatce_piersiowej, 0.7),
    objaw(zmeczenie, 0.6),
    objaw(dreszcze, 0.7),
    objaw(poty, 0.6),
    objaw(splatanie_u_starszych, 0.5),
    czynnik_ryzyka(wiek_starszy, 0.6),
    czynnik_ryzyka(oslabiony_uklad_odpornosciowy, 0.7),
    czynnik_ryzyka(przewlekle_choroby_pluc, 0.6),
    czynnik_ryzyka(palenie_tytoniu, 0.5),
    objaw_wykluczajacy(brak_goraczki_przy_objawach_infekcyjnych)
]).

choroba(gruzlica, [
    objaw_kluczowy(przewlekly_kaszel, 0.9),
    objaw_kluczowy(goraczka, 0.8),
    objaw_kluczowy(pocenie_nocne, 0.9),
    objaw_kluczowy(utrata_masy_ciala, 0.8),
    objaw(kaszel_z_krwia, 0.7),
    objaw(zmeczenie, 0.7),
    objaw(bol_w_klatce_piersiowej, 0.6),
    czynnik_ryzyka(kontakt_z_chorym_na_gruzlice, 1.0),
    czynnik_ryzyka(oslabiony_uklad_odpornosciowy, 0.8),
    czynnik_ryzyka(niedozywienie, 0.6),
    objaw_wykluczajacy(dobra_odpowiedz_na_standardowe_antybiotyki)
]).

choroba(zapalenie_oskrzeli, [
    objaw_kluczowy(kaszel, 0.9),
    objaw(odkrztuszanie_plwociny, 0.8),
    objaw(zmeczenie, 0.6),
    objaw(bol_gardla, 0.5),
    objaw(lekka_goraczka, 0.5),
    objaw(dyskomfort_w_klatce_piersiowej, 0.6),
    czynnik_ryzyka(infekcja_wirusowa_gornych_drog_oddechowych, 0.8),
    czynnik_ryzyka(palenie_tytoniu, 0.7),
    czynnik_ryzyka(narazenie_na_drazniace_substancje, 0.6),
    objaw_wykluczajacy(wysoka_goraczka_powyzej_38_5C),
    objaw_wykluczajacy(znaczna_dusznosc_spoczynkowa)
]).

choroba(alergiczne_zapalenie_pluc, [
    objaw_kluczowy(dusznosc_po_ekspozycji, 0.9),
    objaw_kluczowy(suchy_kaszel, 0.8),
    objaw(goraczka, 0.7),
    objaw(dreszcze, 0.7),
    objaw(zmeczenie, 0.6),
    objaw(bole_miesni_stawow, 0.5),
    czynnik_ryzyka(narazenie_na_alergeny, 1.0),
    objaw_wykluczajacy(brak_zwiazku_objawow_z_ekspozycja)
]).

choroba(zapalenie_pluc_aspiracyjne, [
    objaw_kluczowy(kaszel_po_posilkach_lub_wymiotach, 0.9),
    objaw_kluczowy(dusznosc, 0.8),
    objaw(goraczka, 0.8),
    objaw(odkrztuszanie_plwociny_o_nieprzyjemnym_zapachu, 0.7),
    objaw(bol_w_klatce_piersiowej, 0.6),
    objaw(swistajacy_oddech, 0.5),
    czynnik_ryzyka(zaburzenia_polykania, 0.9),
    czynnik_ryzyka(obnizony_poziom_swiadomosci, 0.8),
    czynnik_ryzyka(refluks_zoladkowo_przelykowy_choroba, 0.6),
    czynnik_ryzyka(zla_higiena_jamy_ustnej, 0.5)
]).

% --- CHOROBY OBTURACYJNE ---

choroba(przewlekla_obturacyjna_choroba_pluc, [
    objaw_kluczowy(dusznosc_wysilkowa_postepujaca, 0.9),
    objaw_kluczowy(przewlekly_kaszel_produktywny, 0.8),
    objaw_kluczowy(swistajacy_oddech, 0.7),
    objaw(uczucie_sciskania_w_klatce, 0.6),
    objaw(zmeczenie, 0.6),
    objaw(czeste_infekcje_drog_oddechowych, 0.7),
    objaw(utrata_masy_ciala, 0.5),
    czynnik_ryzyka(palenie_tytoniu, 1.0),
    czynnik_ryzyka(dlugotrwale_narazenie_na_pyly_chemikalia, 0.7),
    czynnik_ryzyka(wiek_powyzej_40, 0.6),
    objaw_wykluczajacy(calkowita_odwracalnosc_obturacji_po_lekach_rozszerzajacych_oskrzela)
]).

choroba(astma, [
    objaw_kluczowy(swistajacy_oddech, 0.9),
    objaw_kluczowy(nawracajaca_dusznosc, 0.9),
    objaw_kluczowy(kaszel_nasilajacy_sie_w_nocy_lub_nad_ranem, 0.8),
    objaw(uczucie_sciskania_w_klatce, 0.7),
    czynnik_ryzyka(rodzinne_wystepowanie_astmy_lub_alergii, 0.8),
    czynnik_ryzyka(alergie_osobiste, 0.7),
    czynnik_ryzyka(narazenie_na_wyzwalacze, 0.7),
    objaw_wykluczajacy(goraczka_powyzej_38),
    objaw_wykluczajacy(brak_odpowiedzi_na_leki_rozszerzajace_oskrzela)
]).

choroba(bronchiektazje, [
    objaw_kluczowy(przewlekly_kaszel_z_codziennym_odkrztuszaniem_duzych_ilosci_plwociny, 1.0),
    objaw_kluczowy(nawracajace_infekcje_pluc, 0.9),
    objaw(dusznosc, 0.7),
    objaw(krwioplucie, 0.6),
    objaw(zmeczenie, 0.6),
    objaw(bol_w_klatce_piersiowej, 0.5),
    czynnik_ryzyka(przebyte_ciezkie_infekcje_plucne, 0.8),
    czynnik_ryzyka(mukowiscydoza, 0.7),
    czynnik_ryzyka(zaburzenia_odpornosci, 0.6)
]).

choroba(mukowiscydoza, [
    objaw_kluczowy(nawracajace_infekcje_pluc, 0.9),
    objaw_kluczowy(przewlekly_kaszel_z_gesta_lepka_plwocina, 0.9),
    objaw(dusznosc_postepujaca, 0.8),
    objaw(swistajacy_oddech, 0.7),
    objaw(trudnosci_w_przybieraniu_na_wadze, 0.8),
    objaw(polipy_nosa, 0.6),
    objaw(palce_paleczkowate, 0.7),
    czynnik_ryzyka(historia_rodzinna_mukowiscydozy, 1.0)
]).


% --- NOWOTWORY PŁUC ---

choroba(rak_pluc, [
    objaw_kluczowy(przewlekly_kaszel_lub_zmiana_charakteru_kaszlu, 0.9),
    objaw_kluczowy(kaszel_z_krwia, 0.8),
    objaw(dusznosc, 0.7),
    objaw(bol_w_klatce_piersiowej, 0.7),
    objaw(chrypka, 0.6),
    objaw(utrata_masy_ciala, 0.8),
    objaw(zmeczenie, 0.7),
    objaw(nawracajace_zapalenie_pluc_lub_oskrzeli, 0.6),
    czynnik_ryzyka(palenie_tytoniu, 1.0),
    czynnik_ryzyka(narazenie_na_bierne_palenie, 0.7),
    czynnik_ryzyka(narazenie_na_radon_azbest_inne_rakotworcze, 0.8),
    czynnik_ryzyka(historia_rodzinna_raka_pluc, 0.6),
    objaw_wykluczajacy(objawy_szybko_ustepujace_po_leczeniu_infekcji)
]).

choroba(rak_pluc_drobnokomorkowy, [
    objaw_kluczowy(szybko_postepujaca_dusznosc, 0.9),
    objaw_kluczowy(kaszel, 0.8),
    objaw(utrata_masy_ciala, 0.9),
    objaw(zmeczenie_znaczne, 0.8),
    objaw(chrypka, 0.7),
    objaw(bol_w_klatce_piersiowej, 0.7),
    objaw(objawy_paraneoplastyczne, 0.7),
    czynnik_ryzyka(palenie_tytoniu, 1.0),
    objaw_wykluczajacy(bardzo_powolny_rozwoj_objawow_przez_lata)
]).

choroba(rak_pluc_niedrobnokomorkowy, [
    objaw_kluczowy(przewlekly_kaszel_lub_zmiana_charakteru_kaszlu, 0.9),
    objaw_kluczowy(kaszel_z_krwia, 0.8),
    objaw(dusznosc, 0.7),
    objaw(bol_w_klatce_piersiowej, 0.7),
    objaw(utrata_masy_ciala, 0.7),
    objaw(chrypka, 0.6),
    czynnik_ryzyka(palenie_tytoniu, 0.9),
    czynnik_ryzyka(narazenie_na_radon_azbest_inne_rakotworcze, 0.8),
    czynnik_ryzyka(wiek_powyzej_50, 0.6),
    objaw_wykluczajacy(objawy_wskazujace_na_typowe_SCLC_np_szybkie_paraneoplastyczne)
]).

choroba(rak_mezotelioma, [
    objaw_kluczowy(dusznosc, 0.9),
    objaw_kluczowy(bol_w_klatce_piersiowej, 0.9),
    objaw(kaszel, 0.6),
    objaw(utrata_masy_ciala, 0.7),
    objaw(zmeczenie, 0.7),
    objaw(poty_nocne_lub_goraczka, 0.5),
    czynnik_ryzyka(narazenie_na_azbest, 1.0),
    objaw_wykluczajacy(brak_historii_narazenia_na_azbest_przy_typowych_objawach)
]).

% --- CHOROBY INTERSTycjalne I NACZYNIOWE ---

choroba(sarkoidoza, [
    objaw_kluczowy(suchy_kaszel, 0.8),
    objaw_kluczowy(dusznosc_stopniowo_narastajaca, 0.8),
    objaw(zmeczenie, 0.7),
    objaw(bol_w_klatce_piersiowej, 0.6),
    objaw(goraczka, 0.5),
    objaw(utrata_masy_ciala, 0.5),
    objaw(obrzek_wezlow_chlonnych, 0.7),
    objaw(zmiany_skorne_charakterystyczne, 0.6),
    czynnik_ryzyka(wiek_20_40_lat, 0.6),
    czynnik_ryzyka(pochodzenie_afroamerykanskie_skandynawskie, 0.6)
]).

choroba(idiopatyczne_wloknienie_pluc, [
    objaw_kluczowy(postepujaca_dusznosc_wysilkowa, 0.9),
    objaw_kluczowy(suchy_uporczywy_kaszel, 0.9),
    objaw_kluczowy(trzeszczenia_u_podstawy_pluc, 0.8),
    objaw(zmeczenie, 0.7),
    objaw(palce_paleczkowate, 0.7),
    objaw(utrata_masy_ciala, 0.6),
    czynnik_ryzyka(wiek_powyzej_60, 0.8),
    czynnik_ryzyka(palenie_tytoniu_w_anamnezie, 0.7),
    czynnik_ryzyka(refluks_zoladkowo_przelykowy_choroba, 0.5),
    objaw_wykluczajacy(znana_przyczyna_wloknienia_pluc)
]).

choroba(zwloknienie_pluc_azbestowe, [
    objaw_kluczowy(postepujaca_dusznosc, 0.9),
    objaw(suchy_kaszel_przewlekly, 0.8),
    objaw(bol_lub_ucisk_w_klatce_piersiowej, 0.7),
    objaw(trzeszczenia_w_plucach, 0.7),
    objaw(palce_paleczkowate, 0.6),
    czynnik_ryzyka(dlugotrwale_narazenie_na_azbest, 1.0),
    objaw_wykluczajacy(brak_historii_narazenia_na_azbest)
]).

choroba(pylica_pluc, [
    objaw_kluczowy(dusznosc_szczegolnie_podczas_wysilku, 0.9),
    objaw_kluczowy(przewlekly_kaszel, 0.8),
    objaw(zmeczenie, 0.6),
    objaw(bol_w_klatce_piersiowej, 0.6),
    czynnik_ryzyka(dlugotrwale_narazenie_na_pyl_mineralny, 1.0),
    objaw_wykluczajacy(brak_historii_narazenia_na_pyl_przemyslowy)
]).

choroba(zatorowosc_plucna, [
    objaw_kluczowy(nagla_dusznosc, 1.0),
    objaw_kluczowy(bol_w_klatce_piersiowej_nasilajacy_sie_przy_wdechu, 0.9),
    objaw(kaszel, 0.6),
    objaw(krwioplucie, 0.7),
    objaw(przyspieszony_oddech, 0.7),
    objaw(tachykardia, 0.8),
    objaw(omdlenie_lub_zaslabniecie, 0.7),
    czynnik_ryzyka(zakrzepica_zyl_glebokich_DVT, 1.0),
    czynnik_ryzyka(dlugotrwale_unieruchomienie, 0.8),
    czynnik_ryzyka(zabiegi_operacyjne_urazy, 0.7),
    czynnik_ryzyka(nowotwory_zlosliwe, 0.6),
    czynnik_ryzyka(stosowanie_doustnej_antykoncepcji_hormonalnej_terapii_zastepczej, 0.5),
    objaw_wykluczajacy(objawy_rozwijajace_sie_bardzo_powoli_przez_tygodnie_miesiace)
]).

choroba(nadcisnienie_plucne, [
    objaw_kluczowy(dusznosc_poczatkowo_wysilkowa_pozniej_spoczynkowa, 0.9),
    objaw(zmeczenie, 0.8),
    objaw(bol_w_klatce_piersiowej, 0.7),
    objaw(zawroty_glowy, 0.6),
    objaw(omdlenia, 0.7),
    objaw(obrzeki_konczyn_dolnych, 0.6),
    objaw(sinica, 0.5),
    czynnik_ryzyka(choroby_serca_lewej_komory, 0.7),
    czynnik_ryzyka(przewlekle_choroby_pluc_np_POChP_wloknienie, 0.7),
    czynnik_ryzyka(zatorowosc_plucna_przewlekla, 0.6),
    czynnik_ryzyka(niektore_leki_toksyny, 0.5)
]).

% --- INNE CHOROBY PŁUC ---

choroba(odma_oplucnowa, [
    objaw_kluczowy(nagly_ostry_bol_w_klatce_piersiowej_jednostronny, 1.0),
    objaw_kluczowy(nagla_dusznosc, 0.9),
    objaw(suchy_kaszel, 0.6),
    objaw(przyspieszony_oddech, 0.7),
    objaw(uczucie_napiecia_w_klatce_piersiowej, 0.7),
    czynnik_ryzyka(wysoki_szczuply_mlody_mezczyzna, 0.6),
    czynnik_ryzyka(choroby_pluc_np_POChP_mukowiscydoza, 0.7),
    czynnik_ryzyka(uraz_klatki_piersiowej, 0.8),
    czynnik_ryzyka(palenie_tytoniu, 0.5),
    objaw_wykluczajacy(goraczka_wysoka)
]).

choroba(niedodma_pluc, [
    objaw(dusznosc_jesli_rozlegla, 0.8),
    objaw(kaszel, 0.6),
    objaw(bol_w_klatce_piersiowej_rzadko, 0.5),
    objaw(przyspieszony_oddech, 0.7),
    objaw(slabe_szmery_oddechowe_nad_obszarem, 0.9),
    czynnik_ryzyka(unieruchomienie_po_operacji, 0.9),
    czynnik_ryzyka(zablokowanie_drzewa_oskrzelowego, 0.8),
    czynnik_ryzyka(ucisk_na_pluco_z_zewnatrz, 0.7),
    objaw(sinica_w_ciezkich_przypadkach, 0.7)
]).

choroba(obrzek_pluc, [
    objaw_kluczowy(skrajna_dusznosc_nasilajaca_sie_w_pozycji_lezacej, 1.0),
    objaw_kluczowy(kaszel_z_rozowa_pienista_plwocina, 0.9),
    objaw(uczucie_duszenia_sie_topienia, 0.8),
    objaw(swistajacy_oddech_lub_rzezenia, 0.7),
    objaw(niepokoj_lek, 0.6),
    objaw(zimna_spocona_skora, 0.6),
    objaw(tachykardia, 0.7),
    czynnik_ryzyka(choroby_serca_niewydolnosc_serca, 1.0),
    czynnik_ryzyka(wysokie_cisnienie_krwi, 0.7),
    czynnik_ryzyka(uszkodzenie_pluc_np_ARDS_infekcja, 0.6),
    objaw_wykluczajacy(objawy_zlokalizowane_wylacznie_poza_ukladem_oddechowym_i_krazenia)
]).

choroba(krwioplucie, [
    objaw_kluczowy(odkrztuszanie_krwi_lub_plwociny_z_krwia, 1.0),
    objaw(kaszel_z_krwia, 1.0),
    objaw(odpluwanie_krwi, 1.0)
]).

% Nowy predykat do obliczania ważonego dopasowania
dopasowanie(Choroba, ObjawyPacjenta, CzynnikiRyzykaPacjenta, Dopasowanie) :-
    choroba(Choroba, WymaganeGlobal),

    findall(S_wykl, member(objaw_wykluczajacy(S_wykl), WymaganeGlobal), ListaWykluczajacych),
    (   ListaWykluczajacych = [] -> true;
        \+ (member(S_do_sprawdzenia, ListaWykluczajacych), member(S_do_sprawdzenia, ObjawyPacjenta))
    ),

    findall(S_klucz-W_klucz, member(objaw_kluczowy(S_klucz, W_klucz), WymaganeGlobal), ListaKluczowychZWagami),
    findall(S_klucz, member(S_klucz-_, ListaKluczowychZWagami), ListaNazwObjawowKluczowych),
    findall(S_klucz, (member(S_klucz, ListaNazwObjawowKluczowych), member(S_klucz, ObjawyPacjenta)), ObjawyKluczoweObecne),

    findall(S_reg-W_reg, member(objaw(S_reg, W_reg), WymaganeGlobal), ListaObjawowRegZWagami),
    findall(RF-W_rf, member(czynnik_ryzyka(RF, W_rf), WymaganeGlobal), ListaCzynnikowZWagami),

    findall(W_k, (member(S_k-W_k, ListaKluczowychZWagami), member(S_k, ObjawyPacjenta)), WagiPasujacychKluczowych),
    findall(W_r, (member(S_r-W_r, ListaObjawowRegZWagami), member(S_r, ObjawyPacjenta)), WagiPasujacychReg),
    append(WagiPasujacychKluczowych, WagiPasujacychReg, WagiPasujacychObjawowLista),
    sum_list(WagiPasujacychObjawowLista, SumaWagiObjawow),

    findall(W_rf_pas, (member(RF_pas-W_rf_pas, ListaCzynnikowZWagami), member(RF_pas, CzynnikiRyzykaPacjenta)), WagiPasujacychCzynnikowLista),
    sum_list(WagiPasujacychCzynnikowLista, SumaWagiCzynnikow),

    findall(W_k_all, member(_-W_k_all, ListaKluczowychZWagami), WszystkieWagiKluczowe),
    findall(W_r_all, member(_-W_r_all, ListaObjawowRegZWagami), WszystkieWagiReg),
    append(WszystkieWagiKluczowe, WszystkieWagiReg, WszystkieWagiObjawowAllLista),
    sum_list(WszystkieWagiObjawowAllLista, SumaWszystkichWagObjawow),
    
    findall(W_rf_all, member(_-W_rf_all, ListaCzynnikowZWagami), WszystkieWagiCzynnikowLista),
    sum_list(WszystkieWagiCzynnikowLista, SumaWszystkichWagCzynnikow),

    SumaWszystkichWag is SumaWszystkichWagObjawow + SumaWszystkichWagCzynnikow,
    SumaWagiPasujacych is SumaWagiObjawow + SumaWagiCzynnikow,

    length(ObjawyKluczoweObecne, LiczbaObjawowKluczowychObecnych),
    length(ListaNazwObjawowKluczowych, LiczbaWszystkichObjawowKluczowych),
    (LiczbaWszystkichObjawowKluczowych > 0 ->
        BonusKluczowy is (LiczbaObjawowKluczowychObecnych / LiczbaWszystkichObjawowKluczowych) * 20,
        Dopasowanie is min(100, (SumaWagiPasujacych / SumaWszystkichWag) * 80 + BonusKluczowy)
    ;
        Dopasowanie is (SumaWagiPasujacych / SumaWszystkichWag) * 100
    ).

sum_list([], 0).
sum_list([H|T], Sum) :-
    sum_list(T, Sum1),
    Sum is H + Sum1.

diagnozuj(Choroba, Objawy, CzynnikiRyzyka, Dopasowanie) :-
    dopasowanie(Choroba, Objawy, CzynnikiRyzyka, Dopasowanie),
    Dopasowanie > 0.

member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

:- dynamic objaw/1.
:- dynamic czynnik_ryzyka/1.

choroba_info(Choroba, Opis, Leczenie) :-
    choroba_opis(Choroba, Opis),
    choroba_leczenie(Choroba, Leczenie).

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