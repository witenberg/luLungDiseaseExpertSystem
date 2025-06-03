// Klasa z danymi statycznymi
package main.java;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class DiagnosticData {
    // Listy objawów i czynników ryzyka
    public static final List<String> OBJAWY_ISTNIEJACE = Arrays.asList(
            "kaszel", "suchy_kaszel", "mokry_kaszel", "kaszel_z_krwia", "goraczka",
            "dusznosc", "bol_w_klatce_piersiowej", "zmeczenie", "utrata_masy_ciala",
            "odpluwanie_krwi", "chrypka", "pocenie_nocne", "trudnosci_w_oddychaniu",
            "swistajacy_oddech", "bol_plec", "szybkie_oddychanie", "przyspieszony_oddech",
            "obrzek_wezlow_chlonnych", "trzeszczenia_u_podstawy_pluc", "sinica",
            "kaszel_z_piana", "niepokój", "nawracajace_infekcje_pluc",
            "trudnosci_w_przybieraniu_na_wadze", "odkrztuszanie_duzych_ilosci_plwociny",
            "zawroty_glowy", "omdlenia", "krwioplucie"
    );

    public static final List<String> OBJAWY_NOWE = Arrays.asList(
            "dreszcze", "poty", "splatanie_u_starszych", "przewlekly_kaszel",
            "dobra_odpowiedz_na_standardowe_antybiotyki",
            "odkrztuszanie_plwociny", "bol_gardla", "lekka_goraczka", "dyskomfort_w_klatce_piersiowej",
            "wysoka_goraczka_powyzej_38_5C", "znaczna_dusznosc_spoczynkowa",
            "dusznosc_po_ekspozycji", "bole_miesni_stawow", "brak_zwiazku_objawow_z_ekspozycja",
            "kaszel_po_posilkach_lub_wymiotach", "odkrztuszanie_plwociny_o_nieprzyjemnym_zapachu",
            "dusznosc_wysilkowa_postepujaca", "przewlekly_kaszel_produktywny", "uczucie_sciskania_w_klatce",
            "czeste_infekcje_drog_oddechowych",
            "calkowita_odwracalnosc_obturacji_po_lekach_rozszerzajacych_oskrzela",
            "nawracajaca_dusznosc", "kaszel_nasilajacy_sie_w_nocy_lub_nad_ranem",
            "brak_odpowiedzi_na_leki_rozszerzajace_oskrzela",
            "przewlekly_kaszel_z_codziennym_odkrztuszaniem_duzych_ilosci_plwociny",
            "przewlekly_kaszel_z_gesta_lepka_plwocina", "dusznosc_postepujaca", "polipy_nosa", "palce_paleczkowate",
            "przewlekly_kaszel_lub_zmiana_charakteru_kaszlu", "nawracajace_zapalenie_pluc_lub_oskrzeli",
            "objawy_szybko_ustepujace_po_leczeniu_infekcji",
            "szybko_postepujaca_dusznosc", "zmeczenie_znaczne", "objawy_paraneoplastyczne",
            "bardzo_powolny_rozwoj_objawow_przez_lata",
            "objawy_wskazujace_na_typowe_SCLC_np_szybkie_paraneoplastyczne",
            "poty_nocne_lub_goraczka",
            "brak_historii_narazenia_na_azbest_przy_typowych_objawach",
            "dusznosc_stopniowo_narastajaca", "zmiany_skorne_charakterystyczne",
            "postepujaca_dusznosc_wysilkowa", "suchy_uporczywy_kaszel", "znana_przyczyna_wloknienia_pluc",
            "postepujaca_dusznosc", "suchy_kaszel_przewlekly", "bol_lub_ucisk_w_klatce_piersiowej", "trzeszczenia_w_plucach",
            "brak_historii_narazenia_na_azbest",
            "dusznosc_szczegolnie_podczas_wysilku", "brak_historii_narazenia_na_pyl_przemyslowy",
            "nagla_dusznosc", "bol_w_klatce_piersiowej_nasilajacy_sie_przy_wdechu", "tachykardia",
            "omdlenie_lub_zaslabniecie", "objawy_rozwijajace_sie_bardzo_powoli_przez_tygodnie_miesiace",
            "dusznosc_poczatkowo_wysilkowa_pozniej_spoczynkowa", "obrzeki_konczyn_dolnych",
            "nagly_ostry_bol_w_klatce_piersiowej_jednostronny", "uczucie_napiecia_w_klatce_piersiowej",
            "dusznosc_jesli_rozlegla", "bol_w_klatce_piersiowej_rzadko", "slabe_szmery_oddechowe_nad_obszarem",
            "sinica_w_ciezkich_przypadkach",
            "skrajna_dusznosc_nasilajaca_sie_w_pozycji_lezacej", "kaszel_z_rozowa_pienista_plwocina",
            "uczucie_duszenia_sie_topienia", "swistajacy_oddech_lub_rzężenia", "zimna_spocona_skora",
            "objawy_zlokalizowane_wylacznie_poza_ukladem_oddechowym_i_krążenia",
            "odkrztuszanie_krwi_lub_plwociny_z_krwia"
            // Dodatkowe dla rozedmy (jeśli byłaby odkomentowana)
            // "beczkowata_klatka_piersiowa", "dusznosc_znaczna", "wydluzony_wydech", "uzywanie_dodatkowych_miesni_oddechowych"
    );

    public static final List<String> OBJAWY = Stream.concat(OBJAWY_ISTNIEJACE.stream(), OBJAWY_NOWE.stream())
            .distinct()
            .collect(Collectors.toList());


    public static final List<String> CZYNNIKI_RYZYKA_ISTNIEJACE = Arrays.asList(
            "palenie_tytoniu", "przewlekle_zapalenie_oskrzeli",
            "zakazone_srodowisko_pracy", "obesity",
            "narazenie_na_alergeny", "zaburzenia_polykania"
    );

    public static final List<String> CZYNNIKI_RYZYKA_NOWE = Arrays.asList(
            "wiek_starszy", "oslabiony_uklad_odpornosciowy", "przewlekle_choroby_pluc",
            "kontakt_z_chorym_na_gruzlice", "niedozywienie",
            "infekcja_wirusowa_gornych_drog_oddechowych", "narazenie_na_drazniace_substancje",
            "refluks_zoladkowo_przelykowy_choroba", "zla_higiena_jamy_ustnej", "obnizony_poziom_swiadomosci",
            "dlugotrwale_narazenie_na_pyly_chemikalia", "wiek_powyzej_40",
            "rodzinne_wystepowanie_astmy_lub_alergii", "alergie_osobiste", "narazenie_na_wyzwalacze",
            "przebyte_ciezkie_infekcje_plucne", "zaburzenia_odpornosci",
            "historia_rodzinna_mukowiscydozy",
            "narazenie_na_bierne_palenie", "narazenie_na_radon_azbest_inne_rakotworcze", "historia_rodzinna_raka_pluc",
            "narazenie_na_azbest", // Zastępuje 'narazenie_na_azbestowe' dla spójności
            "wiek_20_40_lat", "pochodzenie_afroamerykanskie_skandynawskie",
            "wiek_powyzej_60", // Bardziej specyficzny niż wiek_powyzej_50 dla IPF
            "palenie_tytoniu_w_anamnezie",
            "dlugotrwale_narazenie_na_azbest",
            "dlugotrwale_narazenie_na_pyl_mineralny",
            "zakrzepica_zyl_glebokich_DVT", // Zastępuje 'zakrzepica_zylna'
            "dlugotrwale_unieruchomienie", "zabiegi_operacyjne_urazy", "nowotwory_zlosliwe",
            "stosowanie_doustnej_antykoncepcji_hormonalnej_terapii_zastepczej",
            "choroby_serca_lewej_komory", "przewlekle_choroby_pluc_np_POChP_wloknienie",
            "zatorowosc_plucna_przewlekla", "niektore_leki_toksyny",
            "wysoki_szczuply_mlody_mezczyzna", "choroby_pluc_np_POChP_mukowiscydoza", "uraz_klatki_piersiowej",
            "unieruchomienie_po_operacji", "zablokowanie_drzewa_oskrzelowego", "ucisk_na_pluco_z_zewnatrz",
            "choroby_serca_niewydolnosc_serca", "wysokie_cisnienie_krwi", "uszkodzenie_pluc_np_ARDS_infekcja"
            // Dodatkowe dla rozedmy (jeśli byłaby odkomentowana)
            // "niedobor_alfa_1_antytrypsyny"
    );

    public static final List<String> CZYNNIKI_RYZYKA = Stream.concat(CZYNNIKI_RYZYKA_ISTNIEJACE.stream(), CZYNNIKI_RYZYKA_NOWE.stream())
            .distinct()
            .collect(Collectors.toList());

    // Mapa tłumaczeń objawów na język polski (do wyjaśnień)
    public static String translateSymptom(String symptom) {
        switch(symptom) {
            // Istniejące
            case "kaszel": return "kaszel";
            case "suchy_kaszel": return "suchy kaszel";
            case "mokry_kaszel": return "mokry kaszel";
            case "kaszel_z_krwia": return "kaszel z krwią";
            case "goraczka": return "gorączka";
            case "dusznosc": return "duszność";
            case "bol_w_klatce_piersiowej": return "ból w klatce piersiowej";
            case "zmeczenie": return "zmęczenie";
            case "utrata_masy_ciala": return "utrata masy ciała";
            case "odpluwanie_krwi": return "odpluwanie krwi";
            case "chrypka": return "chrypka";
            case "pocenie_nocne": return "pocenie nocne";
            case "trudnosci_w_oddychaniu": return "trudności w oddychaniu";
            case "swistajacy_oddech": return "świszczący oddech";
            case "bol_plec": return "ból pleców"; // Poprawiona literówka
            case "szybkie_oddychanie": return "szybkie oddychanie";
            case "przyspieszony_oddech": return "przyspieszony oddech";
            case "obrzek_wezlow_chlonnych": return "obrzęk węzłów chłonnych";
            case "trzeszczenia_u_podstawy_pluc": return "trzeszczenia u podstawy płuc";
            case "sinica": return "sinica";
            case "kaszel_z_piana": return "kaszel z pianą";
            case "niepokój": return "niepokój";
            case "nawracajace_infekcje_pluc": return "nawracające infekcje płuc";
            case "trudnosci_w_przybieraniu_na_wadze": return "trudności w przybieraniu na wadze";
            case "odkrztuszanie_duzych_ilosci_plwociny": return "odkrztuszanie dużych ilości plwociny";
            case "zawroty_glowy": return "zawroty głowy";
            case "omdlenia": return "omdlenia";
            case "krwioplucie": return "krwioplucie";

            // Nowe
            case "dreszcze": return "dreszcze";
            case "poty": return "poty";
            case "splatanie_u_starszych": return "splątanie u osób starszych";
            case "przewlekly_kaszel": return "przewlekły kaszel";
            case "dobra_odpowiedz_na_standardowe_antybiotyki": return "dobra odpowiedź na standardowe antybiotyki";
            case "odkrztuszanie_plwociny": return "odkrztuszanie plwociny";
            case "bol_gardla": return "ból gardła";
            case "lekka_goraczka": return "lekka gorączka";
            case "dyskomfort_w_klatce_piersiowej": return "dyskomfort w klatce piersiowej";
            case "wysoka_goraczka_powyzej_38_5C": return "wysoka gorączka (powyżej 38.5°C)";
            case "znaczna_dusznosc_spoczynkowa": return "znaczna duszność spoczynkowa";
            case "dusznosc_po_ekspozycji": return "duszność po ekspozycji na czynnik";
            case "bole_miesni_stawow": return "bóle mięśni i stawów";
            case "brak_zwiazku_objawow_z_ekspozycja": return "brak związku objawów z ekspozycją na czynnik";
            case "kaszel_po_posilkach_lub_wymiotach": return "kaszel po posiłkach lub wymiotach";
            case "odkrztuszanie_plwociny_o_nieprzyjemnym_zapachu": return "odkrztuszanie plwociny o nieprzyjemnym zapachu";
            case "dusznosc_wysilkowa_postepujaca": return "duszność wysiłkowa postępująca";
            case "przewlekly_kaszel_produktywny": return "przewlekły kaszel produktywny (z odkrztuszaniem)";
            case "uczucie_sciskania_w_klatce": return "uczucie ściskania w klatce piersiowej";
            case "czeste_infekcje_drog_oddechowych": return "częste infekcje dróg oddechowych";
            case "calkowita_odwracalnosc_obturacji_po_lekach_rozszerzajacych_oskrzela": return "całkowita odwracalność obturacji po lekach rozszerzających oskrzela";
            case "nawracajaca_dusznosc": return "nawracająca duszność";
            case "kaszel_nasilajacy_sie_w_nocy_lub_nad_ranem": return "kaszel nasilający się w nocy lub nad ranem";
            case "brak_odpowiedzi_na_leki_rozszerzajace_oskrzela": return "brak odpowiedzi na leki rozszerzające oskrzela";
            case "przewlekly_kaszel_z_codziennym_odkrztuszaniem_duzych_ilosci_plwociny": return "przewlekły kaszel z codziennym odkrztuszaniem dużych ilości plwociny";
            case "przewlekly_kaszel_z_gesta_lepka_plwocina": return "przewlekły kaszel z gęstą, lepką plwociną";
            case "dusznosc_postepujaca": return "duszność postępująca";
            case "polipy_nosa": return "polipy nosa";
            case "palce_paleczkowate": return "palce pałeczkowate";
            case "przewlekly_kaszel_lub_zmiana_charakteru_kaszlu": return "przewlekły kaszel lub zmiana charakteru kaszlu";
            case "nawracajace_zapalenie_pluc_lub_oskrzeli": return "nawracające zapalenie płuc lub oskrzeli";
            case "objawy_szybko_ustepujace_po_leczeniu_infekcji": return "objawy szybko ustępujące po leczeniu infekcji";
            case "szybko_postepujaca_dusznosc": return "szybko postępująca duszność";
            case "zmeczenie_znaczne": return "znaczne zmęczenie";
            case "objawy_paraneoplastyczne": return "objawy paraneoplastyczne (zespoły towarzyszące nowotworom)";
            case "bardzo_powolny_rozwoj_objawow_przez_lata": return "bardzo powolny rozwój objawów (przez lata)";
            case "objawy_wskazujace_na_typowe_SCLC_np_szybkie_paraneoplastyczne": return "objawy wskazujące na typowe SCLC (np. szybki przebieg, zespoły paraneoplastyczne)";
            case "poty_nocne_lub_goraczka": return "poty nocne lub gorączka";
            case "brak_historii_narazenia_na_azbest_przy_typowych_objawach": return "brak historii narażenia na azbest przy typowych objawach";
            case "dusznosc_stopniowo_narastajaca": return "duszność stopniowo narastająca";
            case "zmiany_skorne_charakterystyczne": return "charakterystyczne zmiany skórne (np. rumień guzowaty)";
            case "postepujaca_dusznosc_wysilkowa": return "postępująca duszność wysiłkowa";
            case "suchy_uporczywy_kaszel": return "suchy, uporczywy kaszel";
            case "znana_przyczyna_wloknienia_pluc": return "znana przyczyna włóknienia płuc (np. narażenie zawodowe, choroba tkanki łącznej)";
            // "postepujaca_dusznosc" already exists, so covered
            case "suchy_kaszel_przewlekly": return "suchy kaszel przewlekły";
            case "bol_lub_ucisk_w_klatce_piersiowej": return "ból lub ucisk w klatce piersiowej";
            case "trzeszczenia_w_plucach": return "trzeszczenia w płucach (słyszalne podczas osłuchiwania)";
            case "brak_historii_narazenia_na_azbest": return "brak historii narażenia na azbest";
            case "dusznosc_szczegolnie_podczas_wysilku": return "duszność, szczególnie podczas wysiłku";
            case "brak_historii_narazenia_na_pyl_przemyslowy": return "brak historii narażenia na pył przemysłowy";
            case "nagla_dusznosc": return "nagła duszność";
            case "bol_w_klatce_piersiowej_nasilajacy_sie_przy_wdechu": return "ból w klatce piersiowej nasilający się przy wdechu";
            case "tachykardia": return "tachykardia (przyspieszone bicie serca)";
            case "omdlenie_lub_zaslabniecie": return "omdlenie lub zasłabnięcie";
            case "objawy_rozwijajace_sie_bardzo_powoli_przez_tygodnie_miesiace": return "objawy rozwijające się bardzo powoli (tygodnie/miesiące)";
            case "dusznosc_poczatkowo_wysilkowa_pozniej_spoczynkowa": return "duszność, początkowo wysiłkowa, później spoczynkowa";
            case "obrzeki_konczyn_dolnych": return "obrzęki kończyn dolnych";
            case "nagly_ostry_bol_w_klatce_piersiowej_jednostronny": return "nagły, ostry, jednostronny ból w klatce piersiowej";
            case "uczucie_napiecia_w_klatce_piersiowej": return "uczucie napięcia w klatce piersiowej";
            case "dusznosc_jesli_rozlegla": return "duszność (jeśli niedodma jest rozległa)";
            case "bol_w_klatce_piersiowej_rzadko": return "ból w klatce piersiowej (rzadko)";
            case "slabe_szmery_oddechowe_nad_obszarem": return "słabe szmery oddechowe nad obszarem niedodmy";
            case "sinica_w_ciezkich_przypadkach": return "sinica (w ciężkich przypadkach)";
            case "skrajna_dusznosc_nasilajaca_sie_w_pozycji_lezacej": return "skrajna duszność, nasilająca się w pozycji leżącej (orthopnoe)";
            case "kaszel_z_rozowa_pienista_plwocina": return "kaszel z różową, pienistą plwociną";
            case "uczucie_duszenia_sie_topienia": return "uczucie duszenia się lub topienia";
            case "swistajacy_oddech_lub_rzezenia": return "świszczący oddech lub rzężenia";
            case "zimna_spocona_skora": return "zimna, spocona skóra";
            case "objawy_zlokalizowane_wylacznie_poza_ukladem_oddechowym_i_krążenia": return "objawy zlokalizowane wyłącznie poza układem oddechowym i krążenia";
            case "odkrztuszanie_krwi_lub_plwociny_z_krwia": return "odkrztuszanie krwi lub plwociny z domieszką krwi";

            // Dla rozedmy, gdyby była
            // case "beczkowata_klatka_piersiowa": return "beczkowata klatka piersiowa";
            // case "dusznosc_znaczna": return "znaczna duszność";
            // case "wydluzony_wydech": return "wydłużony wydech";
            // case "uzywanie_dodatkowych_miesni_oddechowych": return "używanie dodatkowych mięśni oddechowych";

            default: return symptom.replace("_", " ");
        }
    }

    // Mapa tłumaczeń czynników ryzyka na język polski (do wyjaśnień)
    public static String translateRiskFactor(String factor) {
        switch(factor) {
            // Istniejące
            case "palenie_tytoniu": return "palenie tytoniu";
            // case "narazenie_na_azbestowe": return "narażenie na azbest"; // Zastąpione przez narazenie_na_azbest
            case "przewlekle_zapalenie_oskrzeli": return "przewlekłe zapalenie oskrzeli";
            case "zakazone_srodowisko_pracy": return "zakażone środowisko pracy (np. pyły, gazy)"; // Doprecyzowane
            case "obesity": return "otyłość";
            case "wiek_powyzej_50": return "wiek powyżej 50 lat"; // Ogólne, niektóre choroby mają bardziej specyficzne (np. wiek_powyzej_60)
            case "narazenie_na_alergeny": return "narażenie na alergeny";
            // case "zakrzepica_zylna": return "zakrzepica żylna"; // Zastąpione przez zakrzepica_zyl_glebokich_DVT
            case "zaburzenia_polykania": return "zaburzenia połykania";

            // Nowe
            case "wiek_starszy": return "podeszły wiek";
            case "oslabiony_uklad_odpornosciowy": return "osłabiony układ odpornościowy";
            case "przewlekle_choroby_pluc": return "przewlekłe choroby płuc (inne)";
            case "kontakt_z_chorym_na_gruzlice": return "kontakt z osobą chorą na gruźlicę";
            case "niedozywienie": return "niedożywienie";
            case "infekcja_wirusowa_gornych_drog_oddechowych": return "przebyta infekcja wirusowa górnych dróg oddechowych (np. przeziębienie, grypa)";
            case "narazenie_na_drazniace_substancje": return "narażenie na drażniące substancje (np. dym, opary chemiczne)";
            case "refluks_zoladkowo_przelykowy_choroba": return "choroba refluksowa przełyku (GERD)";
            case "zla_higiena_jamy_ustnej": return "zła higiena jamy ustnej";
            case "obnizony_poziom_swiadomosci": return "obniżony poziom świadomości";
            case "dlugotrwale_narazenie_na_pyly_chemikalia": return "długotrwałe narażenie na pyły lub chemikalia w miejscu pracy";
            case "wiek_powyzej_40": return "wiek powyżej 40 lat";
            case "rodzinne_wystepowanie_astmy_lub_alergii": return "rodzinne występowanie astmy lub alergii";
            case "alergie_osobiste": return "alergie osobiste (np. katar sienny, egzema)";
            case "narazenie_na_wyzwalacze": return "narażenie na wyzwalacze (np. alergeny, dym, wysiłek fizyczny, zimne powietrze)";
            case "przebyte_ciezkie_infekcje_plucne": return "przebyte ciężkie infekcje płucne (np. zapalenie płuc, gruźlica)";
            case "zaburzenia_odpornosci": return "zaburzenia odporności";
            case "historia_rodzinna_mukowiscydozy": return "mukowiscydoza w rodzinie (choroba genetyczna)";
            case "narazenie_na_bierne_palenie": return "narażenie na bierne palenie tytoniu";
            case "narazenie_na_radon_azbest_inne_rakotworcze": return "narażenie na radon, azbest lub inne substancje rakotwórcze";
            case "historia_rodzinna_raka_pluc": return "rak płuc w rodzinie";
            case "narazenie_na_azbest": return "narażenie na azbest";
            case "wiek_20_40_lat": return "wiek 20-40 lat";
            case "pochodzenie_afroamerykanskie_skandynawskie": return "pochodzenie afroamerykańskie lub skandynawskie";
            case "wiek_powyzej_60": return "wiek powyżej 60 lat";
            case "palenie_tytoniu_w_anamnezie": return "palenie tytoniu w przeszłości";
            case "dlugotrwale_narazenie_na_azbest": return "długotrwałe narażenie na azbest";
            case "dlugotrwale_narazenie_na_pyl_mineralny": return "długotrwałe narażenie na pył mineralny (np. krzemionka, pył węglowy)";
            case "zakrzepica_zyl_glebokich_DVT": return "zakrzepica żył głębokich (DVT)";
            case "dlugotrwale_unieruchomienie": return "długotrwałe unieruchomienie (np. po operacji, długa podróż)";
            case "zabiegi_operacyjne_urazy": return "niedawne zabiegi operacyjne lub urazy";
            case "nowotwory_zlosliwe": return "choroba nowotworowa złośliwa";
            case "stosowanie_doustnej_antykoncepcji_hormonalnej_terapii_zastepczej": return "stosowanie doustnej antykoncepcji hormonalnej lub hormonalnej terapii zastępczej";
            case "choroby_serca_lewej_komory": return "choroby lewej komory serca";
            case "przewlekle_choroby_pluc_np_POChP_wloknienie": return "przewlekłe choroby płuc (np. POChP, włóknienie płuc)";
            case "zatorowosc_plucna_przewlekla": return "przewlekła zatorowość płucna";
            case "niektore_leki_toksyny": return "niektóre leki lub toksyny";
            case "wysoki_szczuply_mlody_mezczyzna": return "młody, wysoki, szczupły mężczyzna (ryzyko samoistnej odmy)";
            case "choroby_pluc_np_POChP_mukowiscydoza": return "istniejące choroby płuc (np. POChP, mukowiscydoza)";
            case "uraz_klatki_piersiowej": return "uraz klatki piersiowej";
            case "unieruchomienie_po_operacji": return "unieruchomienie po operacji";
            case "zablokowanie_drzewa_oskrzelowego": return "zablokowanie drzewa oskrzelowego (np. czop śluzowy, guz, ciało obce)";
            case "ucisk_na_pluco_z_zewnatrz": return "ucisk na płuco z zewnątrz (np. płyn w opłucnej, guz)";
            case "choroby_serca_niewydolnosc_serca": return "choroby serca, szczególnie niewydolność serca";
            case "wysokie_cisnienie_krwi": return "wysokie ciśnienie krwi (nadciśnienie tętnicze)";
            case "uszkodzenie_pluc_np_ARDS_infekcja": return "uszkodzenie płuc (np. ARDS, ciężka infekcja)";

            // Dla rozedmy, gdyby była
            // case "niedobor_alfa_1_antytrypsyny": return "niedobór alfa-1 antytrypsyny";

            default: return factor.replace("_", " ");
        }
    }

    // Metoda tłumacząca nazwę choroby na bardziej przyjazną formę
    public static String translateDiseaseName(String disease) {
        return disease.replace("_", " "); // Proste tłumaczenie, można rozbudować jeśli nazwy chorób w Prologu są skrótami
    }
}