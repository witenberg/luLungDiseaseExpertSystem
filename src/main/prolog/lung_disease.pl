% Szczegółowa baza wiedzy chorób płuc

% --- CHOROBY INFEKCYJNE I ZAPALNE ---

choroba(zapalenie_pluc, [ % Pneumonia
    objaw_kluczowy(goraczka, 0.9),              % Fever (often high) is very common
    objaw_kluczowy(kaszel, 0.9),                % Cough, often with phlegm (mokry_kaszel can be a specific type)
    objaw_kluczowy(dusznosc, 0.8),              % Shortness of breath
    objaw(bol_w_klatce_piersiowej, 0.7),      % Chest pain, often pleuritic
    objaw(zmeczenie, 0.6),                    % Fatigue
    objaw(dreszcze, 0.7),                     % Chills (New symptom, common in pneumonia) - add 'dreszcze' to DiagnosticData
    objaw(poty, 0.6),                         % Sweating (pocenie_nocne is more specific to TB) - add 'poty' to DiagnosticData
    objaw(splatanie_u_starszych, 0.5),        % Confusion, especially in older adults (New) - add 'splatanie_u_starszych'
    czynnik_ryzyka(wiek_starszy, 0.6),          % Advanced age (New) - add 'wiek_starszy'
    czynnik_ryzyka(oslabiony_uklad_odpornosciowy, 0.7), % Weakened immune system (New) - add 'oslabiony_uklad_odpornosciowy'
    czynnik_ryzyka(przewlekle_choroby_pluc, 0.6),% Existing chronic lung diseases (New) - add 'przewlekle_choroby_pluc'
    czynnik_ryzyka(palenie_tytoniu, 0.5),
    objaw_wykluczajacy(brak_goraczki_przy_objawach_infekcyjnych) % Absence of fever when other infection signs are strong might suggest non-typical or mild cases, but for typical, it's expected. Let's refine this.
                                            % Original: goraczka_ponizej_38 - this is a bit too specific for a general exclusion.
                                            % A very low grade fever or no fever with severe cough/dyspnea might point away from typical bacterial pneumonia.
                                            % Let's keep it simple or remove if too complex to generalize.
                                            % For now, I'll remove the original wykluczajacy as it's too specific for all pneumonia types.
                                            % Re-thinking: WHO mentions alveoli filled with pus and fluid. A lack of productive cough where expected could be an indicator or atypical.
                                            % Let's stick to what's more clearly excluding or very strongly pointing away.
                                            % A very mild presentation without significant systemic symptoms (like high fever or severe dyspnea) might be atypical but not strictly excluded.
                                            % Will omit objaw_wykluczajacy for now for general pneumonia, as it's broad.
]).

choroba(gruzlica, [ % Tuberculosis
    objaw_kluczowy(przewlekly_kaszel, 0.9),       % Persistent cough, often > 2-3 weeks (New, more specific than just kaszel) - add 'przewlekly_kaszel'
    objaw_kluczowy(goraczka, 0.8),              % Fever, often low-grade and evening
    objaw_kluczowy(pocenie_nocne, 0.9),           % Night sweats are very characteristic
    objaw_kluczowy(utrata_masy_ciala, 0.8),     % Unexplained weight loss
    objaw(kaszel_z_krwia, 0.7),               % Hemoptysis (can be key, but not always present)
    objaw(zmeczenie, 0.7),
    objaw(bol_w_klatce_piersiowej, 0.6),
    czynnik_ryzyka(kontakt_z_chorym_na_gruzlice, 1.0), % Contact with TB patient (New) - add 'kontakt_z_chorym_na_gruzlice'
    czynnik_ryzyka(oslabiony_uklad_odpornosciowy, 0.8), % e.g., HIV
    czynnik_ryzyka(niedozywienie, 0.6),        % Malnutrition (New) - add 'niedozywienie'
    objaw_wykluczajacy(dobra_odpowiedz_na_standardowe_antybiotyki) % TB requires specific antibiotics (New) - add 'dobra_odpowiedz_na_standardowe_antybiotyki'
]).

choroba(zapalenie_oskrzeli, [ % Bronchitis (primarily acute)
    objaw_kluczowy(kaszel, 0.9),                % Cough is the hallmark, can be dry or productive
    objaw(odkrztuszanie_plwociny, 0.8),       % Production of mucus (sputum), can be various colors (New, more general than mokry_kaszel) - add 'odkrztuszanie_plwociny'
    objaw(zmeczenie, 0.6),
    objaw(bol_gardla, 0.5),                   % Sore throat (New, often precedes cough) - add 'bol_gardla'
    objaw(lekka_goraczka, 0.5),               % Slight fever and chills (New) - add 'lekka_goraczka'
    objaw(dyskomfort_w_klatce_piersiowej, 0.6),% Chest discomfort (New, different from sharp pain) - add 'dyskomfort_w_klatce_piersiowej'
    czynnik_ryzyka(infekcja_wirusowa_gornych_drog_oddechowych, 0.8), % Recent cold or flu (New) - add 'infekcja_wirusowa_gornych_drog_oddechowych'
    czynnik_ryzyka(palenie_tytoniu, 0.7),
    czynnik_ryzyka(narazenie_na_drazniace_substancje, 0.6), % Exposure to irritants (New) - add 'narazenie_na_drazniace_substancje'
    objaw_wykluczajacy(wysoka_goraczka_powyzej_38_5C), % High fever might suggest pneumonia instead (New) - add 'wysoka_goraczka_powyzej_38_5C'
    objaw_wykluczajacy(znaczna_dusznosc_spoczynkowa) % Significant shortness of breath at rest is more typical for pneumonia/COPD (New) - add 'znaczna_dusznosc_spoczynkowa'
]).
% Note: Chronic bronchitis is a component of COPD. This entry is more for acute bronchitis.

choroba(alergiczne_zapalenie_pluc, [ % Hypersensitivity Pneumonitis
    objaw_kluczowy(dusznosc_po_ekspozycji, 0.9), % Shortness of breath after exposure to specific allergen (New, very key) - add 'dusznosc_po_ekspozycji'
    objaw_kluczowy(suchy_kaszel, 0.8),
    objaw(goraczka, 0.7),                     % Often occurs 4-6 hours after exposure
    objaw(dreszcze, 0.7),
    objaw(zmeczenie, 0.6),
    objaw(bole_miesni_stawow, 0.5),          % Muscle and joint pain (New) - add 'bole_miesni_stawow'
    czynnik_ryzyka(narazenie_na_alergeny, 1.0), % e.g., molds, bird droppings, certain chemicals
    objaw_wykluczajacy(brak_zwiazku_objawow_z_ekspozycja) % Symptoms not linked to any specific environmental exposure (New) - add 'brak_zwiazku_objawow_z_ekspozycja'
]).

choroba(zapalenie_pluc_aspiracyjne, [ % Aspiration Pneumonia
    objaw_kluczowy(kaszel_po_posilkach_lub_wymiotach, 0.9), % Cough after eating/drinking or vomiting (New) - add 'kaszel_po_posilkach_lub_wymiotach'
    objaw_kluczowy(dusznosc, 0.8),
    objaw(goraczka, 0.8),
    objaw(odkrztuszanie_plwociny_o_nieprzyjemnym_zapachu, 0.7), % Foul-smelling sputum (New) - add 'odkrztuszanie_plwociny_o_nieprzyjemnym_zapachu'
    objaw(bol_w_klatce_piersiowej, 0.6),
    objaw(swistajacy_oddech, 0.5),
    czynnik_ryzyka(zaburzenia_polykania, 0.9),    % Dysphagia is a primary risk factor
    czynnik_ryzyka(obnizony_poziom_swiadomosci, 0.8), % Reduced level of consciousness (e.g., alcohol, stroke) (New) - add 'obnizony_poziom_swiadomosci'
    czynnik_ryzyka(refluks_zoladkowo_przelykowy_choroba, 0.6), % GERD (New) - add 'refluks_zoladkowo_przelykowy_choroba'
    czynnik_ryzyka(zla_higiena_jamy_ustnej, 0.5) % Poor oral hygiene (New) - add 'zla_higiena_jamy_ustnej'
]).

% --- CHOROBY OBTURACYJNE ---

choroba(przewlekla_obturacyjna_choroba_pluc, [ % COPD (includes emphysema and chronic bronchitis aspects)
    objaw_kluczowy(dusznosc_wysilkowa_postepujaca, 0.9), % Progressive exertional dyspnea (New, very key) - add 'dusznosc_wysilkowa_postepujaca'
    objaw_kluczowy(przewlekly_kaszel_produktywny, 0.8), % Chronic productive cough (often morning) (New) - add 'przewlekly_kaszel_produktywny'
    objaw_kluczowy(swistajacy_oddech, 0.7),      % Wheezing
    objaw(uczucie_sciskania_w_klatce, 0.6),   % Chest tightness (New) - add 'uczucie_sciskania_w_klatce'
    objaw(zmeczenie, 0.6),
    objaw(czeste_infekcje_drog_oddechowych, 0.7),% Frequent respiratory infections (New) - add 'czeste_infekcje_drog_oddechowych'
    objaw(utrata_masy_ciala, 0.5),             % In later stages
    czynnik_ryzyka(palenie_tytoniu, 1.0),         % Primary risk factor
    czynnik_ryzyka(dlugotrwale_narazenie_na_pyly_chemikalia, 0.7), % Long-term exposure to dust/chemicals (New) - add 'dlugotrwale_narazenie_na_pyly_chemikalia'
    czynnik_ryzyka(wiek_powyzej_40, 0.6),        % Typically diagnosed in older adults (New) - add 'wiek_powyzej_40'
    objaw_wykluczajacy(calkowita_odwracalnosc_obturacji_po_lekach_rozszerzajacych_oskrzela) % Full reversibility of obstruction suggests asthma more (New) - add 'calkowita_odwracalnosc_obturacji_po_lekach_rozszerzajacych_oskrzela'
]).
% Note: rozedma_pluc is largely covered by COPD. If a separate entry is truly needed, it would focus on lung tissue destruction specifically.

choroba(astma, [ % Asthma
    objaw_kluczowy(swistajacy_oddech, 0.9),      % Wheezing, especially expiratory
    objaw_kluczowy(nawracajaca_dusznosc, 0.9),    % Recurrent shortness of breath, often episodic (New) - add 'nawracajaca_dusznosc'
    objaw_kluczowy(kaszel_nasilajacy_sie_w_nocy_lub_nad_ranem, 0.8), % Cough, worse at night/early morning (New) - add 'kaszel_nasilajacy_sie_w_nocy_lub_nad_ranem'
    objaw(uczucie_sciskania_w_klatce, 0.7),   % Chest tightness
    czynnik_ryzyka(rodzinne_wystepowanie_astmy_lub_alergii, 0.8), % Family history of asthma or allergies (New) - add 'rodzinne_wystepowanie_astmy_lub_alergii'
    czynnik_ryzyka(alergie_osobiste, 0.7),        % Personal history of allergies (e.g., hay fever, eczema) (New) - add 'alergie_osobiste'
    czynnik_ryzyka(narazenie_na_wyzwalacze, 0.7), % Exposure to triggers (allergens, irritants, exercise, cold air) (New) - add 'narazenie_na_wyzwalacze'
    objaw_wykluczajacy(goraczka_powyzej_38),     % High fever suggests infection, not uncomplicated asthma attack
    objaw_wykluczajacy(brak_odpowiedzi_na_leki_rozszerzajace_oskrzela) % Lack of response to bronchodilators during an episode (New) - add 'brak_odpowiedzi_na_leki_rozszerzajace_oskrzela'
]).

choroba(bronchiektazje, [ % Bronchiectasis
    objaw_kluczowy(przewlekly_kaszel_z_codziennym_odkrztuszaniem_duzych_ilosci_plwociny, 1.0), % Chronic cough with daily large volume sputum (New, very key) - add 'przewlekly_kaszel_z_codziennym_odkrztuszaniem_duzych_ilosci_plwociny'
    objaw_kluczowy(nawracajace_infekcje_pluc, 0.9),
    objaw(dusznosc, 0.7),
    objaw(krwioplucie, 0.6),                   % Hemoptysis can occur
    objaw(zmeczenie, 0.6),
    objaw(bol_w_klatce_piersiowej, 0.5),      % Pleuritic chest pain sometimes
    czynnik_ryzyka(przebyte_ciezkie_infekcje_plucne, 0.8), % History of severe lung infections (e.g., pneumonia, TB) (New) - add 'przebyte_ciezkie_infekcje_plucne'
    czynnik_ryzyka(mukowiscydoza, 0.7),          % Cystic fibrosis is a common cause
    czynnik_ryzyka(zaburzenia_odpornosci, 0.6)  % Immune deficiencies (New) - add 'zaburzenia_odpornosci'
]).

choroba(mukowiscydoza, [ % Cystic Fibrosis (pulmonary aspects)
    objaw_kluczowy(nawracajace_infekcje_pluc, 0.9), % Starting in childhood
    objaw_kluczowy(przewlekly_kaszel_z_gesta_lepka_plwocina, 0.9), % Chronic cough with thick, sticky sputum (New) - add 'przewlekly_kaszel_z_gesta_lepka_plwocina'
    objaw(dusznosc_postepujaca, 0.8),         % Progressive dyspnea (New) - add 'dusznosc_postepujaca'
    objaw(swistajacy_oddech, 0.7),
    objaw(trudnosci_w_przybieraniu_na_wadze, 0.8), % Despite good appetite (also a non-pulmonary key sign)
    objaw(polipy_nosa, 0.6),                   % Nasal polyps (New) - add 'polipy_nosa'
    objaw(palce_paleczkowate, 0.7),            % Clubbing of fingers and toes (New) - add 'palce_paleczkowate'
    czynnik_ryzyka(historia_rodzinna_mukowiscydozy, 1.0) % Genetic condition (New) - add 'historia_rodzinna_mukowiscydozy'
    % No typical objaw_wykluczajacy as it's a genetic systemic disease confirmed by tests.
]).


% --- NOWOTWORY PŁUC ---

choroba(rak_pluc, [ % Lung Cancer (general category, can be SCLC or NSCLC)
    objaw_kluczowy(przewlekly_kaszel_lub_zmiana_charakteru_kaszlu, 0.9), % Persistent cough or change in chronic cough (New) - add 'przewlekly_kaszel_lub_zmiana_charakteru_kaszlu'
    objaw_kluczowy(kaszel_z_krwia, 0.8),         % Hemoptysis, even small amounts, is alarming
    objaw(dusznosc, 0.7),
    objaw(bol_w_klatce_piersiowej, 0.7),      % Often persistent, dull, or aching
    objaw(chrypka, 0.6),                      % Hoarseness
    objaw(utrata_masy_ciala, 0.8),            % Unexplained weight loss
    objaw(zmeczenie, 0.7),
    objaw(nawracajace_zapalenie_pluc_lub_oskrzeli, 0.6), % Recurrent pneumonia or bronchitis (New) - add 'nawracajace_zapalenie_pluc_lub_oskrzeli'
    czynnik_ryzyka(palenie_tytoniu, 1.0),         % Strongest risk factor
    czynnik_ryzyka(narazenie_na_bierne_palenie, 0.7),% Exposure to secondhand smoke (New) - add 'narazenie_na_bierne_palenie'
    czynnik_ryzyka(narazenie_na_radon_azbest_inne_rakotworcze, 0.8), % Exposure to radon, asbestos, other carcinogens (New) - add 'narazenie_na_radon_azbest_inne_rakotworcze'
    czynnik_ryzyka(historia_rodzinna_raka_pluc, 0.6), % Family history (New) - add 'historia_rodzinna_raka_pluc'
    objaw_wykluczajacy(objawy_szybko_ustepujace_po_leczeniu_infekcji) % Symptoms that resolve quickly with infection treatment point away from cancer (New) - add 'objawy_szybko_ustepujace_po_leczeniu_infekcji'
]).

choroba(rak_pluc_drobnokomorkowy, [ % Small Cell Lung Cancer (SCLC)
    % SCLC often presents with more aggressive symptoms and earlier metastasis
    objaw_kluczowy(szybko_postepujaca_dusznosc, 0.9), % Rapidly progressive dyspnea (New) - add 'szybko_postepujaca_dusznosc'
    objaw_kluczowy(kaszel, 0.8),
    objaw(utrata_masy_ciala, 0.9),            % Often significant
    objaw(zmeczenie_znaczne, 0.8),             % Significant fatigue (New) - add 'zmeczenie_znaczne'
    objaw(chrypka, 0.7),
    objaw(bol_w_klatce_piersiowej, 0.7),
    objaw(objawy_paraneoplastyczne, 0.7),      % Paraneoplastic syndromes (e.g., SIADH, Cushing's) are more common (New) - add 'objawy_paraneoplastyczne'
    czynnik_ryzyka(palenie_tytoniu, 1.0),         % Very strongly linked to smoking
    % objaw_wykluczajacy - difficult for SCLC as it's aggressive. Perhaps 'bardzo_powolny_rozwoj_objawow' (very slow symptom progression)
    objaw_wykluczajacy(bardzo_powolny_rozwoj_objawow_przez_lata) % SCLC is typically fast-growing (New) - add 'bardzo_powolny_rozwoj_objawow_przez_lata'
]).

choroba(rak_pluc_niedrobnokomorkowy, [ % Non-Small Cell Lung Cancer (NSCLC)
    % Symptoms can be similar to general lung cancer, perhaps slower onset than SCLC
    objaw_kluczowy(przewlekly_kaszel_lub_zmiana_charakteru_kaszlu, 0.9),
    objaw_kluczowy(kaszel_z_krwia, 0.8),
    objaw(dusznosc, 0.7),
    objaw(bol_w_klatce_piersiowej, 0.7),
    objaw(utrata_masy_ciala, 0.7),
    objaw(chrypka, 0.6),
    czynnik_ryzyka(palenie_tytoniu, 0.9),
    czynnik_ryzyka(narazenie_na_radon_azbest_inne_rakotworcze, 0.8),
    czynnik_ryzyka(wiek_powyzej_50, 0.6),
    objaw_wykluczajacy(objawy_wskazujace_na_typowe_SCLC_np_szybkie_paraneoplastyczne) % Symptoms strongly indicative of SCLC's typical rapid course/syndromes (New) - add 'objawy_wskazujace_na_typowe_SCLC_np_szybkie_paraneoplastyczne'
]).
% Note: rak_oskrzeli is essentially a location for lung cancer, covered by the above.

choroba(rak_mezotelioma, [ % Mesothelioma
    objaw_kluczowy(dusznosc, 0.9),                % Often due to pleural effusion
    objaw_kluczowy(bol_w_klatce_piersiowej, 0.9), % Pain in chest wall
    objaw(kaszel, 0.6),                       % Usually dry
    objaw(utrata_masy_ciala, 0.7),
    objaw(zmeczenie, 0.7),
    objaw(poty_nocne_lub_goraczka, 0.5),      % Night sweats or fever (New) - add 'poty_nocne_lub_goraczka'
    czynnik_ryzyka(narazenie_na_azbest, 1.0),   % Primary cause (azbestowe from your list) - changed to 'narazenie_na_azbest'
    objaw_wykluczajacy(brak_historii_narazenia_na_azbest_przy_typowych_objawach) % While rare cases occur without known exposure, it's highly dominant (New) - add 'brak_historii_narazenia_na_azbest_przy_typowych_objawach'
]).

% --- CHOROBY INTERSTycjalne I NACZYNIOWE ---

choroba(sarkoidoza, [ % Sarcoidosis (pulmonary focus)
    objaw_kluczowy(suchy_kaszel, 0.8),
    objaw_kluczowy(dusznosc_stopniowo_narastajaca, 0.8), % Dyspnea, often gradual (New) - add 'dusznosc_stopniowo_narastajaca'
    objaw(zmeczenie, 0.7),
    objaw(bol_w_klatce_piersiowej, 0.6),      % Often non-specific
    objaw(goraczka, 0.5),
    objaw(utrata_masy_ciala, 0.5),
    objaw(obrzek_wezlow_chlonnych, 0.7),      % Especially hilar lymph nodes
    objaw(zmiany_skorne_charakterystyczne, 0.6),% e.g., erythema nodosum (New) - add 'zmiany_skorne_charakterystyczne'
    % No strong objaw_wykluczajacy as it's a diagnosis of exclusion with varied presentation.
    % czynnik_ryzyka are not well defined, possibly genetic predisposition, environmental triggers.
    czynnik_ryzyka(wiek_20_40_lat, 0.6),        % Common age of onset (New) - add 'wiek_20_40_lat'
    czynnik_ryzyka(pochodzenie_afroamerykanskie_skandynawskie, 0.6) % Higher incidence in these populations (New) - add 'pochodzenie_afroamerykanskie_skandynawskie'
]).

choroba(idiopatyczne_wloknienie_pluc, [ % Idiopathic Pulmonary Fibrosis (IPF)
    objaw_kluczowy(postepujaca_dusznosc_wysilkowa, 0.9), % Progressive exertional dyspnea (New, very key) - add 'postepujaca_dusznosc_wysilkowa'
    objaw_kluczowy(suchy_uporczywy_kaszel, 0.9), % Dry, persistent cough (New) - add 'suchy_uporczywy_kaszel'
    objaw_kluczowy(trzeszczenia_u_podstawy_pluc, 0.8), % Bibasilar crackles (Velcro rales)
    objaw(zmeczenie, 0.7),
    objaw(palce_paleczkowate, 0.7),            % Clubbing
    objaw(utrata_masy_ciala, 0.6),
    czynnik_ryzyka(wiek_powyzej_60, 0.8),        % Typically affects older adults (Changed from 50) - add 'wiek_powyzej_60'
    czynnik_ryzyka(palenie_tytoniu_w_anamnezie, 0.7), % History of smoking (New) - add 'palenie_tytoniu_w_anamnezie'
    czynnik_ryzyka(refluks_zoladkowo_przelykowy_choroba, 0.5),
    objaw_wykluczajacy(znana_przyczyna_wloknienia_pluc) % e.g., exposure, connective tissue disease (New) - add 'znana_przyczyna_wloknienia_pluc'
]).

choroba(zwloknienie_pluc_azbestowe, [ % Asbestosis
    objaw_kluczowy(postepujaca_dusznosc, 0.9),   % Progressive dyspnea, especially on exertion (New name for clarity) - add 'postepujaca_dusznosc'
    objaw(suchy_kaszel_przewlekly, 0.8),       % Persistent dry cough (New) - add 'suchy_kaszel_przewlekly'
    objaw(bol_lub_ucisk_w_klatce_piersiowej, 0.7), % Chest pain or tightness (New) - add 'bol_lub_ucisk_w_klatce_piersiowej'
    objaw(trzeszczenia_w_plucach, 0.7),        % Crackles in lungs (New) - add 'trzeszczenia_w_plucach'
    objaw(palce_paleczkowate, 0.6),            % Clubbing
    czynnik_ryzyka(dlugotrwale_narazenie_na_azbest, 1.0), % Prolonged asbestos exposure (New, more specific) - add 'dlugotrwale_narazenie_na_azbest'
    objaw_wykluczajacy(brak_historii_narazenia_na_azbest) % Almost always linked to asbestos exposure (New) - add 'brak_historii_narazenia_na_azbest'
]).

choroba(pylica_pluc, [ % Pneumoconiosis (General term, often due to occupational dusts like silica, coal)
    objaw_kluczowy(dusznosc_szczegolnie_podczas_wysilku, 0.9), % Dyspnea, especially with exertion (New) - add 'dusznosc_szczegolnie_podczas_wysilku'
    objaw_kluczowy(przewlekly_kaszel, 0.8),       % Often with or without sputum
    objaw(zmeczenie, 0.6),
    objaw(bol_w_klatce_piersiowej, 0.6),      % Chest tightness or pain
    czynnik_ryzyka(dlugotrwale_narazenie_na_pyl_mineralny, 1.0), % e.g., silica, coal dust (New, specific) - add 'dlugotrwale_narazenie_na_pyl_mineralny'
                                                              % 'zakazone_srodowisko_pracy' is too vague
    objaw_wykluczajacy(brak_historii_narazenia_na_pyl_przemyslowy) % Essential for diagnosis (New) - add 'brak_historii_narazenia_na_pyl_przemyslowy'
]).

choroba(zatorowosc_plucna, [ % Pulmonary Embolism
    objaw_kluczowy(nagla_dusznosc, 1.0),          % Sudden onset dyspnea is hallmark (New) - add 'nagla_dusznosc'
    objaw_kluczowy(bol_w_klatce_piersiowej_nasilajacy_sie_przy_wdechu, 0.9), % Pleuritic chest pain (New) - add 'bol_w_klatce_piersiowej_nasilajacy_sie_przy_wdechu'
    objaw(kaszel, 0.6),                       % Can be dry or with bloody sputum
    objaw(krwioplucie, 0.7),
    objaw(przyspieszony_oddech, 0.7),
    objaw(tachykardia, 0.8),                  % Rapid heart rate (New) - add 'tachykardia'
    objaw(omdlenie_lub_zaslabniecie, 0.7),    % Syncope or presyncope (New) - add 'omdlenie_lub_zaslabniecie'
    czynnik_ryzyka(zakrzepica_zyl_glebokich_DVT, 1.0), % Deep Vein Thrombosis (from your list, zakrzepica_zylna) - changed to 'zakrzepica_zyl_glebokich_DVT'
    czynnik_ryzyka(dlugotrwale_unieruchomienie, 0.8), % Prolonged immobility (e.g., surgery, travel) (New) - add 'dlugotrwale_unieruchomienie'
    czynnik_ryzyka(zabiegi_operacyjne_urazy, 0.7), % Recent surgery or trauma (New) - add 'zabiegi_operacyjne_urazy'
    czynnik_ryzyka(nowotwory_zlosliwe, 0.6),     % Cancer (New) - add 'nowotwory_zlosliwe'
    czynnik_ryzyka(stosowanie_doustnej_antykoncepcji_hormonalnej_terapii_zastepczej, 0.5), % Oral contraceptives/HRT (New) - add 'stosowanie_doustnej_antykoncepcji_hormonalnej_terapii_zastepczej'
    objaw_wykluczajacy(objawy_rozwijajace_sie_bardzo_powoli_przez_tygodnie_miesiace) % PE is usually acute/subacute (New) - add 'objawy_rozwijajace_sie_bardzo_powoli_przez_tygodnie_miesiace'
]).

choroba(nadcisnienie_plucne, [ % Pulmonary Hypertension
    objaw_kluczowy(dusznosc_poczatkowo_wysilkowa_pozniej_spoczynkowa, 0.9), % Dyspnea, initially exertional, later at rest (New) - add 'dusznosc_poczatkowo_wysilkowa_pozniej_spoczynkowa'
    objaw(zmeczenie, 0.8),
    objaw(bol_w_klatce_piersiowej, 0.7),      % Angina-like chest pain
    objaw(zawroty_glowy, 0.6),
    objaw(omdlenia, 0.7),                     % Especially during exertion
    objaw(obrzeki_konczyn_dolnych, 0.6),       % Peripheral edema (New) - add 'obrzeki_konczyn_dolnych'
    objaw(sinica, 0.5),                       % Cyanosis
    czynnik_ryzyka(choroby_serca_lewej_komory, 0.7), % Left heart disease (New) - add 'choroby_serca_lewej_komory'
    czynnik_ryzyka(przewlekle_choroby_pluc_np_POChP_wloknienie, 0.7), % Chronic lung diseases (New) - add 'przewlekle_choroby_pluc_np_POChP_wloknienie'
    czynnik_ryzyka(zatorowosc_plucna_przewlekla, 0.6), % Chronic thromboembolic pulmonary hypertension (New) - add 'zatorowosc_plucna_przewlekla'
    czynnik_ryzyka(niektore_leki_toksyny, 0.5)   % Certain drugs/toxins (New) - add 'niektore_leki_toksyny'
]).

% --- INNE CHOROBY PŁUC ---

choroba(odma_oplucnowa, [ % Pneumothorax
    objaw_kluczowy(nagly_ostry_bol_w_klatce_piersiowej_jednostronny, 1.0), % Sudden, sharp, unilateral chest pain (New) - add 'nagly_ostry_bol_w_klatce_piersiowej_jednostronny'
    objaw_kluczowy(nagla_dusznosc, 0.9),          % Sudden dyspnea
    objaw(suchy_kaszel, 0.6),                 % Dry, hacking cough
    objaw(przyspieszony_oddech, 0.7),
    objaw(uczucie_napiecia_w_klatce_piersiowej, 0.7), % Feeling of tightness in chest (New) - add 'uczucie_napiecia_w_klatce_piersiowej'
    czynnik_ryzyka(wysoki_szczuply_mlody_mezczyzna, 0.6), % Tall, thin young males (for spontaneous) (New) - add 'wysoki_szczuply_mlody_mezczyzna'
    czynnik_ryzyka(choroby_pluc_np_POChP_mukowiscydoza, 0.7), % Underlying lung disease (New) - add 'choroby_pluc_np_POChP_mukowiscydoza'
    czynnik_ryzyka(uraz_klatki_piersiowej, 0.8),  % Trauma to the chest (New) - add 'uraz_klatki_piersiowej'
    czynnik_ryzyka(palenie_tytoniu, 0.5),
    objaw_wykluczajacy(goraczka_wysoka)        % High fever is not typical for uncomplicated pneumothorax (original 'goraczka' is good)
]).

choroba(niedodma_pluc, [ % Atelectasis
    % Symptoms often depend on the extent and speed of collapse. Can be asymptomatic.
    objaw(dusznosc_jesli_rozlegla, 0.8),     % Dyspnea if large area affected (New) - add 'dusznosc_jesli_rozlegla'
    objaw(kaszel, 0.6),
    objaw(bol_w_klatce_piersiowej_rzadko, 0.5), % Chest pain (less common) (New) - add 'bol_w_klatce_piersiowej_rzadko'
    objaw(przyspieszony_oddech, 0.7),          % Tachypnea
    objaw(slabe_szmery_oddechowe_nad_obszarem, 0.9), % Diminished breath sounds over affected area (Key clinical sign) (New) - add 'slabe_szmery_oddechowe_nad_obszarem'
    czynnik_ryzyka(unieruchomienie_po_operacji, 0.9), % Post-operative immobilization (New) - add 'unieruchomienie_po_operacji'
    czynnik_ryzyka(zablokowanie_drzewa_oskrzelowego, 0.8), % Airway obstruction (mucus plug, tumor, foreign body) (New) - add 'zablokowanie_drzewa_oskrzelowego'
    czynnik_ryzyka(ucisk_na_pluco_z_zewnatrz, 0.7), % External compression (pleural effusion, tumor) (New) - add 'ucisk_na_pluco_z_zewnatrz'
    % No strong objaw_wykluczajacy, as it's often a consequence of other conditions.
    % Sinica (cyanosis) is a severe sign, not always present.
    objaw(sinica_w_ciezkich_przypadkach, 0.7) % Cyanosis in severe cases - original 'sinica' is okay, but this adds context. - add 'sinica_w_ciezkich_przypadkach'
]).

choroba(obrzek_pluc, [ % Pulmonary Edema
    objaw_kluczowy(skrajna_dusznosc_nasilajaca_sie_w_pozycji_lezacej, 1.0), % Extreme dyspnea, worse when lying down (orthopnea) (New) - add 'skrajna_dusznosc_nasilajaca_sie_w_pozycji_lezacej'
    objaw_kluczowy(kaszel_z_rozowa_pienista_plwocina, 0.9), % Cough with pink, frothy sputum (Very characteristic) (New) - add 'kaszel_z_rozowa_pienista_plwocina'
                                                      % (kaszel_z_piana from your list is similar)
    objaw(uczucie_duszenia_sie_topienia, 0.8), % Feeling of suffocation or drowning (New) - add 'uczucie_duszenia_sie_topienia'
    objaw(swistajacy_oddech_lub_rzezenia, 0.7),% Wheezing or crackles (rales) (New) - add 'swistajacy_oddech_lub_rzężenia'
    objaw(niepokoj_lek, 0.6),                  % Anxiety, restlessness (original 'niepokoj' is good)
    objaw(zimna_spocona_skora, 0.6),           % Cold, clammy skin (New) - add 'zimna_spocona_skora'
    objaw(tachykardia, 0.7),
    czynnik_ryzyka(choroby_serca_niewydolnosc_serca, 1.0), % Heart conditions, especially heart failure (New) - add 'choroby_serca_niewydolnosc_serca'
    czynnik_ryzyka(wysokie_cisnienie_krwi, 0.7),  % Hypertension (New) - add 'wysokie_cisnienie_krwi'
    czynnik_ryzyka(uszkodzenie_pluc_np_ARDS_infekcja, 0.6), % Lung injury (ARDS, severe infection) (New) - add 'uszkodzenie_pluc_np_ARDS_infekcja'
    objaw_wykluczajacy(objawy_zlokalizowane_wylacznie_poza_ukladem_oddechowym_i_krazenia) % Symptoms purely non-cardiopulmonary would make PE less likely (New) - add 'objawy_zlokalizowane_wylacznie_poza_ukladem_oddechowym_i_krazenia'
]).

% --- SYMPTOM JAKO "CHOROBA" ---
choroba(krwioplucie, [ % Hemoptysis (treating as a sign that needs investigation)
    % This is a symptom, not a disease. The rules should define what diseases cause it.
    % However, if you want to keep it as a "choroba" entry for some logic:
    objaw_kluczowy(odkrztuszanie_krwi_lub_plwociny_z_krwia, 1.0), % Coughing up blood or bloody sputum (New, more descriptive) - add 'odkrztuszanie_krwi_lub_plwociny_z_krwia'
                                                                % (kaszel_z_krwia and odpluwanie_krwi are good)
    % No typical risk factors or excluding symptoms for "hemoptysis" itself, as these depend on the underlying cause.
    % Perhaps a rule: jeśli objaw(krwioplucie) then diagnozuj(przyczyna_krwioplucia).
    % For now, I will keep it simple as per your structure.
    objaw(kaszel_z_krwia, 1.0),
    objaw(odpluwanie_krwi, 1.0)
    % To make it more useful, one might list common causes as 'objaw' with low weight if it must be a 'choroba'
    % objaw(moze_byc_objawem_raka_pluc, 0.3),
    % objaw(moze_byc_objawem_gruzlicy, 0.3),
    % objaw(moze_byc_objawem_zapalenia_oskrzeli_ciezkiego, 0.2)
]).

% Nowy predykat do obliczania ważonego dopasowania
dopasowanie(Choroba, ObjawyPacjenta, CzynnikiRyzykaPacjenta, Dopasowanie) :-
    choroba(Choroba, WymaganeGlobal),

    % 1. Sprawdź objawy wykluczające
    findall(S_wykl, member(objaw_wykluczajacy(S_wykl), WymaganeGlobal), ListaWykluczajacych),
    (   ListaWykluczajacych = [] -> true;  % Jeśli nie ma objawów wykluczających, kontynuuj
        \+ (member(S_do_sprawdzenia, ListaWykluczajacych), member(S_do_sprawdzenia, ObjawyPacjenta))
    ),

    % 2. Sprawdź obecność objawów kluczowych (nie wymagamy wszystkich)
    findall(S_klucz-W_klucz, member(objaw_kluczowy(S_klucz, W_klucz), WymaganeGlobal), ListaKluczowychZWagami),
    findall(S_klucz, member(S_klucz-_, ListaKluczowychZWagami), ListaNazwObjawowKluczowych),
    findall(S_klucz, (member(S_klucz, ListaNazwObjawowKluczowych), member(S_klucz, ObjawyPacjenta)), ObjawyKluczoweObecne),

    % 3. Zbierz wszystkie typy wymagań do obliczenia wyniku
    findall(S_reg-W_reg, member(objaw(S_reg, W_reg), WymaganeGlobal), ListaObjawowRegZWagami),
    findall(RF-W_rf, member(czynnik_ryzyka(RF, W_rf), WymaganeGlobal), ListaCzynnikowZWagami),

    % 4. Obliczanie sumy wag dla pasujących objawów (kluczowych i regularnych)
    findall(W_k, (member(S_k-W_k, ListaKluczowychZWagami), member(S_k, ObjawyPacjenta)), WagiPasujacychKluczowych),
    findall(W_r, (member(S_r-W_r, ListaObjawowRegZWagami), member(S_r, ObjawyPacjenta)), WagiPasujacychReg),
    append(WagiPasujacychKluczowych, WagiPasujacychReg, WagiPasujacychObjawowLista),
    sum_list(WagiPasujacychObjawowLista, SumaWagiObjawow),

    % 5. Obliczanie sumy wag dla pasujących czynników ryzyka
    findall(W_rf_pas, (member(RF_pas-W_rf_pas, ListaCzynnikowZWagami), member(RF_pas, CzynnikiRyzykaPacjenta)), WagiPasujacychCzynnikowLista),
    sum_list(WagiPasujacychCzynnikowLista, SumaWagiCzynnikow),

    % 6. Obliczanie sumy wszystkich zdefiniowanych wag
    findall(W_k_all, member(_-W_k_all, ListaKluczowychZWagami), WszystkieWagiKluczowe),
    findall(W_r_all, member(_-W_r_all, ListaObjawowRegZWagami), WszystkieWagiReg),
    append(WszystkieWagiKluczowe, WszystkieWagiReg, WszystkieWagiObjawowAllLista),
    sum_list(WszystkieWagiObjawowAllLista, SumaWszystkichWagObjawow),
    
    findall(W_rf_all, member(_-W_rf_all, ListaCzynnikowZWagami), WszystkieWagiCzynnikowLista),
    sum_list(WszystkieWagiCzynnikowLista, SumaWszystkichWagCzynnikow),

    % 7. Obliczanie końcowego dopasowania
    SumaWszystkichWag is SumaWszystkichWagObjawow + SumaWszystkichWagCzynnikow,
    SumaWagiPasujacych is SumaWagiObjawow + SumaWagiCzynnikow,

    % 8. Bonus za obecność objawów kluczowych
    length(ObjawyKluczoweObecne, LiczbaObjawowKluczowychObecnych),
    length(ListaNazwObjawowKluczowych, LiczbaWszystkichObjawowKluczowych),
    (LiczbaWszystkichObjawowKluczowych > 0 ->
        BonusKluczowy is (LiczbaObjawowKluczowychObecnych / LiczbaWszystkichObjawowKluczowych) * 20,
        Dopasowanie is min(100, (SumaWagiPasujacych / SumaWszystkichWag) * 80 + BonusKluczowy)
    ;
        Dopasowanie is (SumaWagiPasujacych / SumaWszystkichWag) * 100
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