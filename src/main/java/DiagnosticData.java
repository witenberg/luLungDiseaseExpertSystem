// Klasa z danymi statycznymi
package main.java;

import java.util.Arrays;
import java.util.List;

public class DiagnosticData {
    // Listy objawów i czynników ryzyka
    public static final List<String> OBJAWY = Arrays.asList(
            "kaszel", "suchy_kaszel", "mokry_kaszel", "kaszel_z_krwia", "goraczka", 
            "dusznosc", "bol_w_klatce_piersiowej", "zmeczenie", "utrata_masy_ciala", 
            "odpluwanie_krwi", "chrypka", "pocenie_nocne", "trudnosci_w_oddychaniu", 
            "swistajacy_oddech", "bol_plec", "szybkie_oddychanie", "przyspieszony_oddech",
            "obrzek_wezlow_chlonnych", "trzeszczenia_u_podstawy_pluc", "sinica", 
            "kaszel_z_piana", "niepokój", "nawracajace_infekcje_pluc", 
            "trudnosci_w_przybieraniu_na_wadze", "odkrztuszanie_duzych_ilosci_plwociny",
            "zawroty_glowy", "omdlenia", "krwioplucie");

    public static final List<String> CZYNNIKI_RYZYKA = Arrays.asList(
            "palenie_tytoniu", "narazenie_na_azbestowe", "przewlekle_zapalenie_oskrzeli", 
            "zakazone_srodowisko_pracy", "obesity", "wiek_powyzej_50", 
            "narazenie_na_alergeny", "zakrzepica_zylna", "zaburzenia_polykania");
    
    // Mapa tłumaczeń objawów na język polski (do wyjaśnień)
    public static String translateSymptom(String symptom) {
        switch(symptom) {
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
            default: return symptom.replace("_", " ");
        }
    }
    
    // Mapa tłumaczeń czynników ryzyka na język polski (do wyjaśnień)
    public static String translateRiskFactor(String factor) {
        switch(factor) {
            case "palenie_tytoniu": return "palenie tytoniu";
            case "narazenie_na_azbestowe": return "narażenie na azbest";
            case "przewlekle_zapalenie_oskrzeli": return "przewlekłe zapalenie oskrzeli";
            case "zakazone_srodowisko_pracy": return "zakażone środowisko pracy";
            case "obesity": return "otyłość";
            case "wiek_powyzej_50": return "wiek powyżej 50 lat";
            case "narazenie_na_alergeny": return "narażenie na alergeny";
            case "zakrzepica_zylna": return "zakrzepica żylna";
            case "zaburzenia_polykania": return "zaburzenia połykania";
            default: return factor.replace("_", " ");
        }
    }
    
    // Metoda tłumacząca nazwę choroby na bardziej przyjazną formę
    public static String translateDiseaseName(String disease) {
        return disease.replace("_", " ");
    }
}