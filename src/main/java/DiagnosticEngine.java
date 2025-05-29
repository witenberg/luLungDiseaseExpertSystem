package main.java;

import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.jpl7.Query;
import org.jpl7.Term;

public class DiagnosticEngine {
    
    public DiagnosticEngine() {
        // Inicjalizacja silnika Prologowego
        Query loadFile = new Query("consult('src/main/prolog/lung_disease.pl')");
        if (!loadFile.hasSolution()) {
            throw new RuntimeException("Błąd: Nie udało się załadować pliku lung_disease.pl");
        }
    }
    
    public DiagnosticResult diagnose(List<String> symptoms, List<String> riskFactors) {
        // Czyszczenie poprzednich faktów
        new Query("retractall(objaw(_, _))").hasSolution();
        new Query("retractall(czynnik_ryzyka(_, _))").hasSolution();

        // Dodawanie nowych faktów (bez wag, ponieważ są one zdefiniowane w bazie wiedzy)
        for (String objaw : symptoms) {
            new Query("assertz(objaw(" + objaw + "))").hasSolution();
        }
        for (String czynnik : riskFactors) {
            new Query("assertz(czynnik_ryzyka(" + czynnik + "))").hasSolution();
        }

        // Tworzenie list w formacie Prologu
        String objawyList = symptoms.isEmpty() ? "[]" : "[" + String.join(",", symptoms) + "]";
        String czynnikiList = riskFactors.isEmpty() ? "[]" : "[" + String.join(",", riskFactors) + "]";

        // Zapytanie diagnostyczne
        Query diagnostyka = new Query("diagnozuj(Choroba, " + objawyList + ", " + czynnikiList + ", Dopasowanie)");

        // Lista do przechowywania wyników
        List<Map.Entry<String, Double>> diagnozyList = new ArrayList<>();
        Map<String, DiseaseExplanation> explanations = new HashMap<>();
        
        if (diagnostyka.hasSolution()) {
            while (diagnostyka.hasMoreSolutions()) {
                Map<String, Term> wynik = diagnostyka.nextSolution();
                String chorobaNazwa = wynik.get("Choroba").toString();
                String chorobaDisplay = DiagnosticData.translateDiseaseName(chorobaNazwa);
                double dopasowanie = Double.parseDouble(wynik.get("Dopasowanie").toString());
                
                diagnozyList.add(new AbstractMap.SimpleEntry<>(chorobaDisplay, dopasowanie));
                
                // Pobieranie danych do mechanizmu wyjaśniającego
                explanations.put(chorobaDisplay, getExplanationForDisease(chorobaNazwa, symptoms, riskFactors));
            }
            
            // Sortowanie wyników według dopasowania (malejąco)
            Collections.sort(diagnozyList, (a, b) -> Double.compare(b.getValue(), a.getValue()));
        }
        
        return new DiagnosticResult(symptoms, riskFactors, diagnozyList, explanations);
    }
    
    private DiseaseExplanation getExplanationForDisease(String diseaseName, List<String> providedSymptoms, List<String> providedRiskFactors) {
        // Pobranie wszystkich wymaganych objawów dla choroby wraz z ich wagami
        Query symptomQuery = new Query("choroba(" + diseaseName + ", Wymagane), findall(X-W, (member(objaw(X, W), Wymagane)), WymaganeObjawyZWagami)");
        Map<String, Term> symptomResult = symptomQuery.oneSolution();
        List<String> requiredSymptoms = parseWeightedTerms(symptomResult.get("WymaganeObjawyZWagami"));
        
        // Pobranie wszystkich wymaganych czynników ryzyka dla choroby wraz z ich wagami
        Query riskQuery = new Query("choroba(" + diseaseName + ", Wymagane), findall(Y-W, (member(czynnik_ryzyka(Y, W), Wymagane)), WymaganeCzynnikiZWagami)");
        Map<String, Term> riskResult = riskQuery.oneSolution();
        List<String> requiredRiskFactors = parseWeightedTerms(riskResult.get("WymaganeCzynnikiZWagami"));
        
        // Znalezienie pasujących objawów
        List<String> matchingSymptoms = providedSymptoms.stream()
                .filter(symptom -> requiredSymptoms.stream()
                        .anyMatch(req -> req.startsWith(symptom + "-")))
                .collect(Collectors.toList());
        
        // Znalezienie pasujących czynników ryzyka
        List<String> matchingRiskFactors = providedRiskFactors.stream()
                .filter(factor -> requiredRiskFactors.stream()
                        .anyMatch(req -> req.startsWith(factor + "-")))
                .collect(Collectors.toList());
        
        return new DiseaseExplanation(
                diseaseName,
                requiredSymptoms,
                requiredRiskFactors,
                matchingSymptoms,
                matchingRiskFactors
        );
    }
    
    // Nowa metoda do parsowania terminów z wagami
    private List<String> parseWeightedTerms(Term listTerm) {
        List<String> result = new ArrayList<>();
        
        // Jeśli lista jest pusta, zwróć pustą listę
        if (listTerm.toString().equals("[]")) {
            return result;
        }
        
        // Inaczej parsuj elementy listy
        Term currentTerm = listTerm;
        while (currentTerm.arity() == 2) {
            Term pair = currentTerm.arg(1);
            if (pair.arity() == 2) {
                String name = pair.arg(1).toString();
                String weight = pair.arg(2).toString();
                result.add(name + "-" + weight);
            }
            currentTerm = currentTerm.arg(2);
        }
        
        return result;
    }
}