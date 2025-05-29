package main.java;

import java.util.List;
import java.util.Map;

public class DiagnosticResult {
    private List<String> providedSymptoms;
    private List<String> providedRiskFactors;
    private List<Map.Entry<String, Double>> diagnoses;
    private Map<String, DiseaseExplanation> explanations;
    
    public DiagnosticResult(List<String> providedSymptoms, List<String> providedRiskFactors,
            List<Map.Entry<String, Double>> diagnoses, Map<String, DiseaseExplanation> explanations) {
        this.providedSymptoms = providedSymptoms;
        this.providedRiskFactors = providedRiskFactors;
        this.diagnoses = diagnoses;
        this.explanations = explanations;
    }
    
    public String getResultText() {
        StringBuilder result = new StringBuilder();
        
        if (diagnoses.isEmpty()) {
            result.append("Nie znaleziono pasujących chorób dla podanych objawów i czynników ryzyka.");
            return result.toString();
        }
        
        // Pobierz najbardziej prawdopodobną diagnozę
        Map.Entry<String, Double> mostProbableDiagnosis = diagnoses.get(0);
        String diseaseName = mostProbableDiagnosis.getKey();
        double probability = mostProbableDiagnosis.getValue();
        
        result.append("Najbardziej prawdopodobna diagnoza:\n");
        result.append(diseaseName).append(" - ").append(String.format("%.2f", probability)).append("%\n\n");
        
        // Dodaj informację o innych możliwych diagnozach, jeśli istnieją
        if (diagnoses.size() > 1) {
            result.append("Inne możliwe diagnozy:\n");
            for (int i = 1; i < diagnoses.size(); i++) {
                Map.Entry<String, Double> diagnosis = diagnoses.get(i);
                result.append("- ").append(diagnosis.getKey())
                      .append(" - ").append(String.format("%.2f", diagnosis.getValue()))
                      .append("%\n");
            }
        }
        
        return result.toString();
    }
    
    public String getExplanationText() {
        if (diagnoses.isEmpty()) {
            return "Brak wyjaśnień - nie znaleziono pasujących chorób.";
        }
        
        // Pobierz wyjaśnienie dla najbardziej prawdopodobnej diagnozy
        String mostProbableDisease = diagnoses.get(0).getKey();
        DiseaseExplanation explanation = explanations.get(mostProbableDisease);
        
        return explanation != null ? explanation.getExplanationText() : "Brak wyjaśnienia dla tej diagnozy.";
    }

    public List<String> getProvidedSymptoms() {
        return providedSymptoms;
    }

    public List<String> getProvidedRiskFactors() {
        return providedRiskFactors;
    }

    public List<Map.Entry<String, Double>> getDiagnoses() {
        return diagnoses;
    }

    public Map<String, DiseaseExplanation> getExplanations() {
        return explanations;
    }
}