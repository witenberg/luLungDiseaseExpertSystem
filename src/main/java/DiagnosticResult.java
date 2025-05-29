package main.java;

import java.util.List;
import java.util.Map;

public class DiagnosticResult {
    private List<String> selectedSymptoms;
    private List<String> selectedRiskFactors;
    private List<Map.Entry<String, Double>> diagnoses;
    private Map<String, DiseaseExplanation> explanations;
    
    public DiagnosticResult(
            List<String> selectedSymptoms,
            List<String> selectedRiskFactors,
            List<Map.Entry<String, Double>> diagnoses,
            Map<String, DiseaseExplanation> explanations) {
        this.selectedSymptoms = selectedSymptoms;
        this.selectedRiskFactors = selectedRiskFactors;
        this.diagnoses = diagnoses;
        this.explanations = explanations;
    }
    
    public String getResultText() {
        StringBuilder wyniki = new StringBuilder();
        wyniki.append("Wybrane objawy: ").append(selectedSymptoms.isEmpty() ? "brak" : selectedSymptoms.toString()).append("\n");
        wyniki.append("Wybrane czynniki ryzyka: ").append(selectedRiskFactors.isEmpty() ? "brak" : selectedRiskFactors.toString()).append("\n\n");
        wyniki.append("Możliwe diagnozy:\n");
        
        if (diagnoses.isEmpty()) {
            wyniki.append("Nie można postawić diagnozy na podstawie podanych objawów.\n");
        } else {
            for (Map.Entry<String, Double> entry : diagnoses) {
                wyniki.append("- ").append(entry.getKey()).append(" (Dopasowanie: ")
                      .append(String.format("%.2f", entry.getValue())).append("%)\n");
            }
        }
        
        return wyniki.toString();
    }
    
    public String getExplanationText() {
        if (diagnoses.isEmpty()) {
            return "Brak dopasowań do wyjaśnienia.";
        }
        
        // Wybieramy najlepsze dopasowanie do wyjaśnienia
        String topDisease = diagnoses.get(0).getKey();
        DiseaseExplanation explanation = explanations.get(topDisease);
        
        StringBuilder explanationText = new StringBuilder();
        explanationText.append("Wyjaśnienie dla: ").append(topDisease).append("\n\n");
        
        explanationText.append("Choroba ").append(topDisease).append(" została zidentyfikowana z dopasowaniem ")
                      .append(String.format("%.2f", diagnoses.get(0).getValue())).append("% ponieważ:\n\n");
        
        // Wyjaśniamy pasujące objawy
        explanationText.append("Pasujące objawy (").append(explanation.getMatchingSymptoms().size()).append("/")
                      .append(explanation.getRequiredSymptoms().size()).append("):\n");
        
        for (String symptom : explanation.getMatchingSymptoms()) {
            explanationText.append("✓ ").append(DiagnosticData.translateSymptom(symptom)).append("\n");
        }
        
        // Wyjaśniamy brakujące objawy
        List<String> missingSymptoms = explanation.getMissingSymptoms();
        if (!missingSymptoms.isEmpty()) {
            explanationText.append("\nBrakujące objawy:\n");
            for (String symptom : missingSymptoms) {
                explanationText.append("✗ ").append(DiagnosticData.translateSymptom(symptom)).append("\n");
            }
        }
        
        // Wyjaśniamy pasujące czynniki ryzyka
        if (!explanation.getRequiredRiskFactors().isEmpty()) {
            explanationText.append("\nPasujące czynniki ryzyka (").append(explanation.getMatchingRiskFactors().size()).append("/")
                          .append(explanation.getRequiredRiskFactors().size()).append("):\n");
            
            for (String factor : explanation.getMatchingRiskFactors()) {
                explanationText.append("✓ ").append(DiagnosticData.translateRiskFactor(factor)).append("\n");
            }
            
            // Wyjaśniamy brakujące czynniki ryzyka
            List<String> missingFactors = explanation.getMissingRiskFactors();
            if (!missingFactors.isEmpty()) {
                explanationText.append("\nBrakujące czynniki ryzyka:\n");
                for (String factor : missingFactors) {
                    explanationText.append("✗ ").append(DiagnosticData.translateRiskFactor(factor)).append("\n");
                }
            }
        }
        
        return explanationText.toString();
    }
}