package main.java;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class DiseaseExplanation {
    private String diseaseName;
    private List<String> requiredSymptoms;
    private List<String> requiredRiskFactors;
    private List<String> matchingSymptoms;
    private List<String> matchingRiskFactors;
    
    public DiseaseExplanation(
            String diseaseName,
            List<String> requiredSymptoms,
            List<String> requiredRiskFactors,
            List<String> matchingSymptoms,
            List<String> matchingRiskFactors) {
        this.diseaseName = diseaseName;
        this.requiredSymptoms = requiredSymptoms;
        this.requiredRiskFactors = requiredRiskFactors;
        this.matchingSymptoms = matchingSymptoms;
        this.matchingRiskFactors = matchingRiskFactors;
    }
    
    public String getDiseaseName() {
        return diseaseName;
    }
    
    public List<String> getRequiredSymptoms() {
        return requiredSymptoms;
    }
    
    public List<String> getRequiredRiskFactors() {
        return requiredRiskFactors;
    }
    
    public List<String> getMatchingSymptoms() {
        return matchingSymptoms;
    }
    
    public List<String> getMatchingRiskFactors() {
        return matchingRiskFactors;
    }
    
    // Metoda zwracająca listę brakujących objawów
    public List<String> getMissingSymptoms() {
        List<String> missing = new ArrayList<>(requiredSymptoms);
        missing.removeAll(matchingSymptoms);
        return missing;
    }
    
    // Metoda zwracająca listę brakujących czynników ryzyka
    public List<String> getMissingRiskFactors() {
        List<String> missing = new ArrayList<>(requiredRiskFactors);
        missing.removeAll(matchingRiskFactors);
        return missing;
    }

    public String getExplanationText() {
        StringBuilder explanation = new StringBuilder();
        
        // Dodanie nazwy choroby
        explanation.append("Wyjaśnienie dla: ").append(DiagnosticData.translateDiseaseName(diseaseName)).append("\n\n");
        
        // Dodanie informacji o objawach
        List<String> matchingSymptomNames = new ArrayList<>();
        List<String> missingSymptomNames = new ArrayList<>();
        
        for (String symptom : requiredSymptoms) {
            String[] parts = symptom.split("-");
            String symptomName = parts[0];
            double weight = parts.length > 1 ? Double.parseDouble(parts[1]) : 1.0;
            String weightText = weight >= 0.9 ? " (bardzo ważny)" : 
                              weight >= 0.7 ? " (ważny)" : 
                              weight >= 0.5 ? " (istotny)" : " (pomocniczy)";
            
            String symptomText = DiagnosticData.translateSymptom(symptomName) + weightText;
            
            if (matchingSymptoms.contains(symptomName)) {
                matchingSymptomNames.add("[+] " + symptomText);
            } else {
                missingSymptomNames.add("[-] " + symptomText);
            }
        }
        
        explanation.append("Pasujące objawy (").append(matchingSymptomNames.size())
                  .append("/").append(requiredSymptoms.size()).append("):\n");
        if (!matchingSymptomNames.isEmpty()) {
            for (String symptom : matchingSymptomNames) {
                explanation.append(symptom).append("\n");
            }
        } else {
            explanation.append("Brak pasujących objawów\n");
        }
        
        explanation.append("\nBrakujące objawy:\n");
        if (!missingSymptomNames.isEmpty()) {
            for (String symptom : missingSymptomNames) {
                explanation.append(symptom).append("\n");
            }
        } else {
            explanation.append("Brak brakujących objawów\n");
        }
        
        // Dodanie informacji o czynnikach ryzyka
        if (!requiredRiskFactors.isEmpty()) {
            List<String> matchingFactorNames = new ArrayList<>();
            List<String> missingFactorNames = new ArrayList<>();
            
            for (String factor : requiredRiskFactors) {
                String[] parts = factor.split("-");
                String factorName = parts[0];
                double weight = parts.length > 1 ? Double.parseDouble(parts[1]) : 1.0;
                String weightText = weight >= 0.9 ? " (bardzo ważny)" : 
                                  weight >= 0.7 ? " (ważny)" : 
                                  weight >= 0.5 ? " (istotny)" : " (pomocniczy)";
                
                String factorText = DiagnosticData.translateRiskFactor(factorName) + weightText;
                
                if (matchingRiskFactors.contains(factorName)) {
                    matchingFactorNames.add("[+] " + factorText);
                } else {
                    missingFactorNames.add("[ ] " + factorText);
                }
            }
            
            explanation.append("\nPasujące czynniki ryzyka (").append(matchingFactorNames.size())
                      .append("/").append(requiredRiskFactors.size()).append("):\n");
            if (!matchingFactorNames.isEmpty()) {
                for (String factor : matchingFactorNames) {
                    explanation.append(factor).append("\n");
                }
            } else {
                explanation.append("Brak pasujących czynników ryzyka\n");
            }
            
            explanation.append("\nBrakujące czynniki ryzyka:\n");
            if (!missingFactorNames.isEmpty()) {
                for (String factor : missingFactorNames) {
                    explanation.append(factor).append("\n");
                }
            } else {
                explanation.append("Brak brakujących czynników ryzyka\n");
            }
        }
        
        return explanation.toString();
    }
}
