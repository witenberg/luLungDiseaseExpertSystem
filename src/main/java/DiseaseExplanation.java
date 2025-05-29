package main.java;

import java.util.ArrayList;
import java.util.List;

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
}
