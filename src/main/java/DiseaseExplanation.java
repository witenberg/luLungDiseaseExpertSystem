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
    private List<String> keySymptoms;
    private List<String> excludingSymptoms;
    
    public DiseaseExplanation(
            String diseaseName,
            List<String> requiredSymptoms,
            List<String> requiredRiskFactors,
            List<String> matchingSymptoms,
            List<String> matchingRiskFactors,
            List<String> keySymptoms,
            List<String> excludingSymptoms) {
        this.diseaseName = diseaseName;
        this.requiredSymptoms = requiredSymptoms;
        this.requiredRiskFactors = requiredRiskFactors;
        this.matchingSymptoms = matchingSymptoms;
        this.matchingRiskFactors = matchingRiskFactors;
        this.keySymptoms = keySymptoms;
        this.excludingSymptoms = excludingSymptoms;
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
    
    public List<String> getKeySymptoms() {
        return keySymptoms;
    }
    
    public List<String> getExcludingSymptoms() {
        return excludingSymptoms;
    }
    
    public List<String> getMissingSymptoms() {
        List<String> missing = new ArrayList<>(requiredSymptoms);
        missing.removeAll(matchingSymptoms);
        return missing;
    }
    
    public List<String> getMissingRiskFactors() {
        List<String> missing = new ArrayList<>(requiredRiskFactors);
        missing.removeAll(matchingRiskFactors);
        return missing;
    }

    public String getExplanationText() {
        StringBuilder sb = new StringBuilder();
        sb.append("Wyjaśnienie diagnozy:\n\n");

        if (!keySymptoms.isEmpty()) {
            sb.append("OBJAWY KLUCZOWE (wysokie prawdopodobieństwo choroby przy ich obecności):\n");
            for (String symptom : keySymptoms) {
                String symptomName = symptom.split("-")[0];
                String weight = symptom.split("-")[1];
                boolean isPresent = matchingSymptoms.stream()
                        .anyMatch(s -> s.startsWith(symptomName + "-"));
                
                sb.append(String.format("[%s] %s (waga: %s)%s\n",
                    isPresent ? "+" : "-",
                    DiagnosticData.translateSymptom(symptomName),
                    weight,
                    isPresent ? "" : " - BRAK TEGO OBJAWU!"));
            }
            sb.append("\n");
        }

        if (!excludingSymptoms.isEmpty()) {
            sb.append("OBJAWY WYKLUCZAJĄCE (obecność któregokolwiek wyklucza diagnozę):\n");
            for (String symptom : excludingSymptoms) {
                boolean isPresent = matchingSymptoms.stream()
                        .anyMatch(s -> s.startsWith(symptom + "-"));
                
                sb.append(String.format("[%s] %s%s\n",
                    isPresent ? "+" : "-",
                    DiagnosticData.translateSymptom(symptom),
                    isPresent ? " - OBECNY! Diagnoza wykluczona!" : ""));
            }
            sb.append("\n");
        }

        if (!matchingSymptoms.isEmpty()) {
            sb.append("PASUJĄCE OBJAWY:\n");
            for (String symptom : matchingSymptoms) {
                String[] parts = symptom.split("-");
                String symptomName = parts[0];
                String weight = parts.length > 1 ? parts[1] : "1.0";
                sb.append(String.format("+ %s (waga: %s)\n",
                    DiagnosticData.translateSymptom(symptomName),
                    weight));
            }
            sb.append("\n");
        }

        List<String> missingSymptoms = requiredSymptoms.stream()
                .filter(symptom -> !matchingSymptoms.contains(symptom))
                .collect(Collectors.toList());
        if (!missingSymptoms.isEmpty()) {
            sb.append("BRAKUJĄCE OBJAWY:\n");
            for (String symptom : missingSymptoms) {
                String[] parts = symptom.split("-");
                String symptomName = parts[0];
                String weight = parts.length > 1 ? parts[1] : "1.0";
                sb.append(String.format("- %s (waga: %s)\n",
                    DiagnosticData.translateSymptom(symptomName),
                    weight));
            }
            sb.append("\n");
        }

        if (!matchingRiskFactors.isEmpty() || !requiredRiskFactors.isEmpty()) {
            sb.append("CZYNNIKI RYZYKA:\n");
            for (String factor : matchingRiskFactors) {
                String[] parts = factor.split("-");
                String factorName = parts[0];
                String weight = parts.length > 1 ? parts[1] : "1.0";
                sb.append(String.format("+ %s (waga: %s)\n",
                    DiagnosticData.translateRiskFactor(factorName),
                    weight));
            }
            for (String factor : requiredRiskFactors) {
                if (!matchingRiskFactors.contains(factor)) {
                    String[] parts = factor.split("-");
                    String factorName = parts[0];
                    String weight = parts.length > 1 ? parts[1] : "1.0";
                    sb.append(String.format("- %s (waga: %s)\n",
                        DiagnosticData.translateRiskFactor(factorName),
                        weight));
                }
            }
        }

        return sb.toString();
    }
}
