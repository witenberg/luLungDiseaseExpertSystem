package main.java;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridLayout;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

public class LungDiseaseExpertSystemGUI extends JFrame {
    /**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private List<JCheckBox> objawyCheckBoxes;
    private List<JCheckBox> czynnikRyzykaCheckBoxes;
    private JTextArea wynikArea;
    private JTextArea wyjasnienieArea;
    private DiagnosticEngine diagnosticEngine;

    public LungDiseaseExpertSystemGUI() {
        // Inicjalizacja silnika diagnostycznego
        diagnosticEngine = new DiagnosticEngine();
        
        // Konfiguracja okna
        configureFrame();
        
        // Tworzenie i układanie komponentów UI
        createUIComponents();
    }

    private void configureFrame() {
        setTitle("System Ekspertowy - Choroby Płuc");
        setSize(900, 900);
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setLayout(new BorderLayout());
        setLocationRelativeTo(null);
    }

    private void createUIComponents() {
        // Panel główny z układem pionowym
        JPanel mainPanel = new JPanel();
        mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));
        
        // Dodanie paneli objawów
        JLabel tytulObjawy = createHeaderLabel("Wybierz objawy:");
        mainPanel.add(tytulObjawy);
        
        JScrollPane objawyScrollPane = createSymptomsPanel();
        mainPanel.add(objawyScrollPane);
        
        // Dodanie paneli czynników ryzyka
        JLabel tytulCzynniki = createHeaderLabel("Wybierz czynniki ryzyka:");
        mainPanel.add(tytulCzynniki);
        
        JScrollPane czynnikRyzykaScrollPane = createRiskFactorsPanel();
        mainPanel.add(czynnikRyzykaScrollPane);
        
        // Dodanie panelu przycisków
        JPanel buttonPanel = createButtonPanel();
        mainPanel.add(buttonPanel);
        
        // Dodanie panelu wyników
        JLabel tytulWyniki = createHeaderLabel("Wyniki diagnozy:");
        tytulWyniki.setFont(new Font("Arial", Font.BOLD, 27));
        mainPanel.add(tytulWyniki);
        
        JScrollPane wynikScrollPane = createResultsPanel();
        mainPanel.add(wynikScrollPane);
        
        // Dodanie panelu wyjaśnień
        JLabel tytulWyjasnienia = createHeaderLabel("Wyjaśnienie diagnozy:");
        tytulWyjasnienia.setFont(new Font("Arial", Font.BOLD, 16));
        mainPanel.add(tytulWyjasnienia);
        
        JScrollPane wyjasnienieScrollPane = createExplanationPanel();
        mainPanel.add(wyjasnienieScrollPane);
        
        // Dodanie panelu głównego do okna
        add(mainPanel);
        pack();
    }
    
    private JLabel createHeaderLabel(String text) {
        JLabel label = new JLabel(text);
        label.setAlignmentX(Component.CENTER_ALIGNMENT);
        label.setFont(new Font("Arial", Font.BOLD, 14));
        return label;
    }
    
    private JScrollPane createSymptomsPanel() {
        JPanel objawyPanel = createCheckboxPanel("Objawy", DiagnosticData.OBJAWY);
        objawyCheckBoxes = getCheckBoxes(objawyPanel);
        
        JScrollPane scrollPane = new JScrollPane(objawyPanel);
        scrollPane.setPreferredSize(new Dimension(800, 250));
        return scrollPane;
    }
    
    private JScrollPane createRiskFactorsPanel() {
        JPanel czynnikRyzykaPanel = createCheckboxPanel("Czynniki Ryzyka", DiagnosticData.CZYNNIKI_RYZYKA);
        czynnikRyzykaCheckBoxes = getCheckBoxes(czynnikRyzykaPanel);
        
        JScrollPane scrollPane = new JScrollPane(czynnikRyzykaPanel);
        scrollPane.setPreferredSize(new Dimension(800, 150));
        return scrollPane;
    }
    
    private JPanel createButtonPanel() {
        JPanel buttonPanel = new JPanel();
        
        // Przycisk diagnozy
        JButton diagnozujButton = new JButton("Diagnozuj");
        diagnozujButton.addActionListener(e -> przeprowadzDiagnoze());
        buttonPanel.add(diagnozujButton);
        
        // Przycisk wyczyszczenia wyboru
        JButton wyczyscButton = new JButton("Wyczyść wybór");
        wyczyscButton.addActionListener(e -> wyczyscWybor());
        buttonPanel.add(wyczyscButton);
        
        return buttonPanel;
    }
    
    private JScrollPane createResultsPanel() {
        wynikArea = new JTextArea(15, 70);
        wynikArea.setEditable(false);
        return new JScrollPane(wynikArea);
    }
    
    private JScrollPane createExplanationPanel() {
        wyjasnienieArea = new JTextArea(10, 70);
        wyjasnienieArea.setEditable(false);
        return new JScrollPane(wyjasnienieArea);
    }

    private JPanel createCheckboxPanel(String title, List<String> elements) {
        JPanel panel = new JPanel();
        panel.setBorder(BorderFactory.createTitledBorder(title));
        int columns = 3; // Więcej kolumn dla lepszej organizacji
        panel.setLayout(new GridLayout(0, columns));

        for (String element : elements) {
            JCheckBox checkBox = new JCheckBox(element.replace("_", " "));
            checkBox.setActionCommand(element);
            panel.add(checkBox);
        }

        return panel;
    }

    private List<JCheckBox> getCheckBoxes(JPanel panel) {
        List<JCheckBox> checkBoxes = new ArrayList<>();
        for (Component comp : panel.getComponents()) {
            if (comp instanceof JCheckBox) {
                checkBoxes.add((JCheckBox) comp);
            }
        }
        return checkBoxes;
    }

    private void wyczyscWybor() {
        // Odznaczenie wszystkich checkboxów
        for (JCheckBox checkbox : objawyCheckBoxes) {
            checkbox.setSelected(false);
        }
        for (JCheckBox checkbox : czynnikRyzykaCheckBoxes) {
            checkbox.setSelected(false);
        }
        wynikArea.setText("");
        wyjasnienieArea.setText("");
    }

    private void przeprowadzDiagnoze() {
        List<String> wybraneObjawy = new ArrayList<>();
        List<String> wybraneCzynniki = new ArrayList<>();

        // Zbieranie zaznaczonych objawów
        for (JCheckBox checkbox : objawyCheckBoxes) {
            if (checkbox.isSelected()) {
                wybraneObjawy.add(checkbox.getActionCommand());
            }
        }

        // Zbieranie zaznaczonych czynników ryzyka
        for (JCheckBox checkbox : czynnikRyzykaCheckBoxes) {
            if (checkbox.isSelected()) {
                wybraneCzynniki.add(checkbox.getActionCommand());
            }
        }

        // Sprawdzenie czy cokolwiek zostało wybrane
        if (wybraneObjawy.isEmpty() && wybraneCzynniki.isEmpty()) {
            wynikArea.setText("Proszę wybrać przynajmniej jeden objaw lub czynnik ryzyka.");
            wyjasnienieArea.setText("");
            return;
        }

        try {
            // Wykonanie diagnozy
            DiagnosticResult diagnosticResult = diagnosticEngine.diagnose(wybraneObjawy, wybraneCzynniki);
            
            // Wyświetlenie wyników
            wynikArea.setText(diagnosticResult.getResultText());
            
            // Wyświetlenie wyjaśnień
            wyjasnienieArea.setText(diagnosticResult.getExplanationText());
            
        } catch (Exception e) {
            wynikArea.setText("Błąd diagnostyczny: " + e.toString());
            wyjasnienieArea.setText("");
            e.printStackTrace();
        }
    }
}