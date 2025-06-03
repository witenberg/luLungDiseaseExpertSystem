package main.java;

import java.awt.BorderLayout;
import java.awt.CardLayout;
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
import java.awt.Color;
import java.awt.FlowLayout;

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
    private CardLayout cardLayout;
    private JPanel mainPanel;
    private static final Color BG_COLOR = new Color(245, 245, 245);
    private static final Color HEADER_COLOR = new Color(30, 30, 30);
    private static final Color BORDER_COLOR = new Color(180, 180, 180);
    private static final Font HEADER_FONT = new Font("Arial", Font.BOLD, 22);
    private static final Font LABEL_FONT = new Font("Arial", Font.BOLD, 15);
    private static final Font CHECKBOX_FONT = new Font("Arial", Font.PLAIN, 15);
    private static final Font BUTTON_FONT = new Font("Arial", Font.BOLD, 16);
    private static final Font TEXTAREA_FONT = new Font("Consolas", Font.PLAIN, 16);

    public LungDiseaseExpertSystemGUI() {
        // Inicjalizacja silnika diagnostycznego
        diagnosticEngine = new DiagnosticEngine();
        // Konfiguracja okna
        configureFrame();
        // Tworzenie i układanie komponentów UI
        createUIComponents();
        pack();
        setExtendedState(JFrame.MAXIMIZED_BOTH);
    }

    private void configureFrame() {
        setTitle("System Ekspertowy - Choroby Płuc");
        setSize(2000, 1500);
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setLayout(new BorderLayout());
        setLocationRelativeTo(null);
        getContentPane().setBackground(BG_COLOR);
    }

    private void createUIComponents() {
        cardLayout = new CardLayout();
        mainPanel = new JPanel(cardLayout);
        mainPanel.setBackground(BG_COLOR);

        // Create the three main screens
        JPanel symptomsPanel = createSymptomsScreen();
        JPanel riskFactorsPanel = createRiskFactorsScreen();
        JPanel resultsPanel = createResultsScreen();

        // Add screens to card layout
        mainPanel.add(symptomsPanel, "SYMPTOMS");
        mainPanel.add(riskFactorsPanel, "RISK_FACTORS");
        mainPanel.add(resultsPanel, "RESULTS");

        add(mainPanel);
    }

    private JPanel createSymptomsScreen() {
        JPanel panel = new JPanel(new BorderLayout());
        panel.setBackground(BG_COLOR);
        panel.setBorder(BorderFactory.createEmptyBorder(20, 20, 20, 20));

        // Header
        JPanel headerPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        headerPanel.setBackground(BG_COLOR);
        JLabel headerLabel = createHeaderLabel("Wybierz objawy:");
        headerPanel.add(headerLabel);
        panel.add(headerPanel, BorderLayout.NORTH);

        // Symptoms checkboxes
        JScrollPane symptomsScrollPane = createSymptomsPanel();
        panel.add(symptomsScrollPane, BorderLayout.CENTER);

        // Navigation button
        JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
        buttonPanel.setBackground(BG_COLOR);
        JButton nextButton = new JButton("Dalej");
        nextButton.setFont(BUTTON_FONT);
        nextButton.setBackground(Color.DARK_GRAY);
        nextButton.setForeground(Color.WHITE);
        nextButton.setFocusPainted(false);
        nextButton.setPreferredSize(new Dimension(150, 40));
        nextButton.addActionListener(e -> cardLayout.show(mainPanel, "RISK_FACTORS"));
        buttonPanel.add(nextButton);
        panel.add(buttonPanel, BorderLayout.SOUTH);

        return panel;
    }

    private JPanel createRiskFactorsScreen() {
        JPanel panel = new JPanel(new BorderLayout());
        panel.setBackground(BG_COLOR);
        panel.setBorder(BorderFactory.createEmptyBorder(20, 20, 20, 20));

        // Header
        JPanel headerPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        headerPanel.setBackground(BG_COLOR);
        JLabel headerLabel = createHeaderLabel("Wybierz czynniki ryzyka:");
        headerPanel.add(headerLabel);
        panel.add(headerPanel, BorderLayout.NORTH);

        // Risk factors checkboxes
        JScrollPane riskFactorsScrollPane = createRiskFactorsPanel();
        panel.add(riskFactorsScrollPane, BorderLayout.CENTER);

        // Navigation buttons
        JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
        buttonPanel.setBackground(BG_COLOR);
        
        JButton backButton = new JButton("Wstecz");
        backButton.setFont(BUTTON_FONT);
        backButton.setBackground(Color.LIGHT_GRAY);
        backButton.setForeground(Color.BLACK);
        backButton.setFocusPainted(false);
        backButton.setPreferredSize(new Dimension(150, 40));
        backButton.addActionListener(e -> cardLayout.show(mainPanel, "SYMPTOMS"));
        
        JButton diagnoseButton = new JButton("Diagnozuj");
        diagnoseButton.setFont(BUTTON_FONT);
        diagnoseButton.setBackground(Color.DARK_GRAY);
        diagnoseButton.setForeground(Color.WHITE);
        diagnoseButton.setFocusPainted(false);
        diagnoseButton.setPreferredSize(new Dimension(150, 40));
        diagnoseButton.addActionListener(e -> {
            przeprowadzDiagnoze();
            cardLayout.show(mainPanel, "RESULTS");
        });

        buttonPanel.add(backButton);
        buttonPanel.add(diagnoseButton);
        panel.add(buttonPanel, BorderLayout.SOUTH);

        return panel;
    }

    private JPanel createResultsScreen() {
        JPanel panel = new JPanel(new BorderLayout());
        panel.setBackground(BG_COLOR);
        panel.setBorder(BorderFactory.createEmptyBorder(20, 20, 20, 20));

        // Results section
        JPanel resultsPanel = new JPanel(new BorderLayout());
        resultsPanel.setBackground(BG_COLOR);
        
        JPanel resultsHeaderPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        resultsHeaderPanel.setBackground(BG_COLOR);
        JLabel resultsLabel = createHeaderLabel("Wyniki diagnozy:");
        resultsHeaderPanel.add(resultsLabel);
        resultsPanel.add(resultsHeaderPanel, BorderLayout.NORTH);
        
        JScrollPane wynikScrollPane = createResultsPanel();
        resultsPanel.add(wynikScrollPane, BorderLayout.CENTER);

        // Explanation section
        JPanel explanationPanel = new JPanel(new BorderLayout());
        explanationPanel.setBackground(BG_COLOR);
        
        JPanel explanationHeaderPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        explanationHeaderPanel.setBackground(BG_COLOR);
        JLabel explanationLabel = createHeaderLabel("Wyjaśnienie diagnozy:");
        explanationHeaderPanel.add(explanationLabel);
        explanationPanel.add(explanationHeaderPanel, BorderLayout.NORTH);
        
        JScrollPane wyjasnienieScrollPane = createExplanationPanel();
        explanationPanel.add(wyjasnienieScrollPane, BorderLayout.CENTER);

        // Combine results and explanation
        JPanel contentPanel = new JPanel(new GridLayout(2, 1, 0, 20));
        contentPanel.setBackground(BG_COLOR);
        contentPanel.add(resultsPanel);
        contentPanel.add(explanationPanel);
        panel.add(contentPanel, BorderLayout.CENTER);

        // Navigation button
        JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
        buttonPanel.setBackground(BG_COLOR);
        JButton newDiagnosisButton = new JButton("Nowa diagnoza");
        newDiagnosisButton.setFont(BUTTON_FONT);
        newDiagnosisButton.setBackground(Color.DARK_GRAY);
        newDiagnosisButton.setForeground(Color.WHITE);
        newDiagnosisButton.setFocusPainted(false);
        newDiagnosisButton.setPreferredSize(new Dimension(150, 40));
        newDiagnosisButton.addActionListener(e -> {
            wyczyscWybor();
            cardLayout.show(mainPanel, "SYMPTOMS");
        });
        buttonPanel.add(newDiagnosisButton);
        panel.add(buttonPanel, BorderLayout.SOUTH);

        return panel;
    }
    
    private JLabel createHeaderLabel(String text) {
        JLabel label = new JLabel(text);
        label.setFont(HEADER_FONT);
        label.setForeground(HEADER_COLOR);
        label.setBorder(BorderFactory.createEmptyBorder(10, 0, 10, 0));
        return label;
    }
    
    private JScrollPane createSymptomsPanel() {
        JPanel objawyPanel = createCheckboxPanel("Objawy", DiagnosticData.OBJAWY);
        objawyCheckBoxes = getCheckBoxes(objawyPanel);
        
        JScrollPane scrollPane = new JScrollPane(objawyPanel);
        scrollPane.setPreferredSize(new Dimension(1000, 250));
        scrollPane.setBorder(BorderFactory.createLineBorder(BORDER_COLOR));
        scrollPane.getViewport().setBackground(BG_COLOR);
        return scrollPane;
    }
    
    private JScrollPane createRiskFactorsPanel() {
        JPanel czynnikRyzykaPanel = createCheckboxPanel("Czynniki Ryzyka", DiagnosticData.CZYNNIKI_RYZYKA);
        czynnikRyzykaCheckBoxes = getCheckBoxes(czynnikRyzykaPanel);
        
        JScrollPane scrollPane = new JScrollPane(czynnikRyzykaPanel);
        scrollPane.setPreferredSize(new Dimension(1000, 150));
        scrollPane.setBorder(BorderFactory.createLineBorder(BORDER_COLOR));
        scrollPane.getViewport().setBackground(BG_COLOR);
        return scrollPane;
    }
    
    private JScrollPane createResultsPanel() {
        wynikArea = new JTextArea(10, 90);
        wynikArea.setFont(TEXTAREA_FONT);
        wynikArea.setEditable(false);
        wynikArea.setLineWrap(true);
        wynikArea.setWrapStyleWord(true);
        wynikArea.setBackground(Color.WHITE);
        wynikArea.setBorder(BorderFactory.createLineBorder(BORDER_COLOR));
        return new JScrollPane(wynikArea);
    }
    
    private JScrollPane createExplanationPanel() {
        wyjasnienieArea = new JTextArea(8, 90);
        wyjasnienieArea.setFont(TEXTAREA_FONT);
        wyjasnienieArea.setEditable(false);
        wyjasnienieArea.setLineWrap(true);
        wyjasnienieArea.setWrapStyleWord(true);
        wyjasnienieArea.setBackground(Color.WHITE);
        wyjasnienieArea.setBorder(BorderFactory.createLineBorder(BORDER_COLOR));
        return new JScrollPane(wyjasnienieArea);
    }

    private JPanel createCheckboxPanel(String title, List<String> elements) {
        JPanel panel = new JPanel();
        panel.setBorder(BorderFactory.createTitledBorder(BorderFactory.createLineBorder(BORDER_COLOR), title, 0, 0, LABEL_FONT, HEADER_COLOR));
        int columns = 6;
        panel.setLayout(new GridLayout(0, columns, 12, 10));
        panel.setBackground(BG_COLOR);
        for (String element : elements) {
            String displayText;
            if (title.equals("Objawy")) {
                displayText = DiagnosticData.translateSymptom(element);
            } else {
                displayText = DiagnosticData.translateRiskFactor(element);
            }
            JCheckBox checkBox = new JCheckBox(displayText);
            checkBox.setFont(CHECKBOX_FONT);
            checkBox.setBackground(BG_COLOR);
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