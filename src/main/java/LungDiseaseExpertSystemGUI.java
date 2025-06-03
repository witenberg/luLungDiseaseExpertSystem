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
import java.awt.Color;
import javax.swing.JSplitPane;
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
        JPanel mainPanel = new JPanel();
        mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));
        mainPanel.setBackground(BG_COLOR);
        mainPanel.setBorder(BorderFactory.createEmptyBorder(15, 15, 15, 15));

        // Nagłówek objawów wyrównany do lewej
        JPanel objawyHeaderPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
        objawyHeaderPanel.setBackground(BG_COLOR);
        JLabel tytulObjawy = createHeaderLabel("Wybierz objawy:");
        objawyHeaderPanel.add(tytulObjawy);
        mainPanel.add(objawyHeaderPanel);
        JScrollPane objawyScrollPane = createSymptomsPanel();
        mainPanel.add(objawyScrollPane);

        // Nagłówek czynników ryzyka wyrównany do lewej
        JPanel czynnikiHeaderPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
        czynnikiHeaderPanel.setBackground(BG_COLOR);
        JLabel tytulCzynniki = createHeaderLabel("Wybierz czynniki ryzyka:");
        czynnikiHeaderPanel.add(tytulCzynniki);
        mainPanel.add(czynnikiHeaderPanel);
        JScrollPane czynnikRyzykaScrollPane = createRiskFactorsPanel();
        mainPanel.add(czynnikRyzykaScrollPane);

        JPanel buttonPanel = createButtonPanel();
        mainPanel.add(buttonPanel);

        // Panel z wynikami i wyjaśnieniem obok siebie
        JPanel resultsAndExplanationPanel = new JPanel(new BorderLayout());

        // Panele z nagłówkami i polami tekstowymi
        JPanel leftPanel = new JPanel(new BorderLayout());
        leftPanel.setBackground(BG_COLOR);
        JPanel leftHeaderPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
        leftHeaderPanel.setBackground(BG_COLOR);
        JLabel tytulWyniki = createHeaderLabel("Wyniki diagnozy:");
        leftHeaderPanel.add(tytulWyniki);
        leftPanel.add(leftHeaderPanel, BorderLayout.NORTH);
        JScrollPane wynikScrollPane = createResultsPanel();
        wynikScrollPane.setPreferredSize(new Dimension(900, 400));
        leftPanel.add(wynikScrollPane, BorderLayout.CENTER);
        leftPanel.setMinimumSize(new Dimension(400, 400));
        leftPanel.setPreferredSize(new Dimension(900, 400));

        JPanel rightPanel = new JPanel(new BorderLayout());
        rightPanel.setBackground(BG_COLOR);
        JPanel rightHeaderPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
        rightHeaderPanel.setBackground(BG_COLOR);
        JLabel tytulWyjasnienia = createHeaderLabel("Wyjaśnienie diagnozy:");
        rightHeaderPanel.add(tytulWyjasnienia);
        rightPanel.add(rightHeaderPanel, BorderLayout.NORTH);
        JScrollPane wyjasnienieScrollPane = createExplanationPanel();
        wyjasnienieScrollPane.setPreferredSize(new Dimension(900, 400));
        rightPanel.add(wyjasnienieScrollPane, BorderLayout.CENTER);
        rightPanel.setMinimumSize(new Dimension(400, 400));
        rightPanel.setPreferredSize(new Dimension(900, 400));

        JSplitPane splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, leftPanel, rightPanel);
        splitPane.setResizeWeight(0.5);
        splitPane.setDividerLocation(getWidth() / 2);
        splitPane.setBackground(BG_COLOR);
        splitPane.setBorder(null);
        resultsAndExplanationPanel.add(splitPane, BorderLayout.CENTER);
        resultsAndExplanationPanel.setPreferredSize(new Dimension(1800, 500));
        resultsAndExplanationPanel.setMaximumSize(new Dimension(Integer.MAX_VALUE, 700));

        mainPanel.add(resultsAndExplanationPanel);
        add(mainPanel);
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
    
    private JPanel createButtonPanel() {
        JPanel buttonPanel = new JPanel();
        buttonPanel.setBackground(BG_COLOR);
        buttonPanel.setBorder(BorderFactory.createEmptyBorder(10, 0, 10, 0));
        
        // Przycisk diagnozy
        JButton diagnozujButton = new JButton("Diagnozuj");
        diagnozujButton.setFont(BUTTON_FONT);
        diagnozujButton.setBackground(Color.DARK_GRAY);
        diagnozujButton.setForeground(Color.WHITE);
        diagnozujButton.setFocusPainted(false);
        diagnozujButton.setPreferredSize(new Dimension(150, 40));
        diagnozujButton.addActionListener(e -> przeprowadzDiagnoze());
        buttonPanel.add(diagnozujButton);
        
        // Przycisk wyczyszczenia wyboru
        JButton wyczyscButton = new JButton("Wyczyść wybór");
        wyczyscButton.setFont(BUTTON_FONT);
        wyczyscButton.setBackground(Color.LIGHT_GRAY);
        wyczyscButton.setForeground(Color.BLACK);
        wyczyscButton.setFocusPainted(false);
        wyczyscButton.setPreferredSize(new Dimension(170, 40));
        wyczyscButton.addActionListener(e -> wyczyscWybor());
        buttonPanel.add(wyczyscButton);
        
        return buttonPanel;
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