package main.java;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.GridLayout;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.SwingUtilities;

import org.jpl7.Query;
import org.jpl7.Term;

public class LungDiseaseExpertSystemGUI extends JFrame {
	private List<JCheckBox> objawyCheckBoxes;
	private List<JCheckBox> czynnikRyzykaCheckBoxes;
	private JTextArea wynikArea;

	private static final List<String> OBJAWY = Arrays.asList("kaszel", "suchy_kaszel", "mokry_kaszel", "kaszel_z_krwia",
			"goraczka", "dusznosc", "bol_w_klatce_piersiowej", "zmeczenie", "utrata_masy_ciala", "odpluwanie_krwi",
			"chrypka", "pocenie_nocne", "trudnosci_w_oddychaniu", "swistajacy_oddech", "bol_plec");

	private static final List<String> CZYNNIKI_RYZYKA = Arrays.asList("palenie_tytoniu", "narazenie_na_azbestowe",
			"przewlekle_zapalenie_oskrzeli", "zakazone_srodowisko_pracy", "obesity", "wiek_powyzej_50");

	public LungDiseaseExpertSystemGUI() {
		setTitle("System Ekspertowy - Choroby Płuc");
		setSize(600, 700);
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		setLayout(new BorderLayout());
		
		Query loadFile = new Query("consult('src/main/prolog/lung_disease.pl')");
        if (!loadFile.hasSolution()) {
            System.err.println("Błąd: Nie udało się załadować pliku lung_disease.pl");
            System.exit(1);
        }

		// Panel objawów
		JPanel objawyPanel = createCheckboxPanel("Objawy", OBJAWY);
		objawyCheckBoxes = getCheckBoxes(objawyPanel);

		// Panel czynników ryzyka
		JPanel czynnikRyzykaPanel = createCheckboxPanel("Czynniki Ryzyka", CZYNNIKI_RYZYKA);
		czynnikRyzykaCheckBoxes = getCheckBoxes(czynnikRyzykaPanel);

		// Przycisk diagnozy
		JButton diagnozujButton = new JButton("Diagnozuj");
		diagnozujButton.addActionListener(e -> przeprowadzDiagnoze());

		// Obszar wyniku
		wynikArea = new JTextArea(10, 40);
		wynikArea.setEditable(false);
		JScrollPane scrollPane = new JScrollPane(wynikArea);

		// Układ paneli
		JPanel mainPanel = new JPanel();
		mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));
		mainPanel.add(objawyPanel);
		mainPanel.add(czynnikRyzykaPanel);
		mainPanel.add(diagnozujButton);
		mainPanel.add(scrollPane);

		add(mainPanel);
		pack();
		setLocationRelativeTo(null);
	}

	private JPanel createCheckboxPanel(String title, List<String> elements) {
		JPanel panel = new JPanel();
		panel.setBorder(BorderFactory.createTitledBorder(title));
		panel.setLayout(new GridLayout(0, 2));

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

	private void przeprowadzDiagnoze() {
		List<String> wybraneObjawy = new ArrayList<>();
		List<String> wybraneCzynniki = new ArrayList<>();

		for (JCheckBox checkbox : objawyCheckBoxes) {
			if (checkbox.isSelected()) {
				wybraneObjawy.add(checkbox.getActionCommand());
			}
		}

		for (JCheckBox checkbox : czynnikRyzykaCheckBoxes) {
			if (checkbox.isSelected()) {
				wybraneCzynniki.add(checkbox.getActionCommand());
			}
		}

		// Logika diagnostyczna z Prologiem
		try {
            // Czyszczenie poprzednich faktów
            new Query("retractall(objaw(_))").hasSolution();
            new Query("retractall(czynnik_ryzyka(_))").hasSolution();

            // Dodawanie nowych faktów
            for (String objaw : wybraneObjawy) {
                new Query("assertz(objaw(" + objaw + "))").hasSolution();
            }
            for (String czynnik : wybraneCzynniki) {
                new Query("assertz(czynnik_ryzyka(" + czynnik + "))").hasSolution();
            }
            System.out.println(wybraneObjawy);
            System.out.println(wybraneCzynniki);

            // Tworzenie list w formacie Prologu
            String objawyList = wybraneObjawy.isEmpty() ? "[]" : "[" + String.join(",", wybraneObjawy) + "]";
            String czynnikiList = wybraneCzynniki.isEmpty() ? "[]" : "[" + String.join(",", wybraneCzynniki) + "]";

            // Zapytanie diagnostyczne
            Query diagnostyka = new Query("diagnozuj(Choroba, " + objawyList + ", " + czynnikiList + ")");

            StringBuilder wyniki = new StringBuilder();
            wyniki.append("Możliwe diagnozy:\n");

            if (diagnostyka.hasSolution()) {
                while (diagnostyka.hasMoreSolutions()) {
                    Map<String, Term> wynik = diagnostyka.nextSolution();
                    wyniki.append("- ").append(wynik.get("Choroba").toString().replace("_", " ")).append("\n");
                }
            } else {
                wyniki.append("Nie można postawić diagnozy na podstawie podanych objawów.\n");
            }

            wynikArea.setText(wyniki.toString());

        } catch (Exception e) {
            wynikArea.setText("Błąd diagnostyczny: " + e.toString());
        }
	}

	public static void main(String[] args) {

		SwingUtilities.invokeLater(() -> {
			new LungDiseaseExpertSystemGUI().setVisible(true);
		});
	}
}