package main.java;

import javax.swing.SwingUtilities;
import javax.swing.UIManager;

public class LungDiseaseExpertSystemMain {
    public static void main(String[] args) {
        try {
            UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
        } catch (Exception e) {
            e.printStackTrace();
        }

        SwingUtilities.invokeLater(() -> {
            LungDiseaseExpertSystemGUI gui = new LungDiseaseExpertSystemGUI();
            gui.setVisible(true);
        });
    }
}