import javax.sound.sampled.*;
import javax.swing.*;
import java.awt.*;
import java.awt.font.TextAttribute;
import java.util.HashMap;
import java.util.Map;

public class FlickerProgramBeta {
    public static void main(String[] args) {
        JFrame frame = new JFrame("40Hz Flicker");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setExtendedState(JFrame.MAXIMIZED_BOTH);
        frame.setUndecorated(true);
        frame.setVisible(true);

        // Text label setup
        JLabel label = new JLabel("NO");
        label.setFont(getStrikethroughFont(label.getFont(), 100));  // Increase font size
        label.setForeground(Color.WHITE);
        label.setHorizontalAlignment(JLabel.CENTER);
        label.setVerticalAlignment(JLabel.CENTER);
        label.setPreferredSize(new Dimension(frame.getWidth() * 3 / 4, frame.getHeight() * 3 / 4));  // Set label size
        frame.add(label);

        boolean isRed = true;

        // Audio thread for 40Hz click sound
        Thread audioThread = new Thread(() -> {
            try {
                AudioFormat format = new AudioFormat(44100, 16, 1, true, false);
                DataLine.Info info = new DataLine.Info(SourceDataLine.class, format);
                SourceDataLine line = (SourceDataLine) AudioSystem.getLine(info);
                line.open(format, 256); // Small buffer size
                line.start();

                int sampleRate = (int) format.getSampleRate();
                int durationMs = 10; // Duration of the click sound in milliseconds
                int numSamples = (sampleRate * durationMs) / 1000;
                byte[] buffer = new byte[numSamples * 2]; // 16-bit audio

                for (int i = 0; i < numSamples; i++) {
                    short amplitude = (short) (Math.sin(2 * Math.PI * i / (sampleRate / 1000)) * 32767);
                    buffer[2 * i] = (byte) (amplitude & 0xff);
                    buffer[2 * i + 1] = (byte) ((amplitude >> 8) & 0xff);
                }

                while (true) {
                    line.write(buffer, 0, buffer.length);
                    try {
                        Thread.sleep(25); // Wait for 25 ms for 40 Hz
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                }
            } catch (LineUnavailableException e) {
                e.printStackTrace();
            }
        });

        audioThread.setPriority(Thread.MAX_PRIORITY);
        audioThread.start();

        // Main thread for flickering light
        while (true) {
            if (isRed) {
                frame.getContentPane().setBackground(Color.RED);
                label.setForeground(Color.BLACK);  // Set label color to black
            } else {
                frame.getContentPane().setBackground(Color.BLACK);
                label.setForeground(Color.RED);  // Set label color to red
            }
            isRed = !isRed;

            try {
                Thread.sleep(25);  // Adjust the sleep duration for 40Hz flicker
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }

    // Helper method to create a strikethrough font
    private static Font getStrikethroughFont(Font baseFont, int fontSize) {
        Map<TextAttribute, Object> attributes = new HashMap<>();
        attributes.put(TextAttribute.STRIKETHROUGH, TextAttribute.STRIKETHROUGH_ON);
        return baseFont.deriveFont(attributes).deriveFont((float) fontSize);
    }
}
