/**
 * <p>Description: A progress bar with label and internal text, can be
 *  determinate or indeterminate.</p>
 *
 * <p>Copyright: Copyright (c) 2002</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 * <p> $Log$
 * <p> Revision 3.1  2004/04/08 15:52:34  rickg
 * <p> Added invokeLater structure to progress bar
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.3  2003/07/01 22:57:12  rickg
 * <p> Cleaned progress panel layout
 * <p>
 * <p> Revision 2.2  2003/05/27 08:53:38  rickg
 * <p> Determinant progress bar now takes a string
 * <p>
 * <p> Revision 2.1  2003/05/23 14:21:27  rickg
 * <p> Added determinant progress methods
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.1.2.1  2003/01/24 18:43:37  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 */

package etomo.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.SwingUtilities;
import javax.swing.Timer;

import etomo.util.Utilities;

public class ProgressPanel {
  public static final String rcsid = "$Id$";

  private JPanel panel = new JPanel();
  private JPanel progressPanel = new JPanel();
  private JLabel taskLabel = new JLabel();
  private JProgressBar progressBar = new JProgressBar();
  private Timer timer;

  // Keep these around so that SwingUtilities.invokeLater can update the
  // the UI status 
  private int counter = 0;
  private int value;
  private int maximum;
  private int minimum;
  private long startTime;
  private String barString;
  private String label;

  public ProgressPanel(String newLabel) {
    taskLabel.setText(newLabel);
    panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
    panel.add(taskLabel);
    panel.add(Box.createRigidArea(FixedDim.x0_y5));
    panel.add(progressBar);
    panel.setAlignmentY(Component.BOTTOM_ALIGNMENT);
    timer = new Timer(1000, new ProgressTimerActionListener(this));
  }

  public void setLabel(String newLabel) {
    label = newLabel;
    SwingUtilities.invokeLater(new SetLabelLater());
  }

  private class SetLabelLater implements Runnable {
    public void run() {
      taskLabel.setText(label);
      panel.revalidate();
      panel.repaint();
    }
  }

  public void start() {
    //  Setting the progress bar indeterminate causes it to move on its own
    counter = 0;
    startTime = System.currentTimeMillis();
    SwingUtilities.invokeLater(new StartLater());
  }

  private class StartLater implements Runnable {
    public void run() {
      progressBar.setIndeterminate(true);
      progressBar.setString("");
      progressBar.setStringPainted(true);
      timer.start();
    }
  }

  public void stop() {
    counter = 0;
    SwingUtilities.invokeLater(new StopLater());
  }

  private class StopLater implements Runnable {
    public void run() {
      timer.stop();
      progressBar.setValue(counter);
      progressBar.setIndeterminate(false);
      progressBar.setString("done");
      progressBar.setStringPainted(true);
    }
  }

  void increment() {
    SwingUtilities.invokeLater(new IncrementLater());
  }

  private class IncrementLater implements Runnable {
    public void run() {
      progressBar.setValue(counter);
       //  Put the elapsed time into the progress bar string
      progressBar.setString("Elapsed: "
          + Utilities
            .millisToMinAndSecs(System.currentTimeMillis() - startTime));
      panel.validate();
      panel.repaint();
      counter++;
      timer.restart();

    }
  }

  /**
   * @param n
   */
  public void setMaximum(int n) {
    maximum = n;
    SwingUtilities.invokeLater(new SetMaximumLater());
  }

  private class SetMaximumLater implements Runnable {
    public void run() {
      progressBar.setMaximum(maximum);
      progressBar.setIndeterminate(false);
      progressBar.setStringPainted(true);
    }
  }

  public void setMinimum(int n) {
    minimum = n;
    SwingUtilities.invokeLater(new SetMinimumLater());
  }

  private class SetMinimumLater implements Runnable {
    public void run() {
      progressBar.setMinimum(minimum);
    }
  }

  public void setValue(int n) {
    value = n;
    SwingUtilities.invokeLater(new SetValueLater());
  }

  private class SetValueLater implements Runnable {
    public void run() {
      progressBar.setValue(value);
    }
  }

  /**
   * 
   * @param n
   * @param string
   */
  public void setValue(int n, String string) {
    value = n;
    barString = string;
    SwingUtilities.invokeLater(new SetValueAndStringLater());
  }

  private class SetValueAndStringLater implements Runnable {
    public void run() {
      progressBar.setValue(value);
      progressBar.setString(barString);
    }
  }

  public Container getContainer() {
    return panel;
  }

  /**
   * @return
   */
  public int getMaximum() {
    return progressBar.getMaximum();
  }

  /**
   * @return
   */
  public int getMinimum() {
    return progressBar.getMinimum();
  }

  /**
   * @return
   */
  public int getValue() {
    return progressBar.getValue();
  }

  class ProgressTimerActionListener implements ActionListener {
    ProgressPanel panel;

    public ProgressTimerActionListener(ProgressPanel panel) {
      this.panel = panel;
    }

    public void actionPerformed(ActionEvent event) {
      System.err.println("timer event");
      panel.increment();
    }
  }

}