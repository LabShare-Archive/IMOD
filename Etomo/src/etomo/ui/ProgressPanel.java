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
 * <p> Revision 3.5  2004/08/19 02:55:12  sueh
 * <p> bug# 508 Fixed a bug where a stop would be overridden by an event
 * <p> that caused IncrementLater.run() to run.  This happened when the
 * <p> event happened to appear after the stop executed.  Create a stopped
 * <p> member variable.  Set stopped to false everywhere, except in
 * <p> StopLater.run(), where it is set to true.  In IncrementLater.run(), if
 * <p> stopped is true, then return without executing.
 * <p>
 * <p> Revision 3.4  2004/04/26 00:20:51  rickg
 * <p> Changed elapsed to elapsed time
 * <p>
 * <p> Revision 3.3  2004/04/09 19:21:53  rickg
 * <p> Removed debugging output
 * <p>
 * <p> Revision 3.2  2004/04/08 19:11:26  rickg
 * <p> Bug #421 Make sure the UI objects are only modified in the event
 * <p> thread.
 * <p> Bug #83 Added elapsed time string to indeterminate progress
 * <p> bars
 * <p>
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

import java.awt.Color;
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
  //stopped: IMPORTANT: The stop action should turn this boolean on, all other
  //actions, except increment should turn this off.
  private boolean stopped = true;

  public ProgressPanel(String newLabel) {
    taskLabel.setText(newLabel);
    panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
    panel.add(taskLabel);
    panel.add(Box.createRigidArea(FixedDim.x0_y5));
    panel.add(progressBar);
    panel.setAlignmentY(Component.BOTTOM_ALIGNMENT);
    timer = new Timer(1000, new ProgressTimerActionListener(this));
  }
  
  public void setBackground(Color bg) {
    panel.setBackground(bg);
  }

  public void setLabel(String newLabel) {
    stopped = false;
    label = newLabel;
    SwingUtilities.invokeLater(new SetLabelLater());
  }

  private class SetLabelLater implements Runnable {
    public void run() {
      //System.out.println("SetLabelLater.run()");
      taskLabel.setText(label);
      panel.revalidate();
      panel.repaint();
    }
  }

  public void start() {
    stopped = false;
    //  Setting the progress bar indeterminate causes it to move on its own
    counter = 0;
    startTime = System.currentTimeMillis();
    SwingUtilities.invokeLater(new StartLater());
  }

  private class StartLater implements Runnable {
    public void run() {
      //System.out.println("StartLater.run()");
      progressBar.setIndeterminate(true);
      progressBar.setString("");
      progressBar.setStringPainted(true);
      timer.start();
    }
  }

  public void stop() {
    stopped = true;
    counter = 0;
    SwingUtilities.invokeLater(new StopLater());
  }

  private class StopLater implements Runnable {
    public void run() {
      //System.out.println("StopLater.run()");
      timer.stop();
      progressBar.setValue(counter);
      progressBar.setIndeterminate(false);
      progressBar.setString("done");
      progressBar.setStringPainted(true);
    }
  }

  void increment() {
    SwingUtilities.invokeLater(new IncrementLater(stopped));
  }

  private class IncrementLater implements Runnable {
    private boolean stopped = false;
    public IncrementLater(boolean stopped) {
      this.stopped = stopped;
    }
    public void run() {
      //System.out.println("IncrementLater.run()");
      //Fixing a bug during kill process where the timer doesn't stop:  the 
      //progress bar goes to determinate mode and increments based on the timer.
      //
      //If the progress bar is stopped this call should never happen.
      //If the timer did not stop before it generated the event that caused
      //increment to be called, then the timer will never stop.
      //
      //Tell the timer to stop each time this function is called incorrectly.
      if (stopped) {
        return;
      }
      progressBar.setValue(counter);
       //  Put the elapsed time into the progress bar string
      progressBar.setString("Elapsed time: "
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
    stopped = false;
    maximum = n;
    SwingUtilities.invokeLater(new SetMaximumLater());
  }

  private class SetMaximumLater implements Runnable {
    public void run() {
      //System.out.println("SetMaximumLater.run()");
      progressBar.setMaximum(maximum);
      progressBar.setIndeterminate(false);
      progressBar.setStringPainted(true);
    }
  }

  public void setMinimum(int n) {
    stopped = false;
    minimum = n;
    SwingUtilities.invokeLater(new SetMinimumLater());
  }

  private class SetMinimumLater implements Runnable {
    public void run() {
      //System.out.println("SetMinimumLater.run()");
      progressBar.setMinimum(minimum);
    }
  }

  public void setValue(int n) {
    stopped = false;
    value = n;
    SwingUtilities.invokeLater(new SetValueLater());
  }

  private class SetValueLater implements Runnable {
    public void run() {
      //System.out.println("SetValueLater.run()");
      progressBar.setValue(value);
    }
  }

  /**
   * 
   * @param n
   * @param string
   */
  public void setValue(int n, String string) {
    stopped = false;
    value = n;
    barString = string;
    SwingUtilities.invokeLater(new SetValueAndStringLater());
  }

  private class SetValueAndStringLater implements Runnable {
    public void run() {
      //System.out.println("SetValueAndStringLater.run()");
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
      //System.out.println("actionPerformed:event=" + event);
      panel.increment();
    }
  }

}