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
 * <p> Revision 3.9  2005/11/14 22:18:11  sueh
 * <p> bug# 762 The internal class is now accessing protected functions instead
 * <p> of private variables.
 * <p>
 * <p> Revision 3.8  2005/08/22 18:19:26  sueh
 * <p> bug# 532  Added an optional status string to be appended to the end
 * <p> state.
 * <p>
 * <p> Revision 3.7  2005/07/26 23:08:12  sueh
 * <p> bug# 701 When stopping the progress bar, pass the process end state.
 * <p>
 * <p> Revision 3.6  2005/04/16 02:03:56  sueh
 * <p> bug# 615 Added setBackground(Color).
 * <p>
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

import etomo.type.ProcessEndState;
import etomo.util.Utilities;

public class ProgressPanel {
  public static final String rcsid = "$Id$";

  public static final String NAME = "progress-bar";
  
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
    progressBar.setName(NAME);
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
      setTaskLabel();
      revalidate();
      repaint();
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
      JProgressBar progressBar = getProgressBar();
      progressBar.setIndeterminate(true);
      progressBar.setString("");
      progressBar.setStringPainted(true);
      startTimer();
    }
  }

  public void stop(ProcessEndState state, String statusString) {
    stopped = true;
    counter = 0;
    if (state == null) {
      state = ProcessEndState.DONE;
    }
    SwingUtilities.invokeLater(new StopLater(state, statusString));
  }

  private class StopLater implements Runnable {
    private final ProcessEndState state;
    private final String statusString;
    
    public StopLater(ProcessEndState state, String statusString) {
      this.state = state;
      this.statusString = statusString;
    }
    
    public void run() {
      //System.out.println("StopLater.run()");
      stopTimer();
      JProgressBar progressBar = getProgressBar();
      setProgressBarCounter();
      progressBar.setIndeterminate(false);
      StringBuffer message = new StringBuffer(state.toString());
      if (statusString != null) {
        message.append(":  " + statusString);
      }
      progressBar.setString(message.toString());
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
      setProgressBarValue();
       //  Put the elapsed time into the progress bar string
      getProgressBar().setString("Elapsed time: "
          + Utilities
            .millisToMinAndSecs(System.currentTimeMillis() - getStartTime()));
      validate();
      repaint();
      incrementCounter();
      restartTimer();
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
      setProgressBarMaximum();
      getProgressBar().setIndeterminate(false);
      getProgressBar().setStringPainted(true);
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
      setProgressBarMinimum();
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
      setProgressBarValue();
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
      setProgressBarValue();
      setProgressBarString();
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
  
  protected final void setTaskLabel() {
    taskLabel.setText(label);
  }
  
  protected final void revalidate() {
    panel.revalidate();
  }
  
  protected final void repaint() {
    panel.repaint();
  }
  
  protected final void validate() {
    panel.validate();
  }
  
  protected final JProgressBar getProgressBar() {
    return progressBar;
  }
  
  protected final void restartTimer() {
    timer.restart();
  }
  
  protected final void startTimer() {
    timer.start();
  }
  
  protected final void stopTimer() {
    timer.stop();
  }
  
  protected final void setProgressBarCounter() {
    progressBar.setValue(counter);
  }
  
  protected final void setProgressBarValue() {
    progressBar.setValue(value);
  }
  
  protected final void incrementCounter() {
    counter++;
  }
  
  protected final void setProgressBarMaximum() {
    progressBar.setMaximum(maximum);
  }
  
  protected final void setProgressBarMinimum() {
    progressBar.setMinimum(minimum);
  }
  
  protected final void setProgressBarString() {
    progressBar.setString(barString);
  }
  
  protected final long getStartTime() {
    return startTime;
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