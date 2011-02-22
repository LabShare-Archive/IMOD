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
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 3.13  2009/01/20 20:22:40  sueh
 * <p> bug# 1102 Gave the progress bar a label that is less likely to be
 * <p> duplicated.
 * <p>
 * <p> Revision 3.12  2008/09/30 22:12:27  sueh
 * <p> bug# 1113 Using a private constructor in ProgressPanel.  Added
 * <p> addListeners.
 * <p>
 * <p> Revision 3.11  2006/08/10 17:51:32  sueh
 * <p> bug# 686 Passing manager and axis to ProgressPanel.  Added pack().
 * <p>
 * <p> Revision 3.10  2006/04/25 19:19:34  sueh
 * <p> bug# 787 Named the progress bar.
 * <p>
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

package etomo.ui.swing;

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

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.type.ProcessEndState;
import etomo.util.Utilities;

public final class ProgressPanel {
  public static final String rcsid = "$Id$";

  public static final String NAME = "the-progress-bar";
  public static final String LABEL_NAME = NAME + "-label";
  private static final int MAX_PACK = 5;

  private final JPanel panel = new JPanel();
  private final JPanel progressPanel = new JPanel();
  private final JLabel taskLabel = new JLabel();
  private final JProgressBar progressBar = new JProgressBar();
  private final BaseManager manager;
  private final AxisID axisID;

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
  private int nPacked = 0;

  //required - instantiate once
  private ProgressTimerActionListener progressTimerActionListener;
  private Timer timer;

  private ProgressPanel(String newLabel, BaseManager manager, AxisID axisID) {
    this.manager = manager;
    this.axisID = axisID;
    taskLabel.setText(newLabel);
    panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
    panel.add(taskLabel);
    panel.add(Box.createRigidArea(FixedDim.x0_y5));
    panel.add(progressBar);
    panel.setAlignmentY(Component.BOTTOM_ALIGNMENT);
    progressBar.setName(NAME);
    taskLabel.setName(LABEL_NAME);
  }

  static ProgressPanel getInstance(String newLabel, BaseManager manager, AxisID axisID) {
    ProgressPanel instance = new ProgressPanel(newLabel, manager, axisID);
    instance.addListeners();
    return instance;
  }

  private void addListeners() {
    progressTimerActionListener = new ProgressTimerActionListener(this);
    timer = new Timer(1000, progressTimerActionListener);
  }

  /**
   * Pack the dialog the first few times it is changed, so that that scroll
   * aren't displayed the first time a process runs.
   */
  private void pack() {
    if (nPacked >= MAX_PACK) {
      return;
    }
    nPacked++;
    UIHarness.INSTANCE.pack(axisID, manager);
  }

  void setBackground(final Color bg) {
    panel.setBackground(bg);
  }

  void setLabel(final String newLabel) {
    stopped = false;
    label = newLabel;
    SwingUtilities.invokeLater(new SetLabelLater());
  }

  private final class SetLabelLater implements Runnable {
    public void run() {
      setTaskLabel();
      revalidate();
      repaint();
      pack();
    }
  }

  void start() {
    stopped = false;
    //  Setting the progress bar indeterminate causes it to move on its own
    counter = 0;
    startTime = System.currentTimeMillis();
    SwingUtilities.invokeLater(new StartLater());
  }

  private final class StartLater implements Runnable {
    public void run() {
      JProgressBar progressBar = getProgressBar();
      progressBar.setIndeterminate(true);
      progressBar.setString("");
      progressBar.setStringPainted(true);
      startTimer();
      pack();
    }
  }

  void stop(ProcessEndState state, final String statusString) {
    stopped = true;
    counter = 0;
    if (state == null) {
      state = ProcessEndState.DONE;
    }
    SwingUtilities.invokeLater(new StopLater(state, statusString));
  }

  private final class StopLater implements Runnable {
    private final ProcessEndState state;
    private final String statusString;

    private StopLater(final ProcessEndState state, final String statusString) {
      this.state = state;
      this.statusString = statusString;
    }

    public void run() {
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
      pack();
    }
  }

  private void increment() {
    SwingUtilities.invokeLater(new IncrementLater(stopped));
  }

  private final class IncrementLater implements Runnable {
    private boolean stopped = false;

    private IncrementLater(final boolean stopped) {
      this.stopped = stopped;
    }

    public void run() {
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
      getProgressBar()
          .setString(
              "Elapsed time: "
                  + Utilities.millisToMinAndSecs(System.currentTimeMillis()
                      - getStartTime()));
      validate();
      repaint();
      incrementCounter();
      restartTimer();
      pack();
    }
  }

  /**
   * @param n
   */
  void setMaximum(final int n) {
    stopped = false;
    maximum = n;
    SwingUtilities.invokeLater(new SetMaximumLater());
  }

  private final class SetMaximumLater implements Runnable {
    public void run() {
      setProgressBarMaximum();
      getProgressBar().setIndeterminate(false);
      getProgressBar().setStringPainted(true);
      pack();
    }
  }

  void setMinimum(final int n) {
    stopped = false;
    minimum = n;
    SwingUtilities.invokeLater(new SetMinimumLater());
  }

  private final class SetMinimumLater implements Runnable {
    public void run() {
      setProgressBarMinimum();
    }
  }

  void setValue(final int n) {
    stopped = false;
    value = n;
    SwingUtilities.invokeLater(new SetValueLater());
  }

  private final class SetValueLater implements Runnable {
    public void run() {
      setProgressBarValue();
    }
  }

  /**
   * 
   * @param n
   * @param string
   */
  void setValue(final int n, final String string) {
    stopped = false;
    value = n;
    barString = string;
    SwingUtilities.invokeLater(new SetValueAndStringLater());
  }

  private final class SetValueAndStringLater implements Runnable {
    public void run() {
      setProgressBarValue();
      setProgressBarString();
    }
  }

  Container getContainer() {
    return panel;
  }

  /**
   * @return
   */
  int getMaximum() {
    return progressBar.getMaximum();
  }

  /**
   * @return
   */
  int getMinimum() {
    return progressBar.getMinimum();
  }

  /**
   * @return
   */
  int getValue() {
    return progressBar.getValue();
  }

  private void setTaskLabel() {
    taskLabel.setText(label);
  }

  private void revalidate() {
    panel.revalidate();
  }

  private void repaint() {
    panel.repaint();
  }

  private void validate() {
    panel.validate();
  }

  private JProgressBar getProgressBar() {
    return progressBar;
  }

  private void restartTimer() {
    timer.restart();
  }

  private void startTimer() {
    timer.start();
  }

  private void stopTimer() {
    timer.stop();
  }

  private void setProgressBarCounter() {
    progressBar.setValue(counter);
  }

  private void setProgressBarValue() {
    progressBar.setValue(value);
  }

  private void incrementCounter() {
    counter++;
  }

  private void setProgressBarMaximum() {
    progressBar.setMaximum(maximum);
  }

  private void setProgressBarMinimum() {
    progressBar.setMinimum(minimum);
  }

  private void setProgressBarString() {
    progressBar.setString(barString);
  }

  private long getStartTime() {
    return startTime;
  }

  private static final class ProgressTimerActionListener implements ActionListener {
    private final ProgressPanel panel;

    private ProgressTimerActionListener(final ProgressPanel panel) {
      this.panel = panel;
    }

    public void actionPerformed(final ActionEvent event) {
      panel.increment();
    }
  }
}