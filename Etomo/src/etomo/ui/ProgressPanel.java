package etomo.ui;

import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.Timer;

/**
 * <p>Description: </p>
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
 * <p> Revision 1.1.2.1  2003/01/24 18:43:37  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 */
public class ProgressPanel {
  public static final String rcsid = "$Id$";

  private JPanel panel = new JPanel();
  private JLabel taskLabel = new JLabel();
  private JProgressBar progressBar = new JProgressBar();
  private int i = 0;
  private Timer timer;

  public ProgressPanel(String label) {
    taskLabel.setText(label);
    panel.add(taskLabel);
    panel.add(progressBar);
  }

  public void setLabel(String label) {
    taskLabel.setText(label);
    panel.revalidate();
    panel.repaint();
  }

  public void start() {
    //  Setting the progress bar indeterminate causes it to move on its own
    progressBar.setIndeterminate(true);
    progressBar.setString("");
    progressBar.setStringPainted(false);
    i = 0;
  }

  public void stop() {
    i = 0;
    progressBar.setValue(i);
    progressBar.setIndeterminate(false);
    progressBar.setString("done");
    progressBar.setStringPainted(true);
  }

  void increment() {
    progressBar.setValue(i);
    panel.validate();
    panel.repaint();
    i++;
    timer.restart();
  }

  public Container getContainer() {
    return panel;
  }
}

class ProgressTimerActionListener implements ActionListener {
  ProgressPanel panel;
  public ProgressTimerActionListener(ProgressPanel panel) {
    this.panel = panel;
  }
  public void actionPerformed(ActionEvent event) {
    panel.increment();
  }
}
