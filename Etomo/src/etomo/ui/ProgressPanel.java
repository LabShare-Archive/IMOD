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
public class ProgressPanel {
  public static final String rcsid =
    "$Id$";

  private JPanel panel = new JPanel();
  private JPanel progressPanel = new JPanel();
  private JLabel taskLabel = new JLabel();
  private JProgressBar progressBar = new JProgressBar();
  private int i = 0;
  private Timer timer;

  public ProgressPanel(String label) {
    taskLabel.setText(label);
    panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
    panel.add(taskLabel);
    panel.add(Box.createRigidArea(FixedDim.x0_y5));
    panel.add(progressBar);
    panel.setAlignmentY(Component.BOTTOM_ALIGNMENT);
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

  /**
   * @param n
   */
  public void setMaximum(int n) {
    progressBar.setMaximum(n);
    progressBar.setIndeterminate(false);
    progressBar.setStringPainted(true);
  }

  /**
   * @param n
   */
  public void setMinimum(int n) {
    progressBar.setMinimum(n);
  }

  /**
   * @param n
   */
  public void setValue(int n) {
    progressBar.setValue(n);
  }

  /**
   * 
   * @param n
   * @param string
   */
  public void setValue(int n, String string) {
    progressBar.setValue(n);
    progressBar.setString(string);
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
