package etomo.process;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Toolkit;
import javax.swing.*;
import etomo.ui.FixedDim;

/*
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
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
public class ProcessProgressDialog extends JDialog implements Runnable {
  public static final String rcsid =
    "$Id$";

  private JPanel contentPane;
  private JLabel label;
  public JProgressBar progressBar;

  private Thread thread;
  public int stepTimeMS;

  public ProcessProgressDialog(String newLabel, Thread current) {
    label = new JLabel(newLabel + " progress...");
    progressBar = new JProgressBar();

    contentPane = (JPanel) getContentPane();
    contentPane.setLayout(new BorderLayout());
    contentPane.add(label, BorderLayout.NORTH);
    contentPane.add(Box.createRigidArea(FixedDim.x5_y0), BorderLayout.EAST);
    contentPane.add(Box.createRigidArea(FixedDim.x5_y0), BorderLayout.WEST);
    contentPane.add(progressBar, BorderLayout.CENTER);
    contentPane.add(Box.createRigidArea(FixedDim.x0_y5), BorderLayout.SOUTH);
    thread = current;
    pack();
    Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
    Dimension size = getSize();
    setLocation(
      (screenSize.width - size.width) / 2,
      (screenSize.height - size.height) / 2);
  }

  public void run() {

    show();

    try {
      int i = 0;
      while (true) {
        thread.sleep(stepTimeMS);
        progressBar.setValue(i);
        repaint();
        i++;
      }
    }
    catch (InterruptedException except) {
    }
    dispose();
  }
}
