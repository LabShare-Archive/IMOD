package etomo.ui;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;

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
 * <p> $Log$ </p>
 */
public class GenericProcessingDialog extends ProcessDialog {
  public static final String rcsid = "$Id$";

  JPanel contentPane;

  public GenericProcessingDialog() {
    contentPane = (JPanel) this.getContentPane();
    contentPane.setLayout(new BoxLayout(contentPane, BoxLayout.Y_AXIS));
    this.setTitle("eTomo Generic processing");
    //this.setSize(defaultWindowSize);


    contentPane.add(Box.createVerticalGlue());
    contentPane.add(Box.createRigidArea(FixedDim.x0_y10));
    contentPane.add(panelExitButtons);
    contentPane.add(Box.createRigidArea(FixedDim.x0_y10));
  }

  //
  //  Action function overides for buttons
  //
  public void buttonPostponeAction(ActionEvent event) {
    System.out.print("Generic processing");
    super.buttonPostponeAction(event);
  }
  public void buttonExecuteAction(ActionEvent event) {
    System.out.print("Generic processing");
    super.buttonExecuteAction(event);

  }
}
