package etomo.ui;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

import etomo.process.ProcessState;

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
 * <p> Revision 2.2  2003/10/23 17:16:55  rickg
 * <p> Bug# Label consistency
 * <p>
 * <p> Revision 2.1  2003/06/09 04:25:04  rickg
 * <p> Changed the execute button to a toggle button and added
 * <p> a setSelected method for the button
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.2.2.1  2003/01/24 18:43:37  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.2  2002/10/07 22:31:18  rickg
 * <p> removed unused imports
 * <p> reformat after emacs trashed it
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
public class ProcessControlPanel {
  public static final String rcsid =
    "$Id$";

  static Dimension dimPanelProcess = new Dimension(80, 130);
  static String[] textStates = { "Not Started", "In Progress", "Complete" };
  static Color colorNotStarted = new Color(0.75f, 0.0f, 0.0f);
  static Color colorInProgress = new Color(0.75f, 0.0f, 0.75f);
  static Color colorComplete = new Color(0.0f, 0.75f, 0.0f);
  static Color[] colorState =
    { colorNotStarted, colorInProgress, colorComplete };

  private String name;
  private JPanel panelRoot = new JPanel();
  private JToggleButton buttonRun = new JToggleButton();

  private JPanel panelState;
  private ColoredStateText highlightState =
    new ColoredStateText(textStates, colorState);

  ProcessControlPanel(String label) {
    name = label;
    panelRoot.setLayout(new BoxLayout(panelRoot, BoxLayout.Y_AXIS));
    highlightState.setSelected(0);
    updateLabel();
    panelRoot.add(buttonRun);
    buttonRun.setActionCommand(label);
  }

  String getName() {
    return name;
  }

  void setButtonActionListener(ActionListener actionListener) {
    buttonRun.addActionListener(actionListener);
  }

  JPanel getContainer() {
    return panelRoot;
  }

  void setState(ProcessState state) {
    if (state == ProcessState.NOTSTARTED) {
      highlightState.setSelected(0);
    }
    if (state == ProcessState.INPROGRESS) {
      highlightState.setSelected(1);
    }
    if (state == ProcessState.COMPLETE) {
      highlightState.setSelected(2);
    }
    updateLabel();
  }

  /**
   * Set the selected state of the buuton
   * @param state
   */
  void setSelected(boolean state){
    buttonRun.setSelected(state);
  }
  
  private void updateLabel() {
    buttonRun.setText(
      "<HTML><CENTER>"
        + name
        + "<br>"
        + highlightState.getSelectedText()
        + "</CENTER>");
    buttonRun.setForeground(highlightState.getSelectedColor());
  }

  void addMouseListener(MouseListener listener) {
    panelRoot.addMouseListener(listener);
    buttonRun.addMouseListener(listener);
  }

  void setToolTipText(String text) {
    panelRoot.setToolTipText(text);
    buttonRun.setToolTipText(text);
  }
}
