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
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
public class ProcessControlPanel {
  public static final String rcsid =
    "$Id$";

  static Dimension dimPanelProcess = new Dimension(80, 130);
  static String[] states = { "Not started", "In progress", "Complete" };

  private JPanel panelBase = new JPanel();
  private JTextArea textArea = new JTextArea();
  private JButton buttonRun = new JButton("Open..");

  private JPanel panelState;
  private HighlightList highlightState = new HighlightList(states);

  ProcessControlPanel(String label) {

    panelBase.setLayout(new BoxLayout(panelBase, BoxLayout.Y_AXIS));
    //panelBase.setPreferredSize(dimPanelProcess);
    //panelBase.setMaximumSize(dimPanelProcess);

    textArea.setRows(2);
    textArea.setText(label);
    textArea.setBackground(panelBase.getBackground());
    textArea.setEditable(false);
    panelBase.add(textArea);

    panelState = highlightState.getPanel();
    panelState.setAlignmentX((float) 0.5);
    highlightState.setSelected(0);
    panelBase.add(panelState);

    buttonRun.setAlignmentX((float) 0.5);
    panelBase.add(buttonRun);
  }

  void setButtonActionListener(ActionListener actionListener) {
    buttonRun.addActionListener(actionListener);
  }

  JPanel getPanel() {
    return panelBase;
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
  }

  void addMouseListener(MouseListener listener) {
    panelBase.addMouseListener(listener);
    textArea.addMouseListener(listener);
    buttonRun.addMouseListener(listener);
  }

  void setToolTipText(String text) {
    panelBase.setToolTipText(text);
    textArea.setToolTipText(text);
    buttonRun.setToolTipText(text);
  }
}
