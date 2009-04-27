package etomo.ui;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.type.DialogType;

/**
 * <p>Description: Panel for fixing incorrect file paths.</p>
 * 
 * <p>Copyright: Copyright 2008</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$ </p>
 */
final class FixPathsPanel implements Expandable {
  public static final String rcsid = "$Id$";

  private final JPanel pnlRoot = new JPanel();
  private final EtomoPanel pnlMain = new EtomoPanel();
  private final JPanel pnlBody = new JPanel();
  private final JLabel lblwarning = new JLabel("Files cannot be found.  PEET may not run.");
  private final CheckBox cbChoosePathEveryRow = new CheckBox(
      "Choose a path for every row and separate file");
  private final MultiLineButton bnFixPaths= new MultiLineButton("Fix Incorrect Paths");

  private final PanelHeader header;

  private final FileContainer fileContainer;
  private final BaseManager manager;
  private final AxisID axisID;

  private FixPathsPanel(FileContainer fileContainer, BaseManager manager, AxisID axisID,
      DialogType dialogType) {
    this.fileContainer=fileContainer;
    this.manager = manager;
    this.axisID = axisID;
    header = PanelHeader.getInstance("Fix File Paths", this, dialogType);
    //root panel
    pnlRoot.setLayout(new BoxLayout(pnlRoot,BoxLayout.X_AXIS));
    pnlRoot.setVisible(false);
    pnlRoot.add(pnlMain);
    //main panel
    BoxLayout boxLayout = new BoxLayout(pnlMain, BoxLayout.Y_AXIS);
    pnlMain.setLayout(boxLayout);
    pnlMain.setBorder(BorderFactory.createEtchedBorder());
    pnlMain.add(header);
    pnlMain.add(pnlBody);
    //body panel
    pnlBody.setLayout(new BoxLayout(pnlBody, BoxLayout.Y_AXIS));
    lblwarning.setForeground(ProcessControlPanel.colorNotStarted);
    pnlBody.add(lblwarning);
    pnlBody.add(cbChoosePathEveryRow);
    JPanel pnlButton = new JPanel();
    pnlButton.setLayout(new BoxLayout(pnlButton,BoxLayout.X_AXIS));
    pnlButton.add(Box.createHorizontalGlue());
    bnFixPaths.setSize();
    pnlButton.add(bnFixPaths.getComponent());
    pnlButton.add(Box.createHorizontalGlue());
    pnlBody.add(pnlButton);
    //Minimize the size once the panels contains all their elements.
    pnlMain.setMaximumSize(boxLayout.preferredLayoutSize(pnlMain));
  }

  static FixPathsPanel getInstance(FileContainer fileContainer,BaseManager manager, AxisID axisID,
      DialogType dialogType) {
    FixPathsPanel instance = new FixPathsPanel(fileContainer,manager, axisID, dialogType);
    instance.addListeners();
    return instance;
  }

  private void addListeners() {
    bnFixPaths.addActionListener(new FixPathsPanelListener(this));
  }

  Component getRootComponent() {
    return pnlRoot;
  }
  
  void setIncorrectPaths(boolean incorrectPaths) {
    if (incorrectPaths) {
      pnlRoot.setVisible(true);
      lblwarning.setVisible(true);
    }
    else {
      lblwarning.setVisible(false);
    }
  }
  
  private void action() {
    fileContainer.fixIncorrectPaths(cbChoosePathEveryRow.isSelected());
  }

  public void expand(ExpandButton button) {
    if (header.equalsOpenClose(button)) {
      pnlBody.setVisible(button.isExpanded());
    }
    UIHarness.INSTANCE.pack(axisID, manager);
  }
  
  private final class FixPathsPanelListener implements ActionListener {
    private final FixPathsPanel adaptee;

    private FixPathsPanelListener(final FixPathsPanel fixPathsPanel) {
      adaptee = fixPathsPanel;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.action();
    }
  }

}
