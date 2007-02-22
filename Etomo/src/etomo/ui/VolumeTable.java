package etomo.ui;

import java.awt.Container;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ArrayList;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JFileChooser;
import javax.swing.JPanel;
import javax.swing.border.LineBorder;

import etomo.PeetManager;
import etomo.storage.TomogramFileFilter;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.2  2007/02/21 04:25:18  sueh
 * <p> bug# 964 Changed PeetManager.setName() to PeetManager.setParamFile().
 * <p>
 * <p> Revision 1.1  2007/02/20 20:37:32  sueh
 * <p> bug# 964 A list of tomograms for the PEET interface.
 * <p> </p>
 */
final class VolumeTable implements Expandable {
  public static final String rcsid = "$Id$";

  private final RowList rowList = new RowList();
  private final MultiLineButton btnAddTomogram = new MultiLineButton(
      "Add Tomogram");
  private final JPanel pnlButtons = new JPanel();
  private final HeaderCell header1Tomogram = new HeaderCell("Tomogram");
  private final ExpandButton tomogramExpandButton;
  private final JPanel pnlTable = new JPanel();
  private final GridBagLayout layout = new GridBagLayout();
  private final GridBagConstraints constraints = new GridBagConstraints();
  private final PeetManager manager;
  private JPanel rootPanel = null;

  public void expand(ExpandButton button) {

  }

  VolumeTable(PeetManager manager) {
    this.manager = manager;
    tomogramExpandButton = ExpandButton.getInstance(this,
        ExpandButton.Type.MORE);
  }

  Container getContainer() {
    create();
    return rootPanel;
  }

  int size() {
    return rowList.size();
  }

  private void create() {
    if (rootPanel != null) {
      return;
    }
    //table
    pnlTable.setLayout(layout);
    pnlTable.setBorder(LineBorder.createBlackLineBorder());
    constraints.fill = GridBagConstraints.BOTH;
    //headers
    //First row
    constraints.anchor = GridBagConstraints.CENTER;
    constraints.weightx = 0.1;
    constraints.weighty = 0.0;
    constraints.gridheight = 1;
    constraints.gridwidth = 1;
    header1Tomogram.add(pnlTable, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    constraints.weightx = 0.0;
    tomogramExpandButton.add(pnlTable, layout, constraints);
    //border
    SpacedPanel pnlBorder = new SpacedPanel();
    pnlBorder.setBoxLayout(BoxLayout.Y_AXIS);
    pnlBorder.setBorder(new EtchedBorder("Boundary Table").getBorder());
    pnlBorder.add(pnlTable);
    //buttons
    pnlButtons.setLayout(new BoxLayout(pnlButtons, BoxLayout.X_AXIS));
    btnAddTomogram.setSize();
    pnlButtons.add(btnAddTomogram.getComponent());
    //root
    rootPanel = new JPanel();
    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    rootPanel.setBorder(BorderFactory.createEtchedBorder());
    rootPanel.add(pnlBorder.getContainer());
    rootPanel.add(pnlButtons);
    //actions
    btnAddTomogram.addActionListener(new VTActionListener(this));
  }

  void action(ActionEvent event) {
    if (event.getActionCommand().equals(btnAddTomogram.getActionCommand())) {
      addTomogram();
    }
  }

  private void addTomogram() {
    if (!manager.setParamFile()) {
      UIHarness.INSTANCE.openMessageDialog("Please set the "
          + PeetDialog.DIRECTORY_LABEL + " and " + PeetDialog.OUTPUT_LABEL
          + " fields before adding tomograms.", "Entry Error");
      return;
    }
    JFileChooser chooser = new JFileChooser(new File(manager
        .getPropertyUserDir()));
    chooser.setFileFilter(new TomogramFileFilter());
    chooser.setPreferredSize(UIParameters.INSTANCE.getFileChooserDimension());
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    int returnVal = chooser.showOpenDialog(rootPanel);
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      addRow(chooser.getSelectedFile());
    }
  }

  private void addRow(File tomogram) {
    rowList.add(tomogram, pnlTable, layout, constraints, tomogramExpandButton
        .isExpanded());
    UIHarness.INSTANCE.pack(manager);
  }

  private static final class RowList {
    private final ArrayList list = new ArrayList();

    int size() {
      return list.size();
    }

    synchronized void add(File tomogram, JPanel panel, GridBagLayout layout,
        GridBagConstraints constraints, boolean expanded) {
      VolumeRow row = new VolumeRow(tomogram, list.size(), panel, layout,
          constraints, expanded);
      list.add(row);
      row.display();
    }
  }

  private static final class VTActionListener implements ActionListener {
    VolumeTable volumeTable;

    VTActionListener(VolumeTable volumeTable) {
      this.volumeTable = volumeTable;
    }

    public void actionPerformed(ActionEvent event) {
      volumeTable.action(event);
    }
  }
}
