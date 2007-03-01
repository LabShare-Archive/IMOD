package etomo.ui;

import java.awt.Container;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JFileChooser;
import javax.swing.JPanel;
import javax.swing.border.LineBorder;

import etomo.PeetManager;
import etomo.storage.ModelFileFilter;
import etomo.storage.MotlFileFilter;
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
 * <p> Revision 1.3  2007/02/22 20:40:00  sueh
 * <p> bug# 964 Added addRow().
 * <p>
 * <p> Revision 1.2  2007/02/21 04:25:18  sueh
 * <p> bug# 964 Changed PeetManager.setName() to PeetManager.setParamFile().
 * <p>
 * <p> Revision 1.1  2007/02/20 20:37:32  sueh
 * <p> bug# 964 A list of tomograms for the PEET interface.
 * <p> </p>
 */
final class VolumeTable implements Expandable, Highlightable {
  public static final String rcsid = "$Id$";

  private final RowList rowList = new RowList();
  private final JPanel rootPanel = new JPanel();
  private final JPanel pnlButtons = new JPanel();
  private final MultiLineButton btnAddTomogram = new MultiLineButton(
      "Add Tomogram and Model");
  private final MultiLineButton btnSetMotl = new MultiLineButton(
      "Set MOTL file");
  private final HeaderCell header1Highlighter = new HeaderCell();
  private final HeaderCell header1Tomogram = new HeaderCell("Tomogram");
  private final HeaderCell header1Model = new HeaderCell("Model");
  private final HeaderCell header1Motl = new HeaderCell("MOTL");
  private final JPanel pnlTable = new JPanel();
  private final GridBagLayout layout = new GridBagLayout();
  private final GridBagConstraints constraints = new GridBagConstraints();
  private final ExpandButton btnExpandTomogram;
  private final ExpandButton btnExpandModel;
  private final ExpandButton btnExpandMotl;

  private final PeetManager manager;

  public void expand(final ExpandButton button) {
    if (button == btnExpandTomogram) {
      rowList.expandTomogram(btnExpandTomogram.isExpanded());
    }
    else if (button == btnExpandModel) {
      rowList.expandModel(btnExpandModel.isExpanded());
    }
    else if (button == btnExpandMotl) {
      rowList.expandMotl(btnExpandMotl.isExpanded());
    }
    UIHarness.INSTANCE.pack(manager);
  }

  public void highlight(final boolean highlight) {
  }

  static VolumeTable getInstance(final PeetManager manager) {
    return new VolumeTable(manager);
  }

  Container getContainer() {
    return rootPanel;
  }

  void action(final ActionEvent event) {
    String actionCommand = event.getActionCommand();
    if (actionCommand.equals(btnAddTomogram.getActionCommand())) {
      addTomogram();
    }
    if (actionCommand.equals(btnSetMotl.getActionCommand())) {
      setMotl();
    }
  }

  private VolumeTable(final PeetManager manager) {
    this.manager = manager;
    //construction
    btnExpandTomogram = ExpandButton.getInstance(this, ExpandButton.Type.MORE);
    btnExpandModel = ExpandButton.getInstance(this, ExpandButton.Type.MORE);
    btnExpandMotl = ExpandButton.getInstance(this, ExpandButton.Type.MORE);
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
    header1Highlighter.add(pnlTable, layout, constraints);
    header1Tomogram.add(pnlTable, layout, constraints);
    constraints.weightx = 0.0;
    btnExpandTomogram.add(pnlTable, layout, constraints);
    constraints.weightx = 0.1;
    header1Model.add(pnlTable, layout, constraints);
    constraints.weightx = 0.0;
    btnExpandModel.add(pnlTable, layout, constraints);
    constraints.weightx = 0.1;
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    header1Motl.add(pnlTable, layout, constraints);
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
    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    rootPanel.setBorder(BorderFactory.createEtchedBorder());
    rootPanel.add(pnlBorder.getContainer());
    rootPanel.add(pnlButtons);
    //actions
    VTActionListener actionListener = new VTActionListener(this);
    btnAddTomogram.addActionListener(actionListener);
    btnSetMotl.addActionListener(actionListener);
    //update display
    updateDisplay();
  }

  /**
   * Allow the user to choose a tomogram and a model.  Only works if they choose
   * both.
   */
  private void addTomogram() {
    if (!manager.setParamFile()) {
      UIHarness.INSTANCE.openMessageDialog("Please set the "
          + PeetDialog.DIRECTORY_LABEL + " and " + PeetDialog.OUTPUT_LABEL
          + " fields before adding tomograms.", "Entry Error");
      return;
    }
    File tomogram = null;
    File model = null;
    JFileChooser chooser = new JFileChooser(new File(manager
        .getPropertyUserDir()));
    chooser.setFileFilter(new TomogramFileFilter());
    chooser.setPreferredSize(UIParameters.INSTANCE.getFileChooserDimension());
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    int returnVal = chooser.showOpenDialog(rootPanel);
    if (returnVal != JFileChooser.APPROVE_OPTION) {
      return;
    }
    tomogram = chooser.getSelectedFile();
    chooser.setFileFilter(new ModelFileFilter());
    returnVal = chooser.showOpenDialog(rootPanel);
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      model = chooser.getSelectedFile();
    }
    if (tomogram == null || model == null) {
      UIHarness.INSTANCE.openMessageDialog(
          "Please choose both a tomogram and a model", "Entry Error");
    }
    else {
      addRow(tomogram, model);
    }
  }

  private void setMotl() {
    JFileChooser chooser = new JFileChooser(new File(manager
        .getPropertyUserDir()));
    chooser.setFileFilter(new MotlFileFilter());
    chooser.setPreferredSize(UIParameters.INSTANCE.getFileChooserDimension());
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    int returnVal = chooser.showOpenDialog(rootPanel);
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      VolumeRow row = rowList.setMotl(chooser.getSelectedFile());
      if (row != null) {
        row.expandMotl(btnExpandMotl.isExpanded());
        UIHarness.INSTANCE.pack(manager);
      }
    }
  }

  private void addRow(final File tomogram, final File model) {
    addRow(tomogram.getName(), tomogram.getAbsolutePath(), model.getName(),
        model.getAbsolutePath());
  }

  private void addRow(final String contractedTomogram,
      final String expandedTomogram, final String contractedModel,
      final String expandedModel) {
    VolumeRow row = rowList.add(contractedTomogram, expandedTomogram,
        contractedModel, expandedModel, this, pnlTable, layout, constraints);
    row.display();
    row.expandTomogram(btnExpandTomogram.isExpanded());
    row.expandModel(btnExpandModel.isExpanded());
    updateDisplay();
    UIHarness.INSTANCE.pack(manager);
  }

  private void updateDisplay() {
    boolean enable = rowList.size() > 0;
    btnExpandTomogram.setEnabled(enable);
    btnExpandModel.setEnabled(enable);
    btnSetMotl.setEnabled(enable && rowList.isHighlighted());
  }

  /**
   * Uses lazy construction.
   */
  private static final class RowList {
    private final List list = new ArrayList();

    int size() {
      return list.size();
    }

    synchronized VolumeRow add(final String contractedTomogram,
        final String expandedTomogram, final String contractedModel,
        final String expandedModel, final VolumeTable table,
        final JPanel panel, final GridBagLayout layout,
        final GridBagConstraints constraints) {
      VolumeRow row = VolumeRow.getInstance(contractedTomogram,
          expandedTomogram, contractedModel, expandedModel, list.size(), table,
          panel, layout, constraints);
      list.add(row);
      return row;
    }

    void expandTomogram(final boolean expanded) {
      for (int i = 0; i < list.size(); i++) {
        ((VolumeRow) list.get(i)).expandTomogram(expanded);
      }
    }

    void expandModel(final boolean expanded) {
      for (int i = 0; i < list.size(); i++) {
        ((VolumeRow) list.get(i)).expandModel(expanded);
      }
    }

    void expandMotl(final boolean expanded) {
      for (int i = 0; i < list.size(); i++) {
        ((VolumeRow) list.get(i)).expandMotl(expanded);
      }
    }

    boolean isHighlighted() {
      for (int i = 0; i < list.size(); i++) {
        if (((VolumeRow) list.get(i)).isHighlighted()) {
          return true;
        }
      }
      return false;
    }

    /**
     * Set
     * @param motl
     * @return
     */
    VolumeRow setMotl(final File motl) {
      VolumeRow row = getHighlightedRow();
      boolean changeDisplay = row.usingMotlSpinner();
      int index = -1;
      if (row != null) {
        index = row.setMotl(motl);
        //if display changed from spinner to text field, redisplay
        if (changeDisplay) {
          row.removeMotl();
        }
      }
      //if display changed from spinner to text field, redisplay
      if (changeDisplay) {
        //removed the following rows
        for (int i = index + 1; i < list.size(); i++) {
          ((VolumeRow) list.get(i)).remove();
        }
        //redisplay highlighted row
        row.displayMotl();
        //redisplay following rows
        for (int i = index + 1; i < list.size(); i++) {
          ((VolumeRow) list.get(i)).display();
        }
      }
      return row;
    }

    private VolumeRow getHighlightedRow() {
      for (int i = 0; i < list.size(); i++) {
        VolumeRow row = (VolumeRow) list.get(i);
        if (row.isHighlighted()) {
          return row;
        }
      }
      return null;
    }
  }

  private static final class VTActionListener implements ActionListener {
    VolumeTable volumeTable;

    VTActionListener(final VolumeTable volumeTable) {
      this.volumeTable = volumeTable;
    }

    public void actionPerformed(final ActionEvent event) {
      volumeTable.action(event);
    }
  }
}
