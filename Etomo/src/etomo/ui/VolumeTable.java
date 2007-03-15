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
import etomo.storage.MatlabParamFile;
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
 * <p> Revision 1.4  2007/03/01 01:46:48  sueh
 * <p> bug# 964 Added highlighting, model and motl to table.
 * <p>
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
  private final MultiLineButton btnAddFnVolume = new MultiLineButton(
      "Add Tomogram and Model");
  private final MultiLineButton btnSetInitMotl = new MultiLineButton(
      "Set initial motive list file");
  private final HeaderCell header1Highlighter = new HeaderCell();
  private final HeaderCell header1FnVolume = new HeaderCell("Tomogram");
  private final HeaderCell header1FnModParticle = new HeaderCell("Model");
  private final HeaderCell header1InitMotl = new HeaderCell("MOTL");
  private final JPanel pnlTable = new JPanel();
  private final GridBagLayout layout = new GridBagLayout();
  private final GridBagConstraints constraints = new GridBagConstraints();
  private final ExpandButton btnExpandFnVolume;
  private final ExpandButton btnExpandFnModParticle;
  private final ExpandButton btnExpandInitMotl;

  private final PeetManager manager;

  public void expand(final ExpandButton button) {
    if (button == btnExpandFnVolume) {
      rowList.expandFnVolume(btnExpandFnVolume.isExpanded());
    }
    else if (button == btnExpandFnModParticle) {
      rowList.expandFnModParticle(btnExpandFnModParticle.isExpanded());
    }
    else if (button == btnExpandInitMotl) {
      rowList.expandInitMotl(btnExpandInitMotl.isExpanded());
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

  void setParameters(final MatlabParamFile matlabParamFile) {
    MatlabParamFile.InitMotlCode initMotlCode = matlabParamFile.getInitMotlCode();
    
    for (int i = 0; i < matlabParamFile.getVolumeListSize(); i++) {
      VolumeRow row = addRow(new File(matlabParamFile.getFnVolume(i)), new File(
          matlabParamFile.getFnModParticle(i)));
      if (initMotlCode == null) {
        row.setInitMotl(new File(matlabParamFile.getInitMotlFile(i)));
      }
    }
  }

  private void action(final ActionEvent event) {
    String actionCommand = event.getActionCommand();
    if (actionCommand.equals(btnAddFnVolume.getActionCommand())) {
      addTomogram();
    }
    if (actionCommand.equals(btnSetInitMotl.getActionCommand())) {
      setInitMotl();
    }
  }

  private VolumeTable(final PeetManager manager) {
    this.manager = manager;
    //construction
    btnExpandFnVolume = ExpandButton.getInstance(this, ExpandButton.Type.MORE);
    btnExpandFnModParticle = ExpandButton.getInstance(this, ExpandButton.Type.MORE);
    btnExpandInitMotl = ExpandButton.getInstance(this, ExpandButton.Type.MORE);
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
    header1FnVolume.add(pnlTable, layout, constraints);
    constraints.weightx = 0.0;
    btnExpandFnVolume.add(pnlTable, layout, constraints);
    constraints.weightx = 0.1;
    header1FnModParticle.add(pnlTable, layout, constraints);
    constraints.weightx = 0.0;
    btnExpandFnModParticle.add(pnlTable, layout, constraints);
    constraints.weightx = 0.1;
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    header1InitMotl.add(pnlTable, layout, constraints);
    //border
    SpacedPanel pnlBorder = new SpacedPanel();
    pnlBorder.setBoxLayout(BoxLayout.Y_AXIS);
    pnlBorder.setBorder(new EtchedBorder("Boundary Table").getBorder());
    pnlBorder.add(pnlTable);
    //buttons
    pnlButtons.setLayout(new BoxLayout(pnlButtons, BoxLayout.X_AXIS));
    btnAddFnVolume.setSize();
    pnlButtons.add(btnAddFnVolume.getComponent());
    //root
    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    rootPanel.setBorder(BorderFactory.createEtchedBorder());
    rootPanel.add(pnlBorder.getContainer());
    rootPanel.add(pnlButtons);
    //actions
    VTActionListener actionListener = new VTActionListener(this);
    btnAddFnVolume.addActionListener(actionListener);
    btnSetInitMotl.addActionListener(actionListener);
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

  private void setInitMotl() {
    JFileChooser chooser = new JFileChooser(new File(manager
        .getPropertyUserDir()));
    chooser.setFileFilter(new MotlFileFilter());
    chooser.setPreferredSize(UIParameters.INSTANCE.getFileChooserDimension());
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    int returnVal = chooser.showOpenDialog(rootPanel);
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      VolumeRow row = rowList.setInitMotl(chooser.getSelectedFile());
      if (row != null) {
        row.expandInitMotl(btnExpandInitMotl.isExpanded());
        UIHarness.INSTANCE.pack(manager);
      }
    }
  }

  private VolumeRow addRow(final File fnVolume, final File fnModel) {
    return addRow(fnVolume.getName(), fnVolume.getAbsolutePath(), fnModel
        .getName(), fnModel.getAbsolutePath());
  }

  private VolumeRow addRow(final String contractedFnVolume,
      final String expandedFnVolume, final String contractedFnModParticle,
      final String expandedFnModParticle) {
    VolumeRow row = rowList.add(contractedFnVolume, expandedFnVolume,
        contractedFnModParticle, expandedFnModParticle, this, pnlTable, layout, constraints);
    row.display();
    row.expandFnVolume(btnExpandFnVolume.isExpanded());
    row.expandFnModParticle(btnExpandFnModParticle.isExpanded());
    updateDisplay();
    UIHarness.INSTANCE.pack(manager);
    return row;
  }

  private void updateDisplay() {
    boolean enable = rowList.size() > 0;
    btnExpandFnVolume.setEnabled(enable);
    btnExpandFnModParticle.setEnabled(enable);
    btnSetInitMotl.setEnabled(enable && rowList.isHighlighted());
  }

  /**
   * Uses lazy construction.
   */
  private static final class RowList {
    private final List list = new ArrayList();

    private int size() {
      return list.size();
    }

    private synchronized VolumeRow add(final String contractedFnVolume,
        final String expandedFnVolume, final String contractedFnModParticle,
        final String expandedFnModParticle, final VolumeTable table,
        final JPanel panel, final GridBagLayout layout,
        final GridBagConstraints constraints) {
      VolumeRow row = VolumeRow.getInstance(contractedFnVolume,
          expandedFnVolume, contractedFnModParticle, expandedFnModParticle, list.size(), table,
          panel, layout, constraints);
      list.add(row);
      return row;
    }

    private void expandFnVolume(final boolean expanded) {
      for (int i = 0; i < list.size(); i++) {
        ((VolumeRow) list.get(i)).expandFnVolume(expanded);
      }
    }

    private void expandFnModParticle(final boolean expanded) {
      for (int i = 0; i < list.size(); i++) {
        ((VolumeRow) list.get(i)).expandFnModParticle(expanded);
      }
    }

    private void expandInitMotl(final boolean expanded) {
      for (int i = 0; i < list.size(); i++) {
        ((VolumeRow) list.get(i)).expandInitMotl(expanded);
      }
    }

    private boolean isHighlighted() {
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
    private VolumeRow setInitMotl(final File initMotl) {
      VolumeRow row = getHighlightedRow();
      //If the motl spinner is being displayed, then the display will have to be
      //changed
      boolean changeDisplay = row.usingInitMotlSpinner();
      int index = -1;
      if (row != null) {
        index = row.setInitMotl(initMotl);
        //if display changed from spinner to text field, redisplay
        if (changeDisplay) {
          row.removeInitMotl();
        }
      }
      //if display changed from spinner to text field, redisplay
      if (changeDisplay) {
        //removed the rows following the current row
        for (int i = index + 1; i < list.size(); i++) {
          ((VolumeRow) list.get(i)).remove();
        }
        //redisplay the highlighted row
        row.displayInitMotl();
        //redisplay the rows following the current row
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
