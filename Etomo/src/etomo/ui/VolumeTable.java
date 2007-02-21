package etomo.ui;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ArrayList;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JFileChooser;
import javax.swing.JPanel;

import etomo.PeetManager;

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
 * <p> Revision 1.1  2007/02/20 20:37:32  sueh
 * <p> bug# 964 A list of tomograms for the PEET interface.
 * <p> </p>
 */
final class VolumeTable {
  public static final String rcsid = "$Id$";

  private final RowList rowList = new RowList();
  private final MultiLineButton btnAddTomogram = new MultiLineButton(
      "Add Tomogram");
  private final JPanel pnlButtons = new JPanel();
  private final PeetManager manager;
  private JPanel rootPanel = null;

  VolumeTable(PeetManager manager) {
    this.manager = manager;
  }

  Component getComponent() {
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
    //buttons
    pnlButtons.setLayout(new BoxLayout(pnlButtons, BoxLayout.X_AXIS));
    btnAddTomogram.setSize();
    pnlButtons.add(btnAddTomogram.getComponent());
    //root
    rootPanel = new JPanel();
    rootPanel.setBorder(BorderFactory.createEtchedBorder());
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
  }

  private static final class RowList {
    private final ArrayList list = new ArrayList();

    int size() {
      return list.size();
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
