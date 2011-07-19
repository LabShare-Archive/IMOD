package etomo.ui.swing;

import java.awt.Container;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.border.LineBorder;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.JoinManager;
import etomo.storage.LogFile;
import etomo.type.ConstJoinMetaData;
import etomo.type.JoinMetaData;
import etomo.type.JoinScreenState;

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
 * <p> Revision 1.2  2011/02/22 18:03:20  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.9  2009/02/04 23:36:48  sueh
 * <p> bug# 1158 Changed id and exception classes in LogFile.
 * <p>
 * <p> Revision 1.8  2009/01/20 19:48:46  sueh
 * <p> bug# 1102 Changed labeled panels to type EtomoPanel so that they can
 * <p> name themselves.  Added getAdjustedHeaderCell so that the adjusted
 * <p> column cells can name themselves.  Made the table label available for
 * <p> the same reason.  Calling row.setNames when creating a new row in
 * <p> RowList.
 * <p>
 * <p> Revision 1.7  2008/10/07 16:42:28  sueh
 * <p> bug# 1113 Changed Viewport.msgViewportMoved to msgViewportPaged.
 * <p>
 * <p> Revision 1.6  2008/10/06 22:38:32  sueh
 * <p> bug# 1113 Setting table size from UserConfiguration.
 * <p>
 * <p> Revision 1.5  2008/10/01 22:51:15  sueh
 * <p> bug# 1113 Renamed repositionViewer to msgViewportMoved.
 * <p>
 * <p> Revision 1.4  2008/09/30 20:58:26  sueh
 * <p> bug# 1113 Implemented Viewable.  Added a Viewport member.
 * <p>
 * <p> Revision 1.3  2008/01/31 20:25:48  sueh
 * <p> bug# 1055 throwing a FileException when LogFile.getInstance fails.
 * <p>
 * <p> Revision 1.2  2007/02/09 00:47:21  sueh
 * <p> bug# 962 Added tooltips.
 * <p>
 * <p> Revision 1.1  2007/02/05 23:34:00  sueh
 * <p> bug# 962 Class representing the boundary table.
 * <p> </p>
 */
final class BoundaryTable implements Viewable {
  public static final String rcsid = "$Id$";

  static final String TABLE_LABEL = "Boundary Table";

  //header
  //first row
  private final HeaderCell header1Boundaries = new HeaderCell("Boundaries");
  private final HeaderCell header1Sections = new HeaderCell("Sections");
  private final HeaderCell header1BestGap = new HeaderCell("Best");
  private final HeaderCell header1Error = new HeaderCell("Error");
  private final HeaderCell header1Original = new HeaderCell("Original");
  private final HeaderCell header1Adjusted = new HeaderCell("Adjusted");
  //second row
  private final HeaderCell header2Boundaries = new HeaderCell();
  private final HeaderCell header2Sections = new HeaderCell();
  private final HeaderCell header2BestGap = new HeaderCell("Gap");
  private final HeaderCell header2MeanError = new HeaderCell("Mean");
  private final HeaderCell header2MaxError = new HeaderCell("Max");
  private final HeaderCell header2OriginalEnd = new HeaderCell();
  private final HeaderCell header2OriginalStart = new HeaderCell();
  private final HeaderCell header2AdjustedEnd = new HeaderCell();
  private final HeaderCell header2AdjustedStart = new HeaderCell();
  //third row
  private final HeaderCell header3Sections = new HeaderCell();
  private final HeaderCell header3BestGap = new HeaderCell();
  private final HeaderCell header3OriginalEnd = new HeaderCell("End");
  private final HeaderCell header3OriginalStart = new HeaderCell("Start");
  private final HeaderCell header3AdjustedEnd = new HeaderCell("End");
  private final HeaderCell header3AdjustedStart = new HeaderCell("Start");

  private final RowList rowList = new RowList(this);
  private final JPanel rootPanel = new JPanel();
  private final GridBagConstraints constraints = new GridBagConstraints();
  private final JPanel pnlTable = new JPanel();
  private final GridBagLayout layout = new GridBagLayout();

  private final Viewport viewport;
  private final JoinManager manager;
  private final JoinDialog parent;
  private final JoinScreenState screenState;
  private final JoinMetaData metaData;

  private boolean rowChange = true;
  private JoinDialog.Tab tab = null;

  BoundaryTable(final JoinManager manager, final JoinDialog joinDialog) {
    this.manager = manager;
    screenState = manager.getScreenState();
    metaData = manager.getJoinMetaData();
    parent = joinDialog;
    viewport = new Viewport(this, EtomoDirector.INSTANCE.getUserConfiguration()
        .getJoinTableSize().getInt(), joinDialog.getModelTabJComponent(), joinDialog
        .getRejoinTabJComponent(), null, "Boundary");
    //construct panels
    EtomoPanel pnlBorder = new EtomoPanel();
    //root panel
    rootPanel.setFocusable(true);
    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    rootPanel.add(pnlBorder);
    rootPanel.add(Box.createRigidArea(FixedDim.x0_y40));
    rootPanel.add(Box.createRigidArea(FixedDim.x0_y20));
    //border pane
    pnlBorder.setLayout(new BoxLayout(pnlBorder, BoxLayout.X_AXIS));
    pnlBorder.setBorder(new EtchedBorder(TABLE_LABEL).getBorder());
    pnlBorder.add(pnlTable);
    pnlBorder.add(viewport.getPagingPanel());
    //table panel
    pnlTable.setLayout(layout);
    pnlTable.setBorder(LineBorder.createBlackLineBorder());
    constraints.fill = GridBagConstraints.BOTH;
    header1BestGap.pad();
    header3OriginalEnd.pad();
    header3OriginalStart.pad();
    setToolTipText();
  }

  void setXfjointomoResult() throws LogFile.LockException, FileNotFoundException,
      IOException {
    rowList.setXfjointomoResult(manager);
  }

  /**
   * Updates and displays the table as necessary.  Does nothing if the tab has
   * not changed and rowChange is false.
   */
  void display() {
    display(false);
  }

  public void msgViewportPaged() {
    display(true);
  }

  public int size() {
    return rowList.size();
  }

  /** 
   * Updates and displays the table as necessary.  Does nothing if the tab has
   * not changed and rowChange is false.  Always updates if force is true.
   */
  void display(final boolean force) {
    JoinDialog.Tab oldTab = tab;
    tab = parent.getTab();
    if (!force && oldTab == tab && !rowChange) {
      return;
    }
    rowList.removeDisplay();
    pnlTable.removeAll();
    addHeader(tab);
    addRows();
    manager.getMainPanel().repaint();
  }

  /**
   * Causes the rows in the table to be deleted and recreated when the table is
   * displayed.
   */
  void msgRowChange() {
    rowChange = true;
    //when addRows() is called, it will load from screenState and metaData, so
    //they need to be empty if they are out of date.
    BoundaryRow.resetScreenState(screenState);
    BoundaryRow.resetMetaData(metaData);
  }

  void getScreenState() {
    BoundaryRow.resetScreenState(screenState);
    rowList.getScreenState(screenState);
  }

  HeaderCell getAdjustedHeaderCell() {
    return header1Adjusted;
  }

  void getMetaData() {
    BoundaryRow.resetMetaData(metaData);
    rowList.getMetaData(metaData);
  }

  Container getContainer() {
    return rootPanel;
  }

  private void addHeader(final JoinDialog.Tab tab) {
    if (tab == JoinDialog.Tab.MODEL) {
      addModelHeader();
    }
    else if (tab == JoinDialog.Tab.REJOIN) {
      addRejoinHeader();
    }
  }

  private void addModelHeader() {
    //Header
    //First row
    constraints.anchor = GridBagConstraints.CENTER;
    constraints.weightx = 0.0;
    constraints.weighty = 0.0;
    constraints.gridheight = 1;
    constraints.gridwidth = 1;
    header1Boundaries.add(pnlTable, layout, constraints);
    constraints.weightx = 0.1;
    header1BestGap.add(pnlTable, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    header1Error.add(pnlTable, layout, constraints);
    //second row
    constraints.weightx = 0.0;
    constraints.gridwidth = 1;
    header2Boundaries.add(pnlTable, layout, constraints);
    constraints.weightx = 0.1;
    header2BestGap.add(pnlTable, layout, constraints);
    header2MeanError.add(pnlTable, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    header2MaxError.add(pnlTable, layout, constraints);
  }

  private void addRejoinHeader() {
    //Header
    //First row
    constraints.anchor = GridBagConstraints.CENTER;
    constraints.weightx = 0.0;
    constraints.weighty = 0.0;
    constraints.gridheight = 1;
    constraints.gridwidth = 1;
    header1Sections.add(pnlTable, layout, constraints);
    constraints.weightx = 0.1;
    constraints.gridwidth = 2;
    header1Original.add(pnlTable, layout, constraints);
    constraints.gridwidth = 1;
    header1BestGap.add(pnlTable, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    header1Adjusted.add(pnlTable, layout, constraints);
    //second row
    constraints.weightx = 0.0;
    constraints.gridwidth = 1;
    header2Sections.add(pnlTable, layout, constraints);
    constraints.weightx = 0.1;
    header2OriginalEnd.add(pnlTable, layout, constraints);
    header2OriginalStart.add(pnlTable, layout, constraints);
    header2BestGap.add(pnlTable, layout, constraints);
    header2AdjustedEnd.add(pnlTable, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    header2AdjustedStart.add(pnlTable, layout, constraints);
    //third row
    constraints.weightx = 0.0;
    constraints.gridwidth = 1;
    header3Sections.add(pnlTable, layout, constraints);
    constraints.weightx = 0.1;
    header3OriginalEnd.add(pnlTable, layout, constraints);
    header3OriginalStart.add(pnlTable, layout, constraints);
    header3BestGap.add(pnlTable, layout, constraints);
    header3AdjustedEnd.add(pnlTable, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    header3AdjustedStart.add(pnlTable, layout, constraints);
  }

  private void setToolTipText() {
    String text = "Boundaries between sections.";
    header1Boundaries.setToolTipText(text);
    header2Boundaries.setToolTipText(text);
    text = "The pairs of sections which define each boundary.";
    header1Sections.setToolTipText(text);
    header2Sections.setToolTipText(text);
    header3Sections.setToolTipText(text);
    text = "Describes how the final start and end values will change when the join is recreated, "
        + "with a positive gap adding slices and a negative gap removing slices at the corresponding boundary.";
    header1BestGap.setToolTipText(text);
    header2BestGap.setToolTipText(text);
    header3BestGap.setToolTipText(text);
    header1Error
        .setToolTipText("Deviations between transformed points extrapolated from above and below the corresponding boundary.");
    header2MeanError.setToolTipText("Mean deviations.");
    header2MaxError.setToolTipText("Maximum deviations.");
    text = "End and start values used to create the original join.";
    header1Original.setToolTipText(text);
    header2OriginalEnd.setToolTipText(text);
    header2OriginalStart.setToolTipText(text);
    header3OriginalEnd.setToolTipText("End values used to create the original join.");
    header3OriginalStart.setToolTipText("Start values used to create the original join.");
    text = "End and start values which will be used to create the new join.";
    header1Adjusted.setToolTipText(text);
    header2AdjustedEnd.setToolTipText(text);
    header2AdjustedStart.setToolTipText(text);
    header3AdjustedEnd
        .setToolTipText("End values which will be used to create the new join.");
    header3AdjustedStart
        .setToolTipText("Start values which will be used to create the new join.");
  }

  /**
   * Displays the rows.  Updates the rows when rowChange is true.  The number of
   * rows to add is the section table size minus 1.
   */
  private void addRows() {
    if (rowChange) {
      rowChange = false;
      rowList.clear(viewport);
      parent.getSectionTable().getMetaData(metaData);
      rowList.add(parent.getSectionTableSize() - 1, metaData, screenState, pnlTable,
          layout, constraints, viewport);
    }
    rowList.display(tab, viewport);
  }

  /**
   * A list of BoundaryRow classes.  Has the functionality of an array and can
   * also run BoundaryRow functions on the whole list.
   */
  private static final class RowList {
    private final ArrayList list = new ArrayList();

    private final BoundaryTable table;

    private RowList(BoundaryTable table) {
      this.table = table;
    }

    /**
     * Clears the list.
     */
    private void clear(final Viewport viewport) {
      list.clear();

    }

    /**
     * @return the list size
     */
    private int size() {
      return list.size();
    }

    /**
     * Runs BoundaryRow.removeDisplay().
     */
    private void removeDisplay() {
      for (int i = 0; i < list.size(); i++) {
        get(i).removeDisplay();
      }
    }

    /**
     * BoundaryRow.display() on rows that are in the viewer.
     */
    private void display(final JoinDialog.Tab tab, final Viewport viewport) {
      for (int i = 0; i < list.size(); i++) {
        get(i).display(i, viewport, tab);
      }
    }

    private void setXfjointomoResult(final BaseManager manager)
        throws LogFile.LockException, FileNotFoundException, IOException {
      for (int i = 0; i < list.size(); i++) {
        get(i).setXfjointomoResult(manager);
      }
    }

    private void getScreenState(final JoinScreenState screenState) {
      for (int i = 0; i < list.size(); i++) {
        get(i).getScreenState(screenState);
      }
    }

    private void getMetaData(final JoinMetaData metaData) {
      for (int i = 0; i < list.size(); i++) {
        get(i).getMetaData(metaData);
      }
    }

    /**
     * Adds new BoundaryRow instances to the list.  The number of instances
     * added equals size.  The number parameter in the BoundaryRow
     * constructor starts at 1.
     * @param size
     * @param panel
     * @param layout
     * @param constraints
     */
    private void add(final int size, final ConstJoinMetaData metaData,
        final JoinScreenState screenState, final JPanel panel,
        final GridBagLayout layout, final GridBagConstraints constraints,
        final Viewport viewport) {
      for (int i = 0; i < size; i++) {
        BoundaryRow row = new BoundaryRow(i + 1, metaData, screenState, panel, layout,
            constraints, table);
        list.add(row);
        row.setNames();
      }
    }

    /**
     * @param index
     * @return an element of the list
     */
    private BoundaryRow get(final int index) {
      return (BoundaryRow) list.get(index);
    }
  }
}
