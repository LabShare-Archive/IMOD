package etomo.ui.swing;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;

import javax.swing.ButtonGroup;
import javax.swing.JPanel;

import etomo.BaseManager;
import etomo.comscript.IntermittentCommand;
import etomo.comscript.ProcesschunksParam;
import etomo.comscript.QueuechunkParam;
import etomo.storage.CpuAdoc;
import etomo.storage.Network;
import etomo.storage.Node;
import etomo.type.AxisID;
import etomo.type.ConstEtomoVersion;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2010</p>
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
* <p> Revision 1.3  2011/07/18 22:44:59  sueh
* <p> Bug# 1515 Removed isSelectOnlyRow - no longer needed.
* <p>
* <p> Revision 1.2  2011/02/22 18:20:39  sueh
* <p> bug# 1437 Reformatting.
* <p>
* <p> Revision 1.1  2011/02/03 06:17:02  sueh
* <p> bug# 1422 Child of ProcessorTable that makes a ProcessorTable display
* <p> queues.
* <p> </p>
*/

final class QueueTable extends ProcessorTable {
  public static final String rcsid = "$Id$";

  private static final String PREPEND = "ProcessorTable.Queue";

  private HeaderCell[] header1LoadArray = null;
  private HeaderCell[] header2LoadArray = null;
  private ButtonGroup buttonGroup = null;

  QueueTable(final BaseManager manager, final ParallelPanel parent, final AxisID axisID) {
    super(manager, parent, axisID, true);
  }

  String getStorePrepend() {
    return PREPEND;
  }

  String getLoadPrepend(ConstEtomoVersion version) {
    return PREPEND;
  }

  private void createHeader1LoadArray() {
    if (header1LoadArray != null) {
      return;
    }
    String[] loadUnitsArray = null;
    loadUnitsArray = CpuAdoc.INSTANCE.getLoadUnits(manager, axisID,
        manager.getPropertyUserDir());
    if (loadUnitsArray.length == 0) {
      header1LoadArray = new HeaderCell[1];
      header1LoadArray[0] = new HeaderCell("Load");
    }
    else {
      header1LoadArray = new HeaderCell[loadUnitsArray.length];
      for (int i = 0; i < loadUnitsArray.length; i++) {
        header1LoadArray[i] = new HeaderCell(loadUnitsArray[i]);
      }
    }
  }

  private void createHeader2LoadArray() {
    if (header2LoadArray != null) {
      return;
    }
    createHeader1LoadArray();
    header2LoadArray = new HeaderCell[header1LoadArray.length];
    for (int i = 0; i < header2LoadArray.length; i++) {
      header2LoadArray[i] = new HeaderCell();
    }
  }

  int getSize() {
    buttonGroup = new ButtonGroup();
    return Network.getNumQueues(manager, axisID, manager.getPropertyUserDir());
  }

  Node getNode(final int index) {
    return Network.getQueue(manager, index, axisID, manager.getPropertyUserDir());
  }

  ProcessorTableRow createProcessorTableRow(final ProcessorTable processorTable,
      final Node node, final int numRowsInTable) {
    return ProcessorTableRow
        .getQueueInstance(
            processorTable,
            node,
            node.getNumber(),
            buttonGroup,
            Math.max(
                1,
                CpuAdoc.INSTANCE.getLoadUnits(manager, axisID,
                    manager.getPropertyUserDir()).length), numRowsInTable);
  }

  String getHeader1ComputerText() {
    return "Queue";
  }

  String getNoCpusSelectedErrorMessage() {
    return "A queue must be selected.";
  }

  void addHeader1Load(final JPanel tablePanel, final GridBagLayout layout,
      final GridBagConstraints constraints) {
    constraints.gridwidth = 1;
    createHeader1LoadArray();
    for (int i = 0; i < header1LoadArray.length; i++) {
      header1LoadArray[i].add(tablePanel, layout, constraints);
    }
  }

  void addHeader1Users(final JPanel tablePanel, final GridBagLayout layout,
      final GridBagConstraints constraints) {
    // The users column contains a load value, so it can't be used when
    // displaying queues.
  }

  void addHeader2Load(final JPanel tablePanel, final GridBagLayout layout,
      final GridBagConstraints constraints) {
    createHeader2LoadArray();
    for (int i = 0; i < header2LoadArray.length; i++) {
      header2LoadArray[i].add(tablePanel, layout, constraints);
    }
  }

  void addHeader2Users(final JPanel tablePanel, final GridBagLayout layout,
      final GridBagConstraints constraints) {
  }

  boolean useUsersColumn() {
    return false;
  }

  void getParameters(final ProcesschunksParam param) {
    String queue = getFirstSelectedComputer();
    Node node = Network.getQueue(manager, queue, axisID, manager.getPropertyUserDir());
    if (node != null) {
      param.setQueueCommand(node.getCommand());
    }
    param.setQueue(queue);
    super.getParameters(param);
  }

  IntermittentCommand getIntermittentCommand(final String computer) {
    return QueuechunkParam.getLoadInstance(computer, axisID, manager);
  }

  void setHeaderLoadToolTipText() {
  }

  void setHeaderUsersToolTipText() {
  }

  boolean isExcludeNode(final Node node) {
    return false;
  }

  boolean isNiceable() {
    return true;
  }

  void initRow(ProcessorTableRow row) {
  }
}
