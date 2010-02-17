package etomo.ui;

import java.awt.Container;
import java.awt.event.MouseEvent;

import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.text.BadLocationException;

import etomo.BaseManager;
import etomo.ToolsManager;
import etomo.comscript.ConstWarpVolParam;
import etomo.storage.Loggable;
import etomo.type.AxisID;
import etomo.type.DialogType;
import etomo.type.ToolType;

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
 * <p> $Log$ </p>
 */
public final class ToolsDialog implements ContextMenu, LogInterface {
  public static final String rcsid = "$Id$";

  private final JPanel pnlRoot = new JPanel();
  private final JTextArea taTaskLog = new JTextArea();
  private final JScrollPane scrTaskLog = new JScrollPane(taTaskLog);
  private final EtomoLogger logger = new EtomoLogger(this);

  private final FlattenVolumePanel flattenVolumePanel;
  private final ToolType toolType;
  private final BaseManager manager;

  private ToolsDialog(final ToolsManager manager, final AxisID axisID,
      final DialogType dialogType, final ToolType toolType) {
    this.toolType = toolType;
    this.manager = manager;
    if (toolType == ToolType.FLATTEN_VOLUME) {
      flattenVolumePanel = FlattenVolumePanel.getToolsInstance(manager, axisID,
          dialogType);
    }
    else {
      flattenVolumePanel = null;
    }
  }

  public static ToolsDialog getInstance(final ToolsManager manager,
      final AxisID axisID, final DialogType dialogType, final ToolType toolType) {
    ToolsDialog instance = new ToolsDialog(manager, axisID, dialogType,
        toolType);
    instance.createPanel();
    return instance;
  }

  public void setParameters(final ConstWarpVolParam param) {
    flattenVolumePanel.setParameters(param);
  }

  public void logMessage(Loggable loggable, AxisID axisID) {
    logger.logMessage(loggable, axisID);
  }

  public void logMessage(String title, AxisID axisID, String[] message) {
    logger.logMessage(title, axisID, message);
  }

  public void save() {
  }

  public void msgChanged() {
  }

  public void append(String line) {
    taTaskLog.append(line);
  }

  public int getLineEndOffset() throws BadLocationException {
    return taTaskLog.getLineEndOffset(taTaskLog.getLineCount() - 1);
  }

  private void createPanel() {
    //initialize
    taTaskLog.setEditable(false);
    taTaskLog.setRows(5);
    taTaskLog.setLineWrap(true);
    taTaskLog.setWrapStyleWord(true);
    //Root panel
    pnlRoot.setLayout(new BoxLayout(pnlRoot, BoxLayout.Y_AXIS));
    if (toolType == ToolType.FLATTEN_VOLUME) {
      pnlRoot.add(flattenVolumePanel.getComponent());
    }
    pnlRoot.add(scrTaskLog);
    UIHarness.INSTANCE.pack(manager);
  }

  public Container getContainer() {
    return pnlRoot;
  }

  public boolean usingParallelProcessing() {
    return false;
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
  }
}
