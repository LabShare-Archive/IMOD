package etomo.ui.swing;

import java.awt.Container;
import java.awt.event.MouseEvent;
import java.io.File;
import java.util.List;

import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.text.BadLocationException;

import etomo.BaseManager;
import etomo.ToolsManager;
import etomo.comscript.ConstWarpVolParam;
import etomo.comscript.GpuTiltTestParam;
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
 * <p> $Log$
 * <p> Revision 1.3  2011/02/22 21:41:07  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.2  2011/02/03 06:22:16  sueh
 * <p> bug# 1422 Control of the processing method has been centralized in the
 * <p> processing method mediator class.  Implementing ProcessInterface.
 * <p> Supplying processes with the current processing method.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.1  2010/02/17 04:56:58  sueh
 * <p> bug# 1301 Main dialog for the ToolsManager.
 * <p> </p>
 */
public final class ToolsDialog implements ContextMenu, LogInterface {
  public static final String rcsid = "$Id$";

  private final JPanel pnlRoot = new JPanel();
  private final JTextArea taTaskLog = new JTextArea();
  private final JScrollPane scrTaskLog = new JScrollPane(taTaskLog);
  private final EtomoLogger logger = new EtomoLogger(this);

  private final ToolPanel toolPanel;
  private final ToolType toolType;
  private final BaseManager manager;

  private ToolsDialog(final ToolsManager manager, final AxisID axisID,
      final DialogType dialogType, final ToolType toolType) {
    this.toolType = toolType;
    this.manager = manager;
    if (toolType == ToolType.FLATTEN_VOLUME) {
      toolPanel = FlattenVolumePanel.getToolsInstance(manager, axisID, dialogType);
    }
    else if (toolType == ToolType.GPU_TILT_TEST) {
      toolPanel = GpuTiltTestPanel.getInstance(manager, axisID);
    }
    else {
      toolPanel = null;
    }
  }

  public static ToolsDialog getInstance(final ToolsManager manager, final AxisID axisID,
      final DialogType dialogType, final ToolType toolType) {
    ToolsDialog instance = new ToolsDialog(manager, axisID, dialogType, toolType);
    instance.createPanel();
    return instance;
  }

  public void setParameters(final ConstWarpVolParam param) {
    ((FlattenVolumePanel) toolPanel).setParameters(param);
  }

  public boolean getParameters(final GpuTiltTestParam param, final boolean doValidation) {
    return ((GpuTiltTestPanel) toolPanel).getParameters(param, doValidation);
  }

  public void logMessage(Loggable loggable, AxisID axisID) {
    logger.logMessage(loggable, axisID);
  }

  public void logMessage(String title, AxisID axisID, String[] message) {
    logger.logMessage(title, axisID, message);
  }

  public void logMessage(String title, AxisID axisID, List<String> message) {
    logger.logMessage(title, axisID, message);
  }

  public void logMessage(String title, AxisID axisID) {
    logger.logMessage(title, axisID);
  }

  public void logMessage(final String message) {
    logger.logMessage(message);
  }

  public void logMessage(final File file) {
    logger.logMessage(file);
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
    // initialize
    taTaskLog.setEditable(false);
    taTaskLog.setRows(5);
    taTaskLog.setLineWrap(true);
    taTaskLog.setWrapStyleWord(true);
    // Root panel
    pnlRoot.setLayout(new BoxLayout(pnlRoot, BoxLayout.Y_AXIS));
    if (toolPanel != null) {
      pnlRoot.add(toolPanel.getComponent());
    }
    pnlRoot.add(scrTaskLog);
    UIHarness.INSTANCE.pack(manager);
  }

  public Container getContainer() {
    return pnlRoot;
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
  }
}
