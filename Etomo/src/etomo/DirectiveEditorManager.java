package etomo;

import java.io.File;
import java.io.IOException;

import etomo.logic.DirectiveEditorBuilder;
import etomo.process.BaseProcessManager;
import etomo.storage.DirectiveMap;
import etomo.storage.LogFile;
import etomo.storage.Storable;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.BaseMetaData;
import etomo.type.DataFileType;
import etomo.type.DialogType;
import etomo.type.DirectiveEditorMetaData;
import etomo.type.DirectiveFileType;
import etomo.type.InterfaceType;
import etomo.type.ParallelState;
import etomo.ui.swing.DirectiveEditorDialog;
import etomo.ui.swing.LogInterface;
import etomo.ui.swing.LogPanel;
import etomo.ui.swing.MainDirectiveEditorPanel;
import etomo.ui.swing.MainPanel;
import etomo.util.Utilities;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2013</p>
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
public final class DirectiveEditorManager extends BaseManager {
  public static final String rcsid = "$Id:$";

  private static final AxisID AXIS_ID = AxisID.ONLY;
  private static final DialogType DIALOG_TYPE = DialogType.DIRECTIVE_EDITOR;
  private static final int STATUS_BAR_SIZE = 65;

  private final BaseManager sourceManager;
  private final DirectiveEditorMetaData metaData;
  private final DirectiveFileType type;
  private final String timestamp;

  private MainDirectiveEditorPanel mainPanel;

  private DirectiveEditorDialog dialog = null;
  private StringBuffer dialogErrmsg = null;

  public DirectiveEditorManager(final DirectiveFileType type,
      final BaseManager sourceManager, final String timestamp, final StringBuffer errmsg) {
    this.sourceManager = sourceManager;
    this.metaData = new DirectiveEditorMetaData(type);
    this.type = type;
    this.timestamp = timestamp;
    dialogErrmsg = errmsg;
    createState();
    initializeUIParameters(null, AXIS_ID);
    // Frame hasn't been created yet so stop here.
  }

  /**
   * Open panel and add dialog.
   */
  public void initialize() {
    if (!EtomoDirector.INSTANCE.getArguments().isHeadless()) {
      openProcessingPanel();
      String location = null;
      int size = STATUS_BAR_SIZE;
      mainPanel.setStatusBarText(location, size);
      openDirectiveEditorDialog();
      uiHarness.toFront(this);
    }
  }

  /**
   * Sets the dataset name.
   * @param file
   */
  public void setName(File inputFile) {
    //TODO not being called
    propertyUserDir = inputFile.getParent();
    metaData.setRootName(inputFile);
    mainPanel.setStatusBarText(propertyUserDir, STATUS_BAR_SIZE);
    uiHarness.setTitle(this, type.getLabel() + " - " + getName());
  }

  public void updateDirectiveMap(final DirectiveMap directiveMap,
      final StringBuffer errmsg) {
    sourceManager.updateDirectiveMap(directiveMap, errmsg);
  }

  public void openDirectiveEditorDialog() {
    if (dialog == null) {
      DirectiveEditorBuilder builder = new DirectiveEditorBuilder(this);
      dialogErrmsg = builder.build(sourceManager.getBaseMetaData().getAxisType(),
          dialogErrmsg);
      dialog = DirectiveEditorDialog.getInstance(this, type, builder, sourceManager
          .getBaseMetaData().getAxisType(), sourceManager.getStatus(), timestamp,
          dialogErrmsg);
    }
    mainPanel.showProcess(dialog.getContainer(), AXIS_ID);
    String actionMessage = Utilities.prepareDialogActionMessage(
        DialogType.DIRECTIVE_EDITOR, AxisID.ONLY, null);
    if (actionMessage != null) {
      System.err.println(actionMessage);
    }
  }

  public ParallelState getState() {
    return null;
  }

  public InterfaceType getInterfaceType() {
    return InterfaceType.TOOLS;
  }

  public LogInterface getLogInterface() {
    return null;
  }

  public LogPanel getLogPanel() {
    return null;
  }

  void createMainPanel() {
    if (!EtomoDirector.INSTANCE.getArguments().isHeadless()) {
      mainPanel = new MainDirectiveEditorPanel(this);
    }
  }

  private void createState() {
  }

  public BaseMetaData getBaseMetaData() {
    return metaData;
  }

  public MainPanel getMainPanel() {
    return mainPanel;
  }

  Storable[] getStorables(final int offset) {
    return null;
  }

  public boolean isInManagerFrame() {
    return true;
  }

  public BaseProcessManager getProcessManager() {
    return null;
  }

  public void save() throws LogFile.LockException, IOException {
    super.save();
    mainPanel.done();
  }

  public boolean exitProgram(final AxisID axisID) {
    try {
      if (super.exitProgram(axisID)) {
        endThreads();
        saveParamFile();
        return true;
      }
      return false;
    }
    catch (Throwable e) {
      e.printStackTrace();
      return true;
    }
  }

  public String getName() {
    return metaData.getName();
  }

  /**
   * MUST run reconnect for all axis
   */
  private void openProcessingPanel() {
    mainPanel.showProcessingPanel(AxisType.SINGLE_AXIS);
    setPanel();
    reconnect(axisProcessData.getSavedProcessData(AxisID.ONLY), AxisID.ONLY, false);
  }

  /**
   * Class identifies dataset files that conflict with the member variable
   * compareFileName.  Used to avoid file name collisions between tools projects
   * and exclusive datasets.  Ignores parallel processing datasets because these
   * are file oriented datasets like tools projects.
   * @author sueh
   *
   */
  private static final class ConflictFileFilter extends
      javax.swing.filechooser.FileFilter implements java.io.FileFilter {
    private final String compareFileName;

    private ConflictFileFilter(final String compareFileName) {
      this.compareFileName = compareFileName;
    }

    /**
     * @return true if file is in conflict with compareFileName
     */
    public boolean accept(final File file) {
      // If this file has one of the five exclusive dataset extensions and the
      // left side of the file name is equal to compareFileName, then
      // compareFileName is in conflict with the dataset in this directory and
      // may cause file name collisions.
      if (file.isFile()) {
        String fileName = file.getName();
        if (fileName.endsWith(DataFileType.RECON.extension)
            || fileName.endsWith(DataFileType.JOIN.extension)
            || fileName.endsWith(DataFileType.PEET.extension)
            || fileName.endsWith(DataFileType.SERIAL_SECTIONS.extension)) {
          return fileName.substring(0, fileName.lastIndexOf('.')).equals(compareFileName);
        }
      }
      return false;
    }

    /**
     * @see javax.swing.filechooser.FileFilter#getDescription()
     */
    public String getDescription() {
      return "Dataset file that conflicts with " + compareFileName;
    }
  }
}
