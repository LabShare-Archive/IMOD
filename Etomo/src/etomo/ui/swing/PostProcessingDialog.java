package etomo.ui.swing;

import java.awt.Container;
import java.awt.event.*;

import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import etomo.ApplicationManager;
import etomo.comscript.ConstSqueezevolParam;
import etomo.comscript.ConstWarpVolParam;
import etomo.comscript.SqueezevolParam;
import etomo.comscript.TrimvolParam;
import etomo.comscript.WarpVolParam;
import etomo.logic.TrimvolInputFileState;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstMetaData;
import etomo.type.DialogType;
import etomo.type.MetaData;
import etomo.type.ReconScreenState;

/**
 * <p>Description: </p>
 *
 * <p>Copyright: Copyright (c) 2002 - 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 */
public final class PostProcessingDialog extends ProcessDialog implements ContextMenu {
  public static final String rcsid = "$Id$";

  private final TrimvolPanel trimvolPanel;

  private final TabbedPane tabbedPane = new TabbedPane();

  private Tab curTab = Tab.DEFAULT;
  private final FlattenVolumePanel flattenVolumePanel;
  private final SqueezeVolPanel squeezeVolPanel;
  private final boolean lockDialog;

  private PostProcessingDialog(final ApplicationManager appMgr, final boolean lockDialog) {
    super(appMgr, AxisID.ONLY, DialogType.POST_PROCESSING);
    this.lockDialog = lockDialog;
    flattenVolumePanel = FlattenVolumePanel.getPostInstance(appMgr, axisID, dialogType,
        lockDialog);
    squeezeVolPanel = SqueezeVolPanel.getInstance(appMgr, axisID, dialogType, lockDialog);
    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    rootPanel.setBorder(new BeveledBorder("Post Processing").getBorder());
    rootPanel.add(tabbedPane);
    trimvolPanel = new TrimvolPanel(applicationManager, axisID, dialogType, lockDialog);
    JPanel trimvolRoot = new JPanel();
    tabbedPane.addTab("Trim vol", trimvolRoot);
    trimvolRoot.add(trimvolPanel.getContainer());
    JPanel flattenRoot = new JPanel();
    tabbedPane.addTab("Flatten", flattenRoot);
    JPanel squeezeRoot = new JPanel();
    tabbedPane.addTab("Squeeze vol", squeezeRoot);
    addExitButtons();
    btnAdvanced.setVisible(false);
    btnExecute.setText("Done");
  }

  public static PostProcessingDialog getInstance(final ApplicationManager manager,
      final boolean lockDialog) {
    PostProcessingDialog instance = new PostProcessingDialog(manager, lockDialog);
    instance.addListeners();
    instance.tabbedPane.setSelectedIndex(Tab.DEFAULT.toInt());
    return instance;
  }

  private void addListeners() {
    // Mouse adapter for context menu
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    rootPanel.addMouseListener(mouseAdapter);
    tabbedPane.addMouseListener(mouseAdapter);
    tabbedPane.addChangeListener(new TabChangeListener(this));
  }

  private void changeTab(ConstEtomoNumber index) {
    if (index == null || index.isNull()) {
      return;
    }
    tabbedPane.setSelectedIndex(index.getInt());
    changeTab();
  }

  private void changeTab() {
    ((Container) tabbedPane.getComponentAt(curTab.toInt())).removeAll();
    curTab = Tab.getInstance(tabbedPane.getSelectedIndex());
    Container panel = (Container) tabbedPane.getSelectedComponent();
    if (curTab == Tab.TRIM_VOL) {
      panel.add(trimvolPanel.getContainer());
    }
    else if (curTab == Tab.FLATTEN) {
      panel.add(flattenVolumePanel.getComponent());
    }
    else if (curTab == Tab.SQUEEZE_VOL) {
      panel.add(squeezeVolPanel.getComponent());
    }
    UIHarness.INSTANCE.pack(axisID, applicationManager);
  }

  /**
   * Set the panel values with the specified parameters
   * @param squeezevolParam
   */
  public void setParameters(ConstSqueezevolParam squeezevolParam) {
    if (lockDialog) {
      return;
    }
    squeezeVolPanel.setParameters(squeezevolParam);
  }

  public void setParameters(ReconScreenState screenState) {
    if (lockDialog) {
      return;
    }
    trimvolPanel.setParameters(screenState);
    squeezeVolPanel.setParameters(screenState);
  }

  public void setParameters(final TrimvolParam param) {
    if (lockDialog) {
      return;
    }
    trimvolPanel.setParameters(param);
  }

  public void setParameters(final ConstMetaData metaData, final boolean dialogExists) {
    if (lockDialog) {
      return;
    }
    trimvolPanel.setParameters(metaData, dialogExists);
    flattenVolumePanel.setParameters(metaData);
    squeezeVolPanel.setParameters(metaData);
    changeTab(metaData.getPostCurTab());
  }

  public FlattenWarpDisplay getFlattenWarpDisplay() {
    return flattenVolumePanel.getFlattenWarpDisplay();
  }

  public void setStartupWarnings(final TrimvolInputFileState inputFileState) {
    trimvolPanel.setStartupWarnings(inputFileState);
  }

  public void getParameters(final MetaData metaData) {
    if (lockDialog) {
      return;
    }
    trimvolPanel.getParameters(metaData);
    flattenVolumePanel.getParameters(metaData);
    squeezeVolPanel.getParameters(metaData);
    metaData.setPostCurTab(curTab.index);
  }

  public void getParametersForTrimvol(final MetaData metaData) {
    if (lockDialog) {
      return;
    }
    trimvolPanel.getParametersForTrimvol(metaData);
  }

  public boolean getParameters(WarpVolParam param, final boolean doValidation) {
    if (lockDialog) {
      return true;
    }
    return flattenVolumePanel.getParameters(param, doValidation);
  }

  public void setParameters(ConstWarpVolParam param) {
    if (lockDialog) {
      return;
    }
    flattenVolumePanel.setParameters(param);
  }

  /**
   * Get the panel values
   * @param squeezevolParam
   */
  public boolean getParameters(SqueezevolParam squeezevolParam, final boolean doValidation) {
    if (lockDialog) {
      return true;
    }
    return squeezeVolPanel.getParameters(squeezevolParam, doValidation);
  }

  /**
   * Get the trimvol parameter values from the panel 
   * @param trimvolParam
   */
  public boolean getParameters(final TrimvolParam trimvolParam, final boolean doValidation) {
    if (lockDialog) {
      return true;
    }
    return trimvolPanel.getParameters(trimvolParam, doValidation);
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] manPagelabel = { "Trimvol" };
    String[] manPage = { "trimvol.html" };

    // ContextPopup contextPopup =
    new ContextPopup(rootPanel, mouseEvent, "POST-PROCESSING", ContextPopup.TOMO_GUIDE,
        manPagelabel, manPage, applicationManager, axisID);
  }

  void done() {
    applicationManager.donePostProcessing();
    squeezeVolPanel.done();
    trimvolPanel.done();
    flattenVolumePanel.done();
    setDisplayed(false);
  }

  private static final class TabChangeListener implements ChangeListener {
    private final PostProcessingDialog adaptee;

    private TabChangeListener(final PostProcessingDialog dialog) {
      adaptee = dialog;
    }

    public void stateChanged(final ChangeEvent event) {
      adaptee.changeTab();
    }
  }

  static final class Tab {
    private static final Tab TRIM_VOL = new Tab(0);
    private static final Tab FLATTEN = new Tab(1);
    private static final Tab SQUEEZE_VOL = new Tab(2);

    static final Tab DEFAULT = TRIM_VOL;

    private final int index;

    private Tab(final int index) {
      this.index = index;
    }

    private static Tab getInstance(final int index) {
      if (index == TRIM_VOL.index) {
        return TRIM_VOL;
      }
      if (index == FLATTEN.index) {
        return FLATTEN;
      }
      if (index == SQUEEZE_VOL.index) {
        return SQUEEZE_VOL;
      }
      return DEFAULT;
    }

    private int toInt() {
      return index;
    }
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.3  2011/02/22 18:19:28  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.2  2010/12/05 05:14:08  sueh
 * <p> bug# 1420 Moved ProcessResultDisplayFactory to etomo.ui.swing package.  Removed static button construction functions.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 3.49  2010/10/11 20:39:46  sueh
 * <p> bug# 1379 Removed flatten and squeezevol items from popup menu.
 * <p>
 * <p> Revision 3.48  2010/02/17 05:03:12  sueh
 * <p> bug# 1301 Using manager instead of manager key for popping up messages.
 * <p>
 * <p> Revision 3.47  2009/12/19 01:18:56  sueh
 * <p> bug# 1294 Added smoothingAssessmentPanel to FlattenWarpPanel.
 * <p>
 * <p> Revision 3.46  2009/10/16 21:15:16  sueh
 * <p> bug# 1230 set/getParameters(Const/MetaData) setting and getting curTab
 * <p> from meta data.  Added changeTab(ConstEtomoNumber) to handle
 * <p> changing the curTab.
 * <p>
 * <p> Revision 3.45  2009/10/01 18:51:19  sueh
 * <p> bug# 1239 Changed getFlattenWarpDisplay to getFlattenWarpButton.
 * <p>
 * <p> Revision 3.44  2009/09/17 19:12:40  sueh
 * <p> Removed unnecessary print.
 * <p>
 * <p> Revision 3.43  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 3.42  2009/06/22 15:33:19  sueh
 * <p> bug# 1224 Added a log message to done().
 * <p>
 * <p> Revision 3.41  2009/06/11 16:59:29  sueh
 * <p> bug# 1221 Removed no longer used getParameters(FlattenWarpParam).
 * <p> The manager is going straight to the panel now.
 * <p>
 * <p> Revision 3.40  2009/06/05 02:14:43  sueh
 * <p> bug# 1219 Added tabs.  Factored out squeezevol.  Attached
 * <p> SqueezeVolPanel and FlattenPanel.
 * <p>
 * <p> Revision 3.39  2008/10/16 22:30:27  sueh
 * <p> bug# 1141 Removed fixRootPanel because it doesn't do anything.
 * <p>
 * <p> Revision 3.38  2008/09/30 22:10:55  sueh
 * <p> bug# 1113 Using a private constructor in SpacedPanel.
 * <p>
 * <p> Revision 3.37  2008/05/28 02:50:30  sueh
 * <p> bug# 1111 Add a dialogType parameter to the ProcessSeries
 * <p> constructor.  DialogType must be passed to any function that constructs
 * <p> a ProcessSeries instance.
 * <p>
 * <p> Revision 3.36  2008/05/13 23:02:33  sueh
 * <p> bug# 847 Adding a right click menu for deferred 3dmods to some
 * <p> process buttons.
 * <p>
 * <p> Revision 3.35  2008/05/03 00:52:10  sueh
 * <p> bug# 847 Passing null for ProcessSeries to process funtions.
 * <p>
 * <p> Revision 3.34  2007/12/26 22:25:45  sueh
 * <p> bug# 1052 Return true when done() completes successfully.
 * <p>
 * <p> Revision 3.33  2007/11/06 20:30:25  sueh
 * <p> bug# 1047 Generalize TripvolPanel.
 * <p>
 * <p> Revision 3.32  2007/05/26 00:33:06  sueh
 * <p> bug# 994 Not automatically setting button size in SpacedPanel anymore.
 * <p> Setting button size in UI.
 * <p>
 * <p> Revision 3.31  2007/02/09 00:51:17  sueh
 * <p> bug# 962 Made TooltipFormatter a singleton and moved its use to low-level ui
 * <p> classes.
 * <p>
 * <p> Revision 3.30  2006/08/14 18:34:02  sueh
 * <p> bug# 890 Returning a success/failure boolean from getTrimvolParams.  Passing
 * <p> axisID to TrimvoPanel constructor.
 * <p>
 * <p> Revision 3.29  2006/07/04 20:41:51  sueh
 * <p> bug# 898 Don't remove action listeners unless the done dialog function
 * <p> succeeds.
 * <p>
 * <p> Revision 3.28  2006/06/30 20:02:18  sueh
 * <p> bug# 877 Calling all the done dialog functions from the dialog.done() function,
 * <p> which is called by the button action functions and saveAction() in
 * <p> ProcessDialog.  Removed the button action function overides.  Set displayed to
 * <p> false after the done dialog function is called.
 * <p>
 * <p> Revision 3.27  2006/06/21 15:54:19  sueh
 * <p> bug# 581 Passing manager and axis to ContextPopup, so that imodqtassist can
 * <p> be run.
 * <p>
 * <p> Revision 3.26  2006/01/31 21:00:03  sueh
 * <p> bug# 521 Managing trimvol and squeezevol buttons in
 * <p> ProcessResultDisplayFactory.  Made trimvol a toggle button.
 * <p>
 * <p> Revision 3.25  2006/01/26 22:05:54  sueh
 * <p> bug# 401 For MultiLineButton toggle buttons:  save the state and keep
 * <p> the buttons turned on each they are run, unless the process fails or is
 * <p> killed.
 * <p>
 * <p> Revision 3.24  2006/01/03 23:42:56  sueh
 * <p> bug# 675 Converted JCheckBox's to CheckBox
 * <p>
 * <p> Revision 3.23  2005/11/14 22:14:53  sueh
 * <p> bug# 762 Made action() protected.
 * <p>
 * <p> Revision 3.22  2005/08/11 23:57:00  sueh
 * <p> bug# 711  Change enum Run3dmodMenuOption to
 * <p> Run3dmodMenuOptions, which can turn on multiple options at once.
 * <p> This allows ImodState to combine input from the context menu and the
 * <p> pulldown menu.  Get rid of duplicate code by running the 3dmods from a
 * <p> private function called run3dmod(String, Run3dmodMenuOptions).  It can
 * <p> be called from run3dmod(Run3dmodButton, Run3dmodMenuOptions) and
 * <p> the action function.
 * <p>
 * <p> Revision 3.21  2005/08/10 20:45:24  sueh
 * <p> bug# 711 Removed MultiLineToggleButton.  Making toggling an attribute
 * <p> of MultiLineButton.
 * <p>
 * <p> Revision 3.20  2005/08/09 20:27:18  sueh
 * <p> bug# 711  Implemented Run3dmodButtonContainer:  added run3dmod().
 * <p> Changed 3dmod buttons to Run3dmodButton.
 * <p>
 * <p> Revision 3.19  2005/08/04 20:14:57  sueh
 * <p> bug# 532  Centralizing fit window functionality by placing fitting functions
 * <p> in UIHarness.  Removing packMainWindow from the manager.  Sending
 * <p> the manager to UIHarness.pack() so that packDialogs() can be called.
 * <p>
 * <p> Revision 3.18  2005/07/06 23:46:25  sueh
 * <p> bug# 619 Removed DoubleSpacedPanel and FormattedPanel.  Placed
 * <p> their functionality in SpacedPanel.  Simplified the construction of
 * <p> SpacedPanel.
 * <p>
 * <p> Revision 3.17  2005/04/21 20:45:54  sueh
 * <p> bug# 615 Pass axisID to packMainWindow so it can pack only the frame
 * <p> that requires it.
 * <p>
 * <p> Revision 3.16  2005/04/16 02:00:52  sueh
 * <p> bug# 615 Moved the adding of exit buttons to the base class.
 * <p>
 * <p> Revision 3.15  2005/03/30 21:06:40  sueh
 * <p> bug# 621 Putting a titled border around the root panel.
 * <p>
 * <p> Revision 3.14  2005/03/24 17:53:59  sueh
 * <p> bug# 621 Moved the clean up panel in to a separate dialog.
 * <p>
 * <p> Revision 3.13  2005/03/21 19:22:35  sueh
 * <p> bug# 620 Added beveled border and tooltips for Squeeze volume
 * <p>
 * <p> Revision 3.12  2005/01/21 23:46:00  sueh
 * <p> bug# 509 bug# 591  Using EtomoNumber.isNull() instead of isSet().
 * <p>
 * <p> Revision 3.11  2005/01/14 03:07:40  sueh
 * <p> bug# 511 Added DialogType to super constructor.
 * <p>
 * <p> Revision 3.10  2005/01/12 00:45:42  sueh
 * <p> bug# 579 Renamed TomogramState.getBackwordCompatible...() functions
 * <p> to ...BackwardCompatible...
 * <p>
 * <p> Revision 3.9  2005/01/10 23:56:32  sueh
 * <p> bug# 578 Modified isSqueezevolFlipped() and isTrimvolFlipped().
 * <p>
 * <p> Revision 3.8  2005/01/08 01:55:32  sueh
 * <p> bug# 578 Calling all backword compatible functions in TomogramState
 * <p> "getBackwordCompatible...".
 * <p>
 * <p> Revision 3.7  2004/12/16 02:33:05  sueh
 * <p> bug# 564 Taking whether trimvol output and squeezevol output are flipped
 * <p> or not when getting and setting Squeezevol parameters.
 * <p>
 * <p> Revision 3.6  2004/12/14 21:50:57  sueh
 * <p> bug# 557 Made separate variables for x and y reduction factors to handle
 * <p> an unflipped tomogram.
 * <p>
 * <p> Revision 3.5  2004/12/04 01:27:19  sueh
 * <p> bug# 557 Added call to imodSqueezedVolume().
 * <p>
 * <p> Revision 3.4  2004/12/02 20:41:48  sueh
 * <p> bug# 566 ContextPopup can specify an anchor in both the tomo guide and
 * <p> the join guide.  Need to specify the guide to anchor.
 * <p>
 * <p> Revision 3.3  2004/12/02 18:30:50  sueh
 * <p> bug# 557 Added the Squeeze Volume panel.  Added an action for the
 * <p> Squeeze Volume button.
 * <p>
 * <p> Revision 3.2  2004/12/01 03:47:37  sueh
 * <p> bug# 557 Added ui fields to use with squeezevol.
 * <p>
 * <p> Revision 3.1  2004/03/15 20:33:55  rickg
 * <p> button variable name changes to btn...
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.5  2003/10/30 21:05:06  rickg
 * <p> Bug# 340 Added context menu
 * <p>
 * <p> Revision 2.4  2003/04/17 23:07:20  rickg
 * <p> Added cleanup panel
 * <p>
 * <p> Revision 2.3  2003/04/16 00:15:01  rickg
 * <p> Trimvol in progress
 * <p>
 * <p> Revision 2.2  2003/04/14 23:57:34  rickg
 * <p> Trimvol management changes
 * <p>
 * <p> Revision 2.1  2003/04/10 23:43:23  rickg
 * <p> Added trimvol panel
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.5.2.1  2003/01/24 18:43:37  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.5  2002/12/19 17:45:22  rickg
 * <p> Implemented advanced dialog state processing
 * <p> including:
 * <p> default advanced state set on start up
 * <p> advanced button management now handled by
 * <p> super class
 * <p>
 * <p> Revision 1.4  2002/12/19 00:30:26  rickg
 * <p> app manager and root pane moved to super class
 * <p>
 * <p> Revision 1.3  2002/10/17 22:39:55  rickg
 * <p> Added fileset name to window title
 * <p> this reference removed applicationManager messages
 * <p>
 * <p> Revision 1.2  2002/10/07 22:31:18  rickg
 * <p> removed unused imports
 * <p> reformat after emacs trashed it
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
