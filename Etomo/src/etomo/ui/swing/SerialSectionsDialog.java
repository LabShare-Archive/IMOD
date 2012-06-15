package etomo.ui.swing;

import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;

import javax.swing.BoxLayout;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import etomo.AutoAlignmentController;
import etomo.BaseManager;
import etomo.SerialSectionsManager;
import etomo.logic.SerialSectionsStartupData;
import etomo.type.AxisID;
import etomo.type.DialogType;
import etomo.type.Run3dmodMenuOptions;
import etomo.type.SerialSectionsMetaData;
import etomo.type.ViewType;
import etomo.ui.AutoAlignmentDisplay;
import etomo.util.Utilities;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2012</p>
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
public final class SerialSectionsDialog implements ContextMenu, Run3dmodButtonContainer,
    AutoAlignmentDisplay {
  public static final String rcsid = "$Id:$";

  private static final DialogType DIALOG_TYPE = DialogType.SERIAL_SECTIONS;

  private final EtomoPanel rootPanel = new EtomoPanel();
  private final EtomoPanel[] pnlTabArray = new EtomoPanel[Tab.MAX_INDEX];
  private final EtomoPanel[] pnlTabBodyArray = new EtomoPanel[Tab.MAX_INDEX];
  private final TabbedPane tabPane = new TabbedPane();
  private final CheckBox cbVerySloppyBlend = new CheckBox("Treat as very sloppy blend");
  private final Run3dmodButton btnInitialBlend = Run3dmodButton.getDeferred3dmodInstance(
      "Run Initial Blend", this);
  private final Run3dmodButton btnOpenInitialBlend = Run3dmodButton.get3dmodInstance(
      "Open Initial Blend Result", this);
  private final MultiLineButton btnFixEdges = new MultiLineButton("Fix Edges With Midas");
  private final MultiLineButton btnOpenAlignedStack = new MultiLineButton(
      "Open Aligned Stack");
  private final AutoAlignmentPanel autoAlignmentPanel;

  private final AxisID axisID;
  private final BaseManager manager;

  private Tab curTab = null;
  private ViewType viewType = null;

  private SerialSectionsDialog(final BaseManager manager, final AxisID axisID) {
    System.err.println(Utilities.getDateTimeStamp() + "\nDialog: " + DIALOG_TYPE);
    this.manager = manager;
    this.axisID = axisID;
    autoAlignmentPanel = AutoAlignmentPanel.getSerialSectionsInstance(manager);
  }

  public static SerialSectionsDialog getInstance(final SerialSectionsManager manager,
      final AxisID axisID) {
    SerialSectionsDialog instance = new SerialSectionsDialog(manager, axisID);
    instance.createPanel();
    instance.init();
    instance.setTooltips();
    instance.addListeners();
    return instance;
  }

  public void setAutoAlignmentController(
      final AutoAlignmentController autoAlignmentController) {
    autoAlignmentPanel.setController(autoAlignmentController);
  }

  private void createPanel() {
    // init
    btnInitialBlend.setDeferred3dmodButton(btnOpenInitialBlend);
    // root panel
    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    rootPanel.setBorder(new BeveledBorder("Serial Sections").getBorder());
    rootPanel.add(tabPane);
    // tab pane
    for (int i = 0; i < Tab.MAX_INDEX; i++) {
      tabPane.add(Tab.getInstance(i).toString(), pnlTabArray[i]);
    }
    // initial blend
    int index = Tab.INITIAL_BLEND.index;
    pnlTabBodyArray[index].add(cbVerySloppyBlend);
    pnlTabBodyArray[index].add(btnInitialBlend.getComponent());
    pnlTabBodyArray[index].add(btnOpenInitialBlend.getComponent());
    pnlTabBodyArray[index].add(btnFixEdges.getComponent());
    // align
    index = Tab.ALIGN.index;
    pnlTabBodyArray[index].add(btnOpenAlignedStack.getComponent());
  }

  private void init() {
    changeTab();
    updateDisplay();
  }

  public Container getContainer() {
    return rootPanel;
  }

  public AxisID getAxisID() {
    return axisID;
  }

  public DialogType getDialogType() {
    return DialogType.SERIAL_SECTIONS;
  }

  private void addListeners() {
    tabPane.addMouseListener(new GenericMouseAdapter(this));
    tabPane.addChangeListener(new TabChangeListener(this));
    ActionListener listener = new SerialSectionsActionListener(this);
    btnInitialBlend.addActionListener(listener);
    btnOpenInitialBlend.addActionListener(listener);
    btnFixEdges.addActionListener(listener);
    btnOpenAlignedStack.addActionListener(listener);
  }

  public void setStartupData(final SerialSectionsStartupData startupData) {
    viewType = startupData.getViewType();
    if (curTab == null) {
      if (viewType == ViewType.MONTAGE) {
        curTab = Tab.INITIAL_BLEND;
      }
      else {
        curTab = Tab.ALIGN;
      }
      changeTab();
    }
    updateDisplay();
  }

  public void updateDisplay() {
    tabPane.setEnabledAt(Tab.INITIAL_BLEND.index, viewType == ViewType.MONTAGE);
  }

  public void enableMidas() {
    autoAlignmentPanel.enableMidas();
  }

  public boolean getMetaData(SerialSectionsMetaData metaData) {
    autoAlignmentPanel.getMetaData(metaData.getAutoAlignmentMetaData());
    return true;
  }

  public void action(final Run3dmodButton button,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    action(button.getActionCommand(), button.getDeferred3dmodButton(),
        run3dmodMenuOptions);
  }

  private void action(final String command, Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    if (command.equals(btnInitialBlend.getActionCommand())) {

    }
    else if (command.equals(btnOpenInitialBlend.getActionCommand())) {

    }
    else if (command.equals(btnFixEdges.getActionCommand())) {

    }
    else if (command.equals(btnOpenAlignedStack.getActionCommand())) {

    }
  }

  private void changeTab() {
    if (curTab != null) {
      pnlTabArray[curTab.index].remove(pnlTabBodyArray[curTab.index]);
    }
    curTab = Tab.getInstance(tabPane.getSelectedIndex());
    pnlTabArray[curTab.index].add(pnlTabBodyArray[curTab.index]);
    UIHarness.INSTANCE.pack(axisID, manager);
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] manPagelabel = { "Processchunks", "3dmod" };
    String[] manPage = { "processchunks.html", "3dmod.html" };
    ContextPopup contextPopup = new ContextPopup(rootPanel, mouseEvent, manPagelabel,
        manPage, true, manager, axisID);
  }

  private void setTooltips() {
  }

  private static final class SerialSectionsActionListener implements ActionListener {
    private final SerialSectionsDialog dialog;

    private SerialSectionsActionListener(final SerialSectionsDialog dialog) {
      this.dialog = dialog;
    }

    public void actionPerformed(final ActionEvent event) {
      dialog.action(event.getActionCommand(), null, null);
    }
  }

  private static final class TabChangeListener implements ChangeListener {
    private final SerialSectionsDialog dialog;

    public TabChangeListener(final SerialSectionsDialog dialog) {
      this.dialog = dialog;
    }

    public void stateChanged(final ChangeEvent changeEvent) {
      dialog.changeTab();
    }
  }

  private static final class Tab {
    private static final Tab INITIAL_BLEND = new Tab(0, "Initial Blend");
    private static final Tab ALIGN = new Tab(1, "Align");
    private static final Tab MAKE_STACK = new Tab(2, "Make Stack");

    private static final int MAX_INDEX = MAKE_STACK.index;

    private final int index;
    private final String title;

    private Tab(final int index, final String title) {
      this.index = index;
      this.title = title;
    }

    private static Tab getInstance(final int index) {
      if (index == INITIAL_BLEND.index) {
        return INITIAL_BLEND;
      }
      if (index == ALIGN.index) {
        return ALIGN;
      }
      if (index == MAKE_STACK.index) {
        return MAKE_STACK;
      }
      return null;
    }

    public String toString() {
      return title;
    }
  }
}
