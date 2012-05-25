package etomo.ui.swing;

import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;

import javax.swing.BoxLayout;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import etomo.BaseManager;
import etomo.SerialSectionsManager;
import etomo.type.AxisID;
import etomo.type.DialogType;
import etomo.type.Run3dmodMenuOptions;
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
public final class SerialSectionsDialog implements ContextMenu {
  public static final String rcsid = "$Id:$";

  private static final DialogType DIALOG_TYPE = DialogType.SERIAL_SECTIONS;
  private static final String[] TAB_LABEL_ARRAY = new String[] { "Setup", "Align",
      "Initial Blend", "Make Stack" };

  private final EtomoPanel rootPanel = new EtomoPanel();
  private final EtomoPanel[] pnlTabArray = new EtomoPanel[] { new EtomoPanel(),
      new EtomoPanel(), new EtomoPanel(), new EtomoPanel() };
  private final EtomoPanel[] pnlTabBodyArray = new EtomoPanel[] { new EtomoPanel(),
      new EtomoPanel(), new EtomoPanel(), new EtomoPanel() };
  private final TabbedPane tabPane = new TabbedPane();

  private final AxisID axisID;
  private final BaseManager manager;

  private Tab curTab = Tab.SETUP;

  private SerialSectionsDialog(final BaseManager manager, final AxisID axisID) {
    System.err.println(Utilities.getDateTimeStamp() + "\nDialog: " + DIALOG_TYPE);
    this.manager = manager;
    this.axisID = axisID;
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

  private void createPanel() {
    // root panel
    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    rootPanel.setBorder(new BeveledBorder("Serial Sections").getBorder());
    rootPanel.add(tabPane);
    // tab pane
    for (int i = 0; i < Tab.SIZE; i++) {
      tabPane.add(TAB_LABEL_ARRAY[i], pnlTabArray[i]);
    }
  }

  private void init() {
    changeTab();
    updateDisplay();
  }

  public Container getContainer() {
    return rootPanel;
  }

  private void addListeners() {
    tabPane.addMouseListener(new GenericMouseAdapter(this));
    DialogActionListener actionListener = new DialogActionListener(this);
    tabPane.addChangeListener(new TabChangeListener(this));
  }

  private void updateDisplay() {
  }

  private void action(final String actionCommand,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
  }

  private void changeTab() {
    pnlTabArray[curTab.index].remove(pnlTabBodyArray[curTab.index]);
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

  private static final class DialogActionListener implements ActionListener {
    private final SerialSectionsDialog dialog;

    private DialogActionListener(final SerialSectionsDialog dialog) {
      this.dialog = dialog;
    }

    public void actionPerformed(final ActionEvent event) {
      dialog.action(event.getActionCommand(), null);
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
    private static final Tab SETUP = new Tab(0);
    private static final Tab INITIAL_BLEND = new Tab(1);
    private static final Tab ALIGN = new Tab(2);
    private static final Tab MAKE_STACK = new Tab(3);

    private static final int SIZE = 4;

    private final int index;

    private Tab(final int index) {
      this.index = index;
    }

    private static Tab getInstance(final int index) {
      if (index == SETUP.index) {
        return SETUP;
      }
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
  }
}
