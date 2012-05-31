package etomo.ui.swing;

import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;

import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.WindowConstants;

import etomo.SerialSectionsManager;
import etomo.logic.DatasetDirectory;
import etomo.storage.autodoc.AutodocTokenizer;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.DataFileType;
import etomo.type.UITestFieldType;
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
public class SerialSectionsStartupDialog {
  public static final String rcsid = "$Id:$";

  private static final String NAME = "Starting Serial Sections";

  private final JPanel pnlRoot = new JPanel();
  private final FileTextField ftfStack = new FileTextField("Stack");

  private final JDialog dialog;
  private final AxisID axisID;
  private final SerialSectionsManager manager;

  private SerialSectionsStartupDialog(final SerialSectionsManager manager, final AxisID axisID) {
    this.axisID = axisID;
    this.manager = manager;
    dialog = new JDialog(UIHarness.INSTANCE.getFrame(manager), NAME, true);
    dialog.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
    String name = Utilities.convertLabelToName(NAME);
    pnlRoot.setName(UITestFieldType.PANEL.toString() + AutodocTokenizer.SEPARATOR_CHAR
        + name);
  }

  public static SerialSectionsStartupDialog getInstance(
      final SerialSectionsManager manager, final AxisID axisID) {
    SerialSectionsStartupDialog instance = new SerialSectionsStartupDialog(manager,
        axisID);
    instance.createPanel();
    instance.setTooltips();
    instance.addListeners();
    return instance;
  }

  private void createPanel() {
    // dialog
    dialog.getContentPane().add(pnlRoot);
    // root
    pnlRoot.add(ftfStack.getContainer());
  }

  private void addListeners() {
    dialog.addWindowListener(new SerialSectionsStartupWindowListener(this));
  }
  
  public void display() {
    dialog.pack();
    dialog.setVisible(true);
  }

  private void dispose() {
    dialog.setVisible(false);
    dialog.dispose();
  }

  private boolean validate() {
    return DatasetDirectory.validateDatasetName(manager, axisID, ftfStack.getFile(),
        DataFileType.SERIAL_SECTIONS, AxisType.SINGLE_AXIS);
  }
  
  private void windowClosing() {
    dispose();
    manager.cancelStartup();
  }

  private void setTooltips() {
    ftfStack
        .setToolTipText("Stack to be processed.  Location will be used as the dataset "
            + "directory.");
  }
  
  private static final class SerialSectionsStartupWindowListener implements WindowListener {
    private final SerialSectionsStartupDialog dialog;

    private SerialSectionsStartupWindowListener(final SerialSectionsStartupDialog dialog) {
      this.dialog = dialog;
    }

    public void windowActivated(final WindowEvent event) {
    }

    public void windowClosed(final WindowEvent event) {
    }

    public void windowClosing(final WindowEvent event) {
      dialog.windowClosing();
    }

    public void windowDeactivated(final WindowEvent event) {
    }

    public void windowDeiconified(final WindowEvent event) {
    }

    public void windowIconified(final WindowEvent event) {
    }

    public void windowOpened(final WindowEvent event) {
    }
  }
}
