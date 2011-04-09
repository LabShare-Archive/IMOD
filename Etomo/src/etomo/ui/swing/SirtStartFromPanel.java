package etomo.ui.swing;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Arrays;
import java.util.Observable;
import java.util.Observer;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JPanel;

import etomo.BaseManager;
import etomo.comscript.SirtsetupParam;
import etomo.storage.LogFile;
import etomo.storage.SirtOutputFileFilter;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstMetaData;
import etomo.type.EtomoAutodoc;
import etomo.type.EtomoNumber;
import etomo.type.FileType;
import etomo.type.MetaData;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2011</p>
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
* <p> Revision 1.1  2011/04/04 17:35:28  sueh
* <p> bug# 1416 Factored SirtPanel start from fields into SirtStartFromPanel to handle their functionality better.
* <p> </p>
*/
public final class SirtStartFromPanel extends Observable implements Observer {
  public static final String rcsid = "$Id$";

  private static final String RESUME_FROM_LAST_ITERATION_LABEL = "Resume from last iteration";

  private final JPanel pnlRoot = new JPanel();
  private final ButtonGroup bgStartingIteration = new ButtonGroup();
  private final RadioButton rbStartFromZero = new RadioButton("Start from beginning",
      bgStartingIteration);
  private final RadioButton rbResumeFromLastIteration = new RadioButton(
      RESUME_FROM_LAST_ITERATION_LABEL, bgStartingIteration);
  private final RadioButton rbResumeFromIteration = new RadioButton(
      "Go back, resume from iteration:", bgStartingIteration);
  private final ComboBox cmbResumeFromIteration = new ComboBox(
      rbResumeFromIteration.getText());

  private final BaseManager manager;
  private final AxisID axisID;

  private SirtStartFromPanel(final BaseManager manager, final AxisID axisID) {
    this.manager = manager;
    this.axisID = axisID;
  }

  static SirtStartFromPanel getInstance(final BaseManager manager, final AxisID axisID) {
    SirtStartFromPanel instance = new SirtStartFromPanel(manager, axisID);
    instance.createPanel();
    instance.setTooltips();
    instance.addListeners();
    return instance;
  }

  private void createPanel() {
    //init
    JPanel pnlStartFromZero = new JPanel();
    JPanel pnlResumeFromLastIteration = new JPanel();
    JPanel pnlResumeFromIteration = new JPanel();
    //root panel
    pnlRoot.setLayout(new BoxLayout(pnlRoot, BoxLayout.Y_AXIS));
    pnlRoot.add(pnlStartFromZero);
    pnlRoot.add(pnlResumeFromLastIteration);
    pnlRoot.add(pnlResumeFromIteration);
    //StartFromZero panel
    pnlStartFromZero.setLayout(new BoxLayout(pnlStartFromZero, BoxLayout.X_AXIS));
    pnlStartFromZero.setAlignmentX(Box.CENTER_ALIGNMENT);
    pnlStartFromZero.add(rbStartFromZero.getComponent());
    pnlStartFromZero.add(Box.createHorizontalGlue());
    //ResumeFromLastIteration panel
    pnlResumeFromLastIteration.setLayout(new BoxLayout(pnlResumeFromLastIteration,
        BoxLayout.X_AXIS));
    pnlResumeFromLastIteration.setAlignmentX(Box.CENTER_ALIGNMENT);
    pnlResumeFromLastIteration.add(rbResumeFromLastIteration.getComponent());
    pnlResumeFromLastIteration.add(Box.createHorizontalGlue());
    //Resume from iteration panel
    pnlResumeFromIteration.setLayout(new BoxLayout(pnlResumeFromIteration,
        BoxLayout.X_AXIS));
    pnlResumeFromIteration.add(rbResumeFromIteration.getComponent());
    pnlResumeFromIteration.add(cmbResumeFromIteration);
    //defaults
    rbStartFromZero.setSelected(true);
    loadResumeFrom();
  }

  Component getRoot() {
    return pnlRoot;
  }

  private void addListeners() {
    ActionListener listener = new SirtStartFromActionListener(this);
    rbStartFromZero.addActionListener(listener);
    rbResumeFromLastIteration.addActionListener(listener);
    rbResumeFromIteration.addActionListener(listener);
  }

  public void addObserver(Observer observer) {
    super.addObserver(observer);
    //New observer needs to be informed if there is a nondefault state.
    if (isResume()) {
      setChanged();
      notifyObservers(true);
    }
  }

  public void update(final Observable observable, final Object arg) {
    if (observable instanceof AbstractTiltPanel) {
      //This observable signals when resume is not a legal option.
      updateDisplay(false);
    }
  }

  void msgSirtSucceeded() {
    loadResumeFrom();
  }

  private void updateDisplay(final boolean resumeEnabled) {
    rbResumeFromLastIteration.setEnabled(resumeEnabled);
    rbResumeFromIteration.setEnabled(resumeEnabled);
    cmbResumeFromIteration
        .setEnabled(resumeEnabled && rbResumeFromIteration.isSelected());
    //This may change the result of isResume().
    setChanged();
    notifyObservers(isResume());
  }

  Boolean isResumeEnabled() {
    return rbResumeFromLastIteration.isEnabled();
  }

  Boolean isResume() {
    return (rbResumeFromLastIteration.isEnabled() && rbResumeFromLastIteration
        .isSelected())
        || (rbResumeFromIteration.isEnabled() && rbResumeFromIteration.isSelected());
  }

  void setParameters(final ConstMetaData metaData) {
    updateDisplay(metaData.isGenResumeEnabled(axisID));
  }

  void setParameters(final SirtsetupParam param) {
    if (param.isStartFromZero()) {
      rbStartFromZero.setSelected(true);
    }
    else if (!param.isResumeFromIterationNull()) {
      rbResumeFromIteration.setSelected(true);
    }
    else {
      rbResumeFromLastIteration.setSelected(true);
    }
    updateDisplay(isResumeEnabled());
    //This happens when the dialog is created, so the observer only needs to be informed
    //if there is a nondefault state.
    if (isResume()) {
      setChanged();
      notifyObservers(true);
    }
  }

  void getParameters(final MetaData metaData) {
    metaData.setGenResumeEnabled(axisID, rbResumeFromIteration.isEnabled());
  }

  public boolean getParameters(final SirtsetupParam param) {
    if (rbStartFromZero.isSelected()) {
      param.setStartFromZero(true);
      param.resetResumeFromIteration();
    }
    else if (rbResumeFromLastIteration.isEnabled()
        && rbResumeFromLastIteration.isSelected()) {
      param.setStartFromZero(false);
      param.resetResumeFromIteration();
    }
    else if (rbResumeFromIteration.isEnabled() && rbResumeFromIteration.isSelected()) {
      param.setStartFromZero(false);
      param.setResumeFromIteration((ConstEtomoNumber) cmbResumeFromIteration
          .getSelectedItem());
    }
    else {
      UIHarness.INSTANCE.openMessageDialog(manager,
          "Please select an enabled starting option.", "Entry Error", axisID);
      return false;
    }
    return true;
  }

  /**
   * Load all of the .srecdd files in the current directory (any number of digits).  Save
   * the digits in the ResumeFromIteration pulldown list.
   */
  private void loadResumeFrom() {
    //Clear pulldown list.
    cmbResumeFromIteration.removeAllItems();
    //Get file names.
    String[] fileNameList = new File(manager.getPropertyUserDir())
        .list(new SirtOutputFileFilter(manager, axisID, false));
    //Extract iteration numbers, sort them, and add them to the pulldown list.
    int[] fileNumberList = null;
    String templateExt = FileType.SIRT_OUTPUT_TEMPLATE.getExtension(manager);
    if (fileNameList != null && fileNameList.length > 0) {
      fileNumberList = new int[fileNameList.length];
      for (int i = 0; i < fileNumberList.length; i++) {
        EtomoNumber fileNumber = new EtomoNumber();
        fileNumber.set(fileNameList[i]
            .substring(fileNameList[i].lastIndexOf(templateExt)).substring(
                templateExt.length()));
        //SirtOutputFileFilter only accepts files the end in a valid integer, so assume
        //the number is valid.
        fileNumberList[i] = fileNumber.getInt();
      }
      Arrays.sort(fileNumberList);
      //Add sorted numbers to the pulldown list.
      for (int i = fileNumberList.length - 1; i >= 0; i--) {
        EtomoNumber fileNumber = new EtomoNumber();
        fileNumber.set(fileNumberList[i]);
        if (i == fileNumberList.length - 1) {
          rbResumeFromLastIteration.setText(RESUME_FROM_LAST_ITERATION_LABEL + ": "
              + fileNumber);
        }
        cmbResumeFromIteration.addItem(fileNumber);
      }
      if (fileNumberList.length > 1) {
        cmbResumeFromIteration.setSelectedIndex(1);
      }
    }
    else {
      rbResumeFromLastIteration.setText(RESUME_FROM_LAST_ITERATION_LABEL);
    }
    updateDisplay(fileNumberList != null && fileNumberList.length != 0);
  }

  private void action() {
    updateDisplay(isResumeEnabled());
    //The action may have changed the result of isResume().
    setChanged();
    notifyObservers(isResume());
  }

  void setTooltips() {
    ReadOnlyAutodoc autodoc = null;
    try {
      autodoc = AutodocFactory.getInstance(manager, AutodocFactory.SIRTSETUP, axisID);
    }
    catch (FileNotFoundException except) {
      except.printStackTrace();
    }
    catch (IOException except) {
      except.printStackTrace();
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
    }
    rbStartFromZero.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        SirtsetupParam.START_FROM_ZERO_KEY));
    rbResumeFromLastIteration
        .setToolTipText("Iterate from the last existing reconstruction.");
    String tooltip = EtomoAutodoc.getTooltip(autodoc,
        SirtsetupParam.RESUME_FROM_ITERATION_KEY);
    rbResumeFromIteration.setToolTipText(tooltip);
    cmbResumeFromIteration.setToolTipText(tooltip);
  }

  private static final class SirtStartFromActionListener implements ActionListener {
    private final SirtStartFromPanel adaptee;

    private SirtStartFromActionListener(final SirtStartFromPanel adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.action();
    }
  }
}
