package etomo.ui.swing;

import java.awt.Component;
import java.awt.Container;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Vector;

import javax.swing.BoxLayout;
import javax.swing.JPanel;

import etomo.BaseManager;
import etomo.comscript.TrimvolParam;
import etomo.comscript.XYParam;
import etomo.process.ImodProcess;
import etomo.type.AxisID;
import etomo.type.ConstMetaData;
import etomo.type.MetaData;
import etomo.type.ParallelMetaData;
import etomo.ui.FieldType;
import etomo.ui.FieldValidationFailedException;

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
 */
public final class RubberbandPanel {
  public static final String rcsid = "$Id$";

  private final BaseManager manager;

  private final SpacedPanel pnlRubberband = SpacedPanel.getInstance();
  private final JPanel pnlRange = new JPanel();
  private final LabeledTextField ltfXMin = new LabeledTextField(FieldType.INTEGER,
      "X min: ");
  private final LabeledTextField ltfXMax = new LabeledTextField(FieldType.INTEGER,
      "X max: ");
  private final LabeledTextField ltfYMin = new LabeledTextField(FieldType.INTEGER,
      "Y min: ");
  private final LabeledTextField ltfYMax = new LabeledTextField(FieldType.INTEGER,
      "Y max: ");
  private final LabeledTextField ltfZMin = new LabeledTextField(FieldType.INTEGER,
      "Z min: ");
  private final LabeledTextField ltfZMax = new LabeledTextField(FieldType.INTEGER,
      "Z max: ");
  private final MultiLineButton btnRubberband;
  private final String imodKey;
  private final String xMinTooltip;
  private final String xMaxTooltip;
  private final String yMinTooltip;
  private final String yMaxTooltip;
  private final String zMinTooltip;
  private final String zMaxTooltip;
  private final Run3dmodButton btnImod;
  private final RubberbandContainer container;// optional,
  private final boolean placeButtons;
  private final boolean lockPanel;

  private RubberbandPanel(final BaseManager manager, final RubberbandContainer container,
      final String imodKey, final String borderLabel, final String buttonLabel,
      final String xMinTooltip, final String xMaxTooltip, final String yMinTooltip,
      final String yMaxTooltip, final String zMinTooltip, final String zMaxTooltip,
      final Run3dmodButton btnImod, final boolean placeButtons, final boolean lockPanel) {
    this.lockPanel = lockPanel;
    this.container = container;
    this.imodKey = imodKey;
    this.xMinTooltip = xMinTooltip;
    this.xMaxTooltip = xMaxTooltip;
    this.yMinTooltip = yMinTooltip;
    this.yMaxTooltip = yMaxTooltip;
    this.zMinTooltip = zMinTooltip;
    this.zMaxTooltip = zMaxTooltip;
    this.placeButtons = placeButtons;
    this.btnImod = btnImod;
    this.manager = manager;
    pnlRubberband.setBorder(new EtchedBorder(borderLabel).getBorder());
    btnRubberband = new MultiLineButton(buttonLabel);
    btnRubberband.setSize();
    pnlRubberband.setBoxLayout(BoxLayout.Y_AXIS);
    SpacedPanel pnlButtons = null;
    if (btnImod != null) {
      btnImod.setSize();
      if (placeButtons) {
        pnlButtons = SpacedPanel.getInstance();
        pnlButtons.setBoxLayout(BoxLayout.X_AXIS);
        pnlButtons.addHorizontalGlue();
        pnlButtons.add(btnImod);
        pnlButtons.addHorizontalGlue();
        pnlButtons.add(btnRubberband);
        pnlButtons.addHorizontalGlue();
      }
      pnlRange.setLayout(new GridLayout(3, 2, 5, 5));
    }
    else {
      pnlRange.setLayout(new GridLayout(2, 2, 5, 5));
    }
    pnlRange.add(ltfXMin.getContainer());
    pnlRange.add(ltfXMax.getContainer());
    pnlRange.add(ltfYMin.getContainer());
    pnlRange.add(ltfYMax.getContainer());
    if (btnImod != null) {
      pnlRange.add(ltfZMin.getContainer());
      pnlRange.add(ltfZMax.getContainer());
      if (placeButtons) {
        pnlRubberband.add(pnlButtons.getContainer());
      }
    }
    pnlRubberband.add(pnlRange);
    if (btnImod == null) {
      btnRubberband.setAlignmentX(Component.CENTER_ALIGNMENT);
      if (placeButtons) {
        pnlRubberband.addRigidArea();
        pnlRubberband.add(btnRubberband);
      }
    }
    setToolTipText();
  }

  static RubberbandPanel getInstance(BaseManager manager, RubberbandContainer container,
      String imodKey, String borderLabel, String buttonLabel, String xMinTooltip,
      String xMaxTooltip, String yMinTooltip, String yMaxTooltip) {
    RubberbandPanel instance = new RubberbandPanel(manager, container, imodKey,
        borderLabel, buttonLabel, xMinTooltip, xMaxTooltip, yMinTooltip, yMaxTooltip, "",
        "", null, true, false);
    instance.addListeners();
    return instance;
  }

  static RubberbandPanel getInstance(BaseManager manager, String imodKey,
      String borderLabel, String buttonLabel, String xMinTooltip, String xMaxTooltip,
      String yMinTooltip, String yMaxTooltip, String zMinTooltip, String zMaxTooltip,
      Run3dmodButton btnIdmod) {
    RubberbandPanel instance = new RubberbandPanel(manager, null, imodKey, borderLabel,
        buttonLabel, xMinTooltip, xMaxTooltip, yMinTooltip, yMaxTooltip, zMinTooltip,
        zMaxTooltip, btnIdmod, true, false);
    instance.addListeners();
    return instance;
  }

  /**
   * Does not use the 3dmod button and does not add the rubberband button to the
   * display.  The rubberband button must be placed in the container to be
   * available.
   * @param manager
   * @param container
   * @param imodKey
   * @param borderLabel
   * @param buttonLabel
   * @param xMinTooltip
   * @param xMaxTooltip
   * @param yMinTooltip
   * @param yMaxTooltip
   * @return
   */
  static RubberbandPanel getNoButtonInstance(final BaseManager manager,
      final RubberbandContainer container, final String imodKey,
      final String borderLabel, final String buttonLabel, final String xMinTooltip,
      final String xMaxTooltip, final String yMinTooltip, final String yMaxTooltip,
      final boolean lockPanel) {
    RubberbandPanel instance = new RubberbandPanel(manager, container, imodKey,
        borderLabel, buttonLabel, xMinTooltip, xMaxTooltip, yMinTooltip, yMaxTooltip, "",
        "", null, false, lockPanel);
    instance.addListeners();
    return instance;
  }

  private void addListeners() {
    btnRubberband.addActionListener(new RubberbandActionListener(this));
  }

  Component getRubberbandButtonComponent() {
    return btnRubberband.getComponent();
  }

  Component getComponent() {
    return pnlRubberband.getContainer();
  }

  Container getContainer() {
    return pnlRubberband.getContainer();
  }

  void buttonAction(ActionEvent event) {
    String command = event.getActionCommand();
    if (command == btnRubberband.getActionCommand()) {
      Vector coordinates = manager.imodGetRubberbandCoordinates(imodKey, AxisID.ONLY);
      setMinAndMax(coordinates);
    }
  }

  private void setMinAndMax(Vector coordinates) {
    if (coordinates == null) {
      return;
    }
    int size = coordinates.size();
    if (size == 0) {
      return;
    }
    int index = 0;
    while (index < size) {
      if (ImodProcess.RUBBERBAND_RESULTS_STRING.equals((String) coordinates.get(index++))) {
        ltfXMin.setText((String) coordinates.get(index++));
        if (index >= size) {
          return;
        }
        ltfYMin.setText((String) coordinates.get(index++));
        if (index >= size) {
          return;
        }
        ltfXMax.setText((String) coordinates.get(index++));
        if (index >= size) {
          return;
        }
        ltfYMax.setText((String) coordinates.get(index++));
        if (index >= size) {
          return;
        }
        if (btnImod == null && container == null) {
          return;
        }
        if (btnImod != null) {
          ltfZMin.setText((String) coordinates.get(index));
        }
        if (container != null) {
          container.setZMin((String) coordinates.get(index));
        }
        index++;
        if (index >= size) {
          return;
        }
        if (btnImod != null) {
          ltfZMax.setText((String) coordinates.get(index));
        }
        if (container != null) {
          container.setZMax((String) coordinates.get(index));
        }
        return;
      }
    }
  }

  void setEnabled(boolean enable) {
    ltfXMin.setEnabled(enable);
    ltfXMax.setEnabled(enable);
    ltfYMin.setEnabled(enable);
    ltfYMax.setEnabled(enable);
    btnRubberband.setEnabled(enable);
  }

  void setVisible(boolean visible) {
    pnlRubberband.setVisible(visible);
  }

  public void getParameters(MetaData metaData) {
    if (lockPanel) {
      return;
    }
    metaData.setPostTrimvolScaleXMin(ltfXMin.getText());
    metaData.setPostTrimvolScaleXMax(ltfXMax.getText());
    metaData.setPostTrimvolScaleYMin(ltfYMin.getText());
    metaData.setPostTrimvolScaleYMax(ltfYMax.getText());
    if (btnImod != null) {
      metaData.setPostTrimvolSectionScaleMin(ltfZMin.getText());
      metaData.setPostTrimvolSectionScaleMax(ltfZMax.getText());
    }
  }

  public boolean getParameters(TrimvolParam trimvolParam, final boolean doValidation) {
    if (lockPanel) {
      return true;
    }
    try {
      trimvolParam.setXMin(ltfXMin.getText(doValidation));
      trimvolParam.setXMax(ltfXMax.getText(doValidation));
      trimvolParam.setYMin(ltfYMin.getText(doValidation));
      trimvolParam.setYMax(ltfYMax.getText(doValidation));
      if (btnImod != null) {
        trimvolParam.setZMin(ltfZMin.getText(doValidation));
        trimvolParam.setZMax(ltfZMax.getText(doValidation));
      }
      return true;
    }
    catch (FieldValidationFailedException e) {
      return false;
    }
  }

  public boolean getScaleParameters(TrimvolParam trimvolParam, final boolean doValidation) {
    if (lockPanel) {
      return true;
    }
    try {
      XYParam xyParam = trimvolParam.getScaleXYParam();
      xyParam.setXMin(ltfXMin.getText(doValidation));
      xyParam.setXMax(ltfXMax.getText(doValidation));
      xyParam.setYMin(ltfYMin.getText(doValidation));
      xyParam.setYMax(ltfYMax.getText(doValidation));
      if (btnImod != null) {
        trimvolParam.setSectionScaleMin(ltfZMin.getText(doValidation));
        trimvolParam.setSectionScaleMax(ltfZMax.getText(doValidation));
      }
      return true;
    }
    catch (FieldValidationFailedException e) {
      return false;
    }
  }

  public void getParameters(ParallelMetaData metaData) {
    if (lockPanel) {
      return;
    }
    metaData.setXMin(ltfXMin.getText());
    metaData.setXMax(ltfXMax.getText());
    metaData.setYMin(ltfYMin.getText());
    metaData.setYMax(ltfYMax.getText());
    if (btnImod != null) {
      metaData.setZMin(ltfZMin.getText());
      metaData.setZMax(ltfZMax.getText());
    }
  }

  void getParametersForTrimvol(final ParallelMetaData metaData) {
    if (lockPanel) {
      return;
    }
    if (btnImod != null) {
      metaData.setNewStyleZ(ltfZMin.getText(), ltfZMax.getText());
    }
  }

  public void setParameters(TrimvolParam param) {
    if (lockPanel) {
      return;
    }
    ltfXMin.setText(param.getXMin());
    ltfXMax.setText(param.getXMax());
    ltfYMin.setText(param.getYMin());
    ltfYMax.setText(param.getYMax());
    if (btnImod != null) {
      ltfZMin.setText(param.getZMin());
      ltfZMax.setText(param.getZMax());
    }
  }

  public void setScaleParameters(TrimvolParam trimvolParam) {
    if (lockPanel) {
      return;
    }
    XYParam xyParam = trimvolParam.getScaleXYParam();
    ltfXMin.setText(xyParam.getXMin());
    ltfXMax.setText(xyParam.getXMax());
    ltfYMin.setText(xyParam.getYMin());
    ltfYMax.setText(xyParam.getYMax());
    if (btnImod != null) {
      ltfZMin.setText(trimvolParam.getSectionScaleMin());
      ltfZMax.setText(trimvolParam.getSectionScaleMax());
    }
  }

  public void setParameters(ConstMetaData metaData) {
    if (lockPanel) {
      return;
    }
    ltfXMin.setText(metaData.getPostTrimvolScaleXMin());
    ltfXMax.setText(metaData.getPostTrimvolScaleXMax());
    ltfYMin.setText(metaData.getPostTrimvolScaleYMin());
    ltfYMax.setText(metaData.getPostTrimvolScaleYMax());
    if (btnImod != null) {
      ltfZMin.setText(metaData.getPostTrimvolSectionScaleMin());
      ltfZMax.setText(metaData.getPostTrimvolSectionScaleMax());
    }
  }

  public void setParameters(ParallelMetaData metaData) {
    if (lockPanel) {
      return;
    }
    ltfXMin.setText(metaData.getXMin());
    ltfXMax.setText(metaData.getXMax());
    ltfYMin.setText(metaData.getYMin());
    ltfYMax.setText(metaData.getYMax());
    if (btnImod != null) {
      ltfZMin.setText(metaData.getZMin());
      ltfZMax.setText(metaData.getZMax());
    }
  }

  private void setToolTipText() {
    ltfXMin.setToolTipText(xMinTooltip);
    ltfXMax.setToolTipText(xMaxTooltip);
    ltfYMin.setToolTipText(yMinTooltip);
    ltfYMax.setToolTipText(yMaxTooltip);
    ltfZMin.setToolTipText(zMinTooltip);
    ltfZMax.setToolTipText(zMaxTooltip);
    btnRubberband.setToolTipText("After opening the volume in 3dmod, press shift-B in "
        + "the ZaP window.  Create a rubberband around the contrast "
        + "range.  Then press this button to retrieve the X"
        + (btnImod == null ? " and Y" : ", Y, and Z") + " coordinates.");
  }

  private static final class RubberbandActionListener implements ActionListener {
    RubberbandPanel adaptee;

    RubberbandActionListener(RubberbandPanel panel) {
      adaptee = panel;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.buttonAction(event);
    }
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.2  2011/02/22 19:08:03  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.10  2009/06/05 02:15:09  sueh
 * <p> bug# 1219 Improved the layout of pnlRange.
 * <p>
 * <p> Revision 1.9  2008/09/30 22:20:14  sueh
 * <p> bug# 1113 Using a private constructor in SpacedPanel.
 * <p>
 * <p> Revision 1.8  2008/07/15 21:22:33  sueh
 * <p> bug# 1127 Added placeButtons.  Don't place the buttons if placeButtons
 * <p> is false.
 * <p>
 * <p> Revision 1.7  2008/02/28 21:18:53  sueh
 * <p> bug# 1085 Changed setXYMinAndMax to setMinAndMax.  Added setting Z
 * <p> in both the current panel (if Z is displayed) and the containing panel (if the
 * <p> container is available).
 * <p>
 * <p> Revision 1.6  2007/11/09 17:47:12  sueh
 * <p> bug# 1047 Fixed problems with tooltips.
 * <p>
 * <p> Revision 1.5  2007/11/06 20:30:53  sueh
 * <p> bug# 1047 Generalize and make more flexible.
 * <p>
 * <p> Revision 1.4  2007/02/09 00:52:25  sueh
 * <p> bug# 962 Made TooltipFormatter a singleton and moved its use to low-level ui
 * <p> classes.
 * <p>
 * <p> Revision 1.3  2006/08/16 22:41:46  sueh
 * <p> bug# 912 Make panel less flexible
 * <p>
 * <p> Revision 1.2  2006/08/16 18:51:56  sueh
 * <p> bug# 912 Making panel generic so it can be used for all places where a
 * <p> rubberband is used.
 * <p>
 * <p> Revision 1.1  2006/06/28 23:29:36  sueh
 * <p> bug# 881 Panel to get X and Y scaling range using a 3dmod rubberband.
 * <p> </p>
 */
