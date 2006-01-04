package etomo.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Vector;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JPanel;

import etomo.ApplicationManager;
import etomo.comscript.TrimvolParam;
import etomo.process.ImodManager;
import etomo.process.ImodProcess;
import etomo.type.AxisID;
import etomo.type.Run3dmodMenuOptions;
/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2002</p>
 * 
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 3.10  2005/11/14 22:35:38  sueh
 * <p> bug# 762 Made scaleAction().
 * <p>
 * <p> Revision 3.9  2005/08/12 00:01:38  sueh
 * <p> bug# 711  Change enum Run3dmodMenuOption to
 * <p> Run3dmodMenuOptions, which can turn on multiple options at once.
 * <p> This allows ImodState to combine input from the context menu and the
 * <p> pulldown menu.  Prevent context menu from popping up when button is
 * <p> disabled.  Get rid of duplicate code by running the 3dmods from a private
 * <p> function called run3dmod(String, Run3dmodMenuOptions).  It can be
 * <p> called from run3dmod(Run3dmodButton, Run3dmodMenuOptions) and the
 * <p> action function.
 * <p>
 * <p> Revision 3.8  2005/08/10 20:48:56  sueh
 * <p> bug# 711 Moved button sizing to MultiLineButton.  SetSize() sets the
 * <p> standard button size.
 * <p>
 * <p> Revision 3.7  2005/08/09 21:11:46  sueh
 * <p> bug# 711  Implemented Run3dmodButtonContainer:  added run3dmod().
 * <p> Changed 3dmod buttons to Run3dmodButton.  No longer inheriting
 * <p> MultiLineButton from JButton.
 * <p>
 * <p> Revision 3.6  2005/04/25 21:41:51  sueh
 * <p> bug# 615 Passing the axis where a command originates to the message
 * <p> functions so that the message will be popped up in the correct window.
 * <p> This requires adding AxisID to many objects.
 * <p>
 * <p> Revision 3.5  2005/03/24 17:55:44  sueh
 * <p> bug# 621 Set a preferred text width for fields that where too small.
 * <p>
 * <p> Revision 3.4  2004/05/13 20:13:51  sueh
 * <p> bug# 33 change setXYMinAndMax() so it can ignore non rubberband data
 * <p>
 * <p> Revision 3.3  2004/05/07 19:53:23  sueh
 * <p> bug# 33 getting coordinates info in the right order, getting only
 * <p> the correct kind of data
 * <p>
 * <p> Revision 3.2  2004/05/06 20:25:16  sueh
 * <p> bug# 33 added getCoordinates button, moved fullvol button to the top
 * <p> of the dialog, added setXYMinAndMax() to set field values
 * <p>
 * <p> Revision 3.1  2004/01/30 22:45:34  sueh
 * <p> bug# 356 Changing buttons with html labels to
 * <p> MultiLineButton and MultiLineToggleButton
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 1.12  2003/10/29 20:49:11  rickg
 * <p> Bug# 308 Tooltips
 * <p>
 * <p> Revision 1.11  2003/10/20 23:25:41  rickg
 * <p> Bug# 253 Added convert to bytes checkbox
 * <p>
 * <p> Revision 1.10  2003/10/16 17:05:10  rickg
 * <p> Bug# 305 Label changes, backup file filter
 * <p>
 * <p> Revision 1.9  2003/09/08 05:48:16  rickg
 * <p> Method name change for opening the complete volume
 * <p>
 * <p> Revision 1.8  2003/04/28 23:25:25  rickg
 * <p> Changed visible imod references to 3dmod
 * <p>
 * <p> Revision 1.7  2003/04/17 23:08:38  rickg
 * <p> Initial revision
 * <p>
 * <p> Revision 1.6  2003/04/16 22:18:17  rickg
 * <p> Trimvol in progress
 * <p>
 * <p> Revision 1.5  2003/04/16 00:14:40  rickg
 * <p> Trimvol in progress
 * <p>
 * <p> Revision 1.4  2003/04/14 23:57:44  rickg
 * <p> In progress
 * <p>
 * <p> Revision 1.3  2003/04/14 04:31:31  rickg
 * <p> In progres
 * <p>
 * <p> Revision 1.2  2003/04/10 23:42:51  rickg
 * <p> In progress
 * <p>
 * <p> Revision 1.1  2003/04/09 23:37:20  rickg
 * <p> In progress
 * <p> </p>
 */

public class TrimvolPanel implements Run3dmodButtonContainer {
  public static final String rcsid =
    "$Id$";

  private ApplicationManager applicationManager;

  private JPanel pnlTrimvol = new JPanel();

  private JPanel pnlRange = new JPanel();
  private LabeledTextField ltfXMin = new LabeledTextField("X min: ");
  private LabeledTextField ltfXMax = new LabeledTextField(" X max: ");
  private LabeledTextField ltfYMin = new LabeledTextField("Y min: ");
  private LabeledTextField ltfYMax = new LabeledTextField(" Y max: ");
  private LabeledTextField ltfZMin = new LabeledTextField("Z min: ");
  private LabeledTextField ltfZMax = new LabeledTextField(" Z max: ");

  private JPanel pnlScale = new JPanel();
  private JPanel pnlScaleFixed = new JPanel();
  private CheckBox cbConvertToBytes = new CheckBox("Convert to bytes");
  private RadioButton rbScaleFixed =
    new RadioButton("Scale to match contrast  ");
  private LabeledTextField ltfFixedScaleMin = new LabeledTextField("black: ");
  private LabeledTextField ltfFixedScaleMax = new LabeledTextField(" white: ");

  private RadioButton rbScaleSection =
    new RadioButton("Find scaling from sections  ");
  private JPanel pnlScaleSection = new JPanel();
  private LabeledTextField ltfSectionScaleMin = new LabeledTextField("Z min: ");
  private LabeledTextField ltfSectionScaleMax =
    new LabeledTextField(" Z max: ");

  private CheckBox cbSwapYZ = new CheckBox("Swap Y and Z dimensions");

  private JPanel pnlButton = new JPanel();
  private Run3dmodButton btnImodFull = new Run3dmodButton("<html><b>3dmod Full Volume</b>", this);
  private MultiLineButton btnTrimvol = new MultiLineButton("<html><b>Trim Volume</b>");
  private Run3dmodButton btnImodTrim =
    new Run3dmodButton("<html><b>3dmod Trimmed Volume</b>", this);
  private MultiLineButton btnGetCoordinates =
    new MultiLineButton("Get XY Volume Range From 3dmod");
  private JPanel pnlImodFull = new JPanel();

  /**
   * Default constructor
   */
  public TrimvolPanel(ApplicationManager appMgr) {

    applicationManager = appMgr;

    //  Set the button sizes
    btnImodFull.setSize();
    btnTrimvol.setSize();
    btnImodTrim.setSize();
    btnGetCoordinates.setSize();

    //  Layout the range panel
    pnlRange.setLayout(new GridLayout(3, 2));
    pnlRange.setBorder(new EtchedBorder("Volume Range").getBorder());

    pnlRange.add(ltfXMin.getContainer());
    pnlRange.add(ltfXMax.getContainer());
    pnlRange.add(ltfYMin.getContainer());
    pnlRange.add(ltfYMax.getContainer());
    pnlRange.add(ltfZMin.getContainer());
    pnlRange.add(ltfZMax.getContainer());

    //  Layout the scale panel
    pnlScaleFixed.setLayout(new BoxLayout(pnlScaleFixed, BoxLayout.X_AXIS));

    pnlScaleFixed.add(rbScaleFixed);
    pnlScaleFixed.add(ltfFixedScaleMin.getContainer());
    pnlScaleFixed.add(ltfFixedScaleMax.getContainer());

    pnlScaleSection.setLayout(new BoxLayout(pnlScaleSection, BoxLayout.X_AXIS));
    pnlScaleSection.add(rbScaleSection);
    ltfSectionScaleMin.setTextPreferredWidth(FixedDim.fourDigitWidth);
    ltfSectionScaleMax.setTextPreferredWidth(FixedDim.fourDigitWidth);
    pnlScaleSection.add(ltfSectionScaleMin.getContainer());
    pnlScaleSection.add(ltfSectionScaleMax.getContainer());

    ButtonGroup bgScale = new ButtonGroup();
    bgScale.add(rbScaleFixed);
    bgScale.add(rbScaleSection);

    pnlScale.setLayout(new BoxLayout(pnlScale, BoxLayout.Y_AXIS));
    pnlScale.setBorder(new EtchedBorder("Scaling").getBorder());

    cbConvertToBytes.setAlignmentX(Component.RIGHT_ALIGNMENT);
    pnlScale.add(cbConvertToBytes);
    pnlScale.add(pnlScaleFixed);
    pnlScale.add(pnlScaleSection);

    pnlButton.setLayout(new BoxLayout(pnlButton, BoxLayout.X_AXIS));
    pnlButton.add(Box.createHorizontalGlue());
    pnlButton.add(btnTrimvol.getComponent());
    pnlButton.add(Box.createHorizontalGlue());
    pnlButton.add(btnImodTrim.getComponent());
    pnlButton.add(Box.createHorizontalGlue());

    pnlTrimvol.setLayout(new BoxLayout(pnlTrimvol, BoxLayout.Y_AXIS));
    pnlTrimvol.setBorder(new BeveledBorder("Volume Trimming").getBorder());

    pnlImodFull.setLayout(new BoxLayout(pnlImodFull, BoxLayout.X_AXIS));
    pnlImodFull.add(Box.createHorizontalGlue());
    pnlImodFull.add(btnImodFull.getComponent());
    pnlImodFull.add(Box.createHorizontalGlue());
    pnlImodFull.add(btnGetCoordinates.getComponent());
    pnlImodFull.add(Box.createHorizontalGlue());
    pnlTrimvol.add(pnlImodFull);
    pnlTrimvol.add(pnlRange);
    pnlTrimvol.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlTrimvol.add(pnlScale);
    pnlTrimvol.add(Box.createRigidArea(FixedDim.x0_y10));
    cbSwapYZ.setAlignmentX(Component.RIGHT_ALIGNMENT);
    pnlTrimvol.add(cbSwapYZ);
    pnlTrimvol.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlTrimvol.add(pnlButton);
    pnlTrimvol.add(Box.createRigidArea(FixedDim.x0_y10));

    ScalingListener ScalingListener = new ScalingListener(this);
    rbScaleFixed.addActionListener(ScalingListener);
    rbScaleSection.addActionListener(ScalingListener);
    cbConvertToBytes.addActionListener(ScalingListener);

    ButtonListener buttonActonListener = new ButtonListener(this);
    btnImodFull.addActionListener(buttonActonListener);
    btnTrimvol.addActionListener(buttonActonListener);
    btnImodTrim.addActionListener(buttonActonListener);
    btnGetCoordinates.addActionListener(buttonActonListener);

    setToolTipText();
  }

  /**
   * Return the container of the panel
   * @return
   */
  public Container getContainer() {
    return pnlTrimvol;
  }

  /**
   * Set the panel values with the specified parameters
   * @param trimvolParam
   */
  public void setParameters(TrimvolParam trimvolParam) {
    ltfXMin.setText(trimvolParam.getXMin());
    ltfXMax.setText(trimvolParam.getXMax());
    //  Y and Z  are swapped to present the user with Z as the depth domain
    ltfYMin.setText(trimvolParam.getZMin());
    ltfYMax.setText(trimvolParam.getZMax());
    ltfZMin.setText(trimvolParam.getYMin());
    ltfZMax.setText(trimvolParam.getYMax());
    cbSwapYZ.setSelected(trimvolParam.isSwapYZ());

    cbConvertToBytes.setSelected(trimvolParam.isConvertToBytes());
    if (trimvolParam.isFixedScaling()) {
      ltfFixedScaleMin.setText(trimvolParam.getFixedScaleMin());
      ltfFixedScaleMax.setText(trimvolParam.getFixedScaleMax());
      rbScaleFixed.setSelected(true);
    }
    else {
      ltfSectionScaleMin.setText(trimvolParam.getSectionScaleMin());
      ltfSectionScaleMax.setText(trimvolParam.getSectionScaleMax());
      rbScaleSection.setSelected(true);
    }
    setScaleState();
  }

  /**
   * Get the parameter values from the panel 
   * @param trimvolParam
   */
  public void getParameters(TrimvolParam trimvolParam) {
    trimvolParam.setXMin(Integer.parseInt(ltfXMin.getText()));
    trimvolParam.setXMax(Integer.parseInt(ltfXMax.getText()));
    //  Y and Z  are swapped to present the user with Z as the depth domain
    trimvolParam.setYMin(Integer.parseInt(ltfZMin.getText()));
    trimvolParam.setYMax(Integer.parseInt(ltfZMax.getText()));
    trimvolParam.setZMin(Integer.parseInt(ltfYMin.getText()));
    trimvolParam.setZMax(Integer.parseInt(ltfYMax.getText()));
    trimvolParam.setSwapYZ(cbSwapYZ.isSelected());

    trimvolParam.setConvertToBytes(cbConvertToBytes.isSelected());
    if (rbScaleFixed.isSelected()) {
      trimvolParam.setFixedScaling(true);
      trimvolParam.setFixedScaleMin(
        Integer.parseInt(ltfFixedScaleMin.getText()));
      trimvolParam.setFixedScaleMax(
        Integer.parseInt(ltfFixedScaleMax.getText()));
    }
    else {
      trimvolParam.setFixedScaling(false);
      trimvolParam.setSectionScaleMin(
        Integer.parseInt(ltfSectionScaleMin.getText()));
      trimvolParam.setSectionScaleMax(
        Integer.parseInt(ltfSectionScaleMax.getText()));
    }
  }
  
  public void setXYMinAndMax(Vector coordinates) {
    if (coordinates == null) {
      return;
    }
    int size = coordinates.size();
    if (size == 0) {
      return;
    }
    int index = 0;
    while (index < size) {
      if (ImodProcess
        .RUBBERBAND_RESULTS_STRING
        .equals((String) coordinates.get(index++))) {
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
        return;     
      }
    }
  }

  /**
   * Enable/disable the appropriate text fields for the scale section
   *
   */
  private void setScaleState() {

    rbScaleFixed.setEnabled(cbConvertToBytes.isSelected());
    rbScaleSection.setEnabled(cbConvertToBytes.isSelected());
    boolean fixedState =
      cbConvertToBytes.isSelected() && rbScaleFixed.isSelected();
    ltfFixedScaleMin.setEnabled(fixedState);
    ltfFixedScaleMax.setEnabled(fixedState);
    boolean scaleState =
      cbConvertToBytes.isSelected() && rbScaleSection.isSelected();
    ltfSectionScaleMin.setEnabled(scaleState);
    ltfSectionScaleMax.setEnabled(scaleState);
  }

  /**
   * Call setScaleState when the radio buttons change
   * @param event
   */
  protected void scaleAction(ActionEvent event) {
    setScaleState();
  }
  
  public void run3dmod(Run3dmodButton button, Run3dmodMenuOptions menuOptions) {
    run3dmod(button.getActionCommand(), menuOptions);
  }
  
  private void run3dmod(String command, Run3dmodMenuOptions menuOptions) {
    if (command == btnImodFull.getActionCommand()) {
      applicationManager.imodCombinedTomogram(menuOptions);
    }
    if (command == btnImodTrim.getActionCommand()) {
      applicationManager.imodTrimmedVolume(menuOptions);
    }
  }

  protected void buttonAction(ActionEvent event) {
    String command = event.getActionCommand();
    if (command == btnTrimvol.getActionCommand()) {
      applicationManager.trimVolume();
    }
    if (command == btnGetCoordinates.getActionCommand()) {
      setXYMinAndMax(
        applicationManager.imodGetRubberbandCoordinates(
          ImodManager.COMBINED_TOMOGRAM_KEY, AxisID.ONLY));
    } 
    else {
      run3dmod(command, new Run3dmodMenuOptions());
    }
  }

  private void cbConvertToBytesAction(ActionEvent event) {
    boolean state = cbConvertToBytes.isSelected();
    rbScaleFixed.setEnabled(state);
    ltfFixedScaleMax.setEnabled(state);
    ltfFixedScaleMin.setEnabled(state);

    rbScaleSection.setEnabled(state);
    ltfSectionScaleMin.setEnabled(state);
    ltfSectionScaleMax.setEnabled(state);
  }
  /**
   * An inner class to manage the scale controls 
   */
  class ScalingListener implements ActionListener {
    TrimvolPanel listenee;

    ScalingListener(TrimvolPanel TrimvolPanel) {
      listenee = TrimvolPanel;
    }

    public void actionPerformed(ActionEvent event) {
      listenee.scaleAction(event);
    }
  }

  class ButtonListener implements ActionListener {
    TrimvolPanel listenee;

    ButtonListener(TrimvolPanel trimvolPanel) {
      listenee = trimvolPanel;
    }

    public void actionPerformed(ActionEvent event) {
      listenee.buttonAction(event);
    }
  }
  /**
  * Initialize the tooltip text
  */
  private void setToolTipText() {
    String text;
    TooltipFormatter tooltipFormatter = new TooltipFormatter();

    text = "The X coordinate on the left side to retain in the volume.";
    ltfXMin.setToolTipText(tooltipFormatter.setText(text).format());

    text = "The X coordinate on the right side to retain in the volume.";
    ltfXMax.setToolTipText(tooltipFormatter.setText(text).format());

    text = "The lower Y coordinate to retain in the volume.";
    ltfYMin.setToolTipText(tooltipFormatter.setText(text).format());

    text = "The upper Y coordinate to retain in the volume.";
    ltfYMax.setToolTipText(tooltipFormatter.setText(text).format());

    text = "The bottom Z slice to retain in the volume.";
    ltfZMin.setToolTipText(tooltipFormatter.setText(text).format());

    text = "The top Z slice to retain in the volume.";
    ltfZMax.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Scale densities to bytes with extreme densities truncated.";
    cbConvertToBytes.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Set the scaling to match the contrast in a 3dmod display.";
    rbScaleFixed.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "Enter the black contrast slider setting (0-254) that gives the desired "
        + "contrast.";
    ltfFixedScaleMin.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "Enter the white contrast slider setting (1-255) that gives the desired "
        + "contrast.";
    ltfFixedScaleMax.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "Set the scaling based on the range of contrast in a subset of sections. "
        + "Exclude sections with extreme densities that can be truncated (gold "
        + "particles).";
    rbScaleSection.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Minimum Z section of the subset to analyze for contrast range.";
    ltfSectionScaleMin.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Maximum Z section of the subset to analyze for contrast range.";
    ltfSectionScaleMax.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "Flip Y and Z in the output volume so that the file does not need to be "
        + "flipped when loaded into 3dmod.";
    cbSwapYZ.setToolTipText(tooltipFormatter.setText(text).format());

    text = "View the original, untrimmed volume in 3dmod.";
    btnImodFull.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "After pressing the 3dmod Full Volume button, press shift-B in the "
        + "ZaP window.  Create a rubberband around the volume range.  Then "
        + "press this button to retrieve X and Y coordinates.";
    this.btnGetCoordinates.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Trim the original volume with the parameters given above.";
    btnTrimvol.setToolTipText(tooltipFormatter.setText(text).format());

    text = "View the trimmed volume.";
    btnImodTrim.setToolTipText(tooltipFormatter.setText(text).format());
  }
}