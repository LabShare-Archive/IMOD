package etomo.ui;

import java.awt.Component;
import java.awt.Container;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JCheckBox;
import javax.swing.JPanel;
import javax.swing.JRadioButton;

import etomo.EtomoDirector;
import etomo.comscript.TransferfidParam;
import etomo.type.AxisID;
import etomo.type.MetaData;

/**
 * <p>Description: </p>
 *
 * <p>Copyright: Copyright (c) 2002, 2003, 2004, 2005</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
*/
public class TransferfidPanel {
  public static final String rcsid =
    "$Id$";

  private JPanel panelTransferfid = new JPanel();
  MultiLineToggleButton buttonTransferfid = null;
  private boolean includeButton = false;

  private JCheckBox cbRunMidas = new JCheckBox("Run midas");
  private LabeledTextField ltfCenterViewA =
    new LabeledTextField("Center view A: ");
  private LabeledTextField ltfCenterViewB =
    new LabeledTextField("Center view B: ");
	private LabeledTextField ltfNumberViews =
		new LabeledTextField("Number of views in the search: ");
		
  private JPanel panelSearchDirection = new JPanel();
  private ButtonGroup bgSearchDirection = new ButtonGroup();
  private JRadioButton rbSearchBoth = new JRadioButton("Both directions");
  private JRadioButton rbSearchPlus90 = new JRadioButton("+90 (CCW) only");
  private JRadioButton rbSearchMinus90 = new JRadioButton("-90 (CW) only");
  private MetaData metaData;
  private AxisID axisID;

  public TransferfidPanel(AxisID axisID, boolean inclButton) {
    this.axisID = axisID;
    metaData = EtomoDirector.getInstance().getCurrentReconstructionMetaData();
    includeButton = inclButton;
    setup();
  }
 
  private void setup() {
    panelTransferfid.setLayout(
      new BoxLayout(panelTransferfid, BoxLayout.Y_AXIS));
    panelTransferfid.setBorder(
      new EtchedBorder("Transferfid Parameters").getBorder());
    cbRunMidas.setAlignmentX(Component.RIGHT_ALIGNMENT);
    panelTransferfid.add(cbRunMidas);

    //  Add a horizontal strut to keep the panel a minimum size    
    panelTransferfid.add(Box.createHorizontalStrut(300));
    panelTransferfid.add(ltfCenterViewA.getContainer());
    panelTransferfid.add(ltfCenterViewB.getContainer());
		panelTransferfid.add(ltfNumberViews.getContainer());
		
    bgSearchDirection.add(rbSearchBoth);
    bgSearchDirection.add(rbSearchPlus90);
    bgSearchDirection.add(rbSearchMinus90);
    panelSearchDirection.setLayout(
      new BoxLayout(panelSearchDirection, BoxLayout.Y_AXIS));
    panelSearchDirection.setBorder(
      new EtchedBorder("Search Direction").getBorder());
    panelSearchDirection.add(rbSearchBoth);
    panelSearchDirection.add(rbSearchPlus90);
    panelSearchDirection.add(rbSearchMinus90);
    panelSearchDirection.setAlignmentX(Component.CENTER_ALIGNMENT);
    panelTransferfid.add(panelSearchDirection);
    panelTransferfid.add(Box.createRigidArea(FixedDim.x0_y5)); 
    
    if (includeButton) {
      buttonTransferfid =
        new MultiLineToggleButton("Transfer Fiducials From Other Axis");
      buttonTransferfid.setAlignmentX(Component.CENTER_ALIGNMENT);
      buttonTransferfid.setPreferredSize(FixedDim.button2Line);
      buttonTransferfid.setMaximumSize(FixedDim.button2Line);
      panelTransferfid.add(buttonTransferfid);  
      panelTransferfid.add(Box.createRigidArea(FixedDim.x0_y5));   
    }
    
    setToolTipText();
  }
  /**
   * Set the values of the panel using a TransferfidParam parameter
   * object
   */
  public void setParameters() {
    TransferfidParam params = new TransferfidParam(axisID);
    params.initialize();
    if (axisID == AxisID.SECOND) {
      metaData.getTransferfidBFields(params);
    }
    else {
      metaData.getTransferfidAFields(params);
    }
    cbRunMidas.setSelected(params.getRunMidas().is());
    ltfCenterViewA.setText(params.getCenterViewA().toString());
    ltfCenterViewB.setText(params.getCenterViewB().toString());
    ltfNumberViews.setText(params.getNumberViews().toString());

    if (!params.getSearchDirection().isUpdateCommand()) {
      rbSearchBoth.setSelected(true);
    }
    if (params.getSearchDirection().isNegative()) {
      rbSearchMinus90.setSelected(true);
    }
    if (params.getSearchDirection().isPositive()) {
      rbSearchPlus90.setSelected(true);
    }
  }
  
  public void getParameters() {
    getParameters(new TransferfidParam(axisID));
  }

  /**
   * Get the values from the panel filling in the TransferfidParam object
   */
  public void getParameters(TransferfidParam params) {
    params.setRunMidas(cbRunMidas.isSelected());
    params.setCenterViewA(ltfCenterViewA.getText());
    params.setCenterViewB(ltfCenterViewB.getText());
    if (rbSearchBoth.isSelected()) {
      params.getSearchDirection().reset();
    }
    if (rbSearchPlus90.isSelected()) {
      params.setSearchDirection(1);
    }
    if (rbSearchMinus90.isSelected()) {
      params.setSearchDirection(-1);
    }
		params.setNumberViews(ltfNumberViews.getText());
    if (axisID == AxisID.SECOND) {
      metaData.setTransferfidBFields(params);
    }
    else {
      metaData.setTransferfidAFields(params);
    }
  }

  public Container getContainer() {
    return panelTransferfid;
  }

  public void setAdvanced(boolean isAdvanced) {
    ltfCenterViewA.setVisible(isAdvanced);
    ltfCenterViewB.setVisible(isAdvanced);
		ltfNumberViews.setVisible(isAdvanced);
    panelSearchDirection.setVisible(isAdvanced);
  }
  
  public void setEnabled(boolean isEnabled) {
    buttonTransferfid.setEnabled(isEnabled);
    cbRunMidas.setEnabled(isEnabled);
    ltfCenterViewA.setEnabled(isEnabled);
    ltfCenterViewB.setEnabled(isEnabled);
    ltfNumberViews.setEnabled(isEnabled);
    rbSearchBoth.setEnabled(isEnabled);
    rbSearchPlus90.setEnabled(isEnabled);
    rbSearchMinus90.setEnabled(isEnabled);
  }
  
  public MultiLineToggleButton getButton() {
    if (includeButton) {
      return buttonTransferfid;
    }
    
    return null;
  }
  //  ToolTip string setup
  private void setToolTipText() {
    String text;
    TooltipFormatter tooltipFormatter = new TooltipFormatter();

    text = "Run Midas to adjust initial alignment manually.";
    cbRunMidas.setToolTipText(tooltipFormatter.setText(text).format());
    
    text = "View from A around which to search for the best pair of views.";
    ltfCenterViewA.setToolTipText(tooltipFormatter.setText(text).format());

    text = "View from B around which to search for the best pair of views.";
    ltfCenterViewB.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Number of views from each axis to consider in searching for best pair.";
    ltfNumberViews.setToolTipText(tooltipFormatter.setText(text).format());

    text = 
      "Try both +90 and -90 degree rotations in searching for best pair of "
      + "views.";
    rbSearchBoth.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Try only +90 degree rotations in searching for best pair of views.";
    rbSearchPlus90.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Try only -90 degree rotations in searching for best pair of views.";
    rbSearchMinus90.setToolTipText(tooltipFormatter.setText(text).format());
    
    text = 
      "Run Transferfid to make a seed model for this axis from fiducial model for "
      + "the other axis.";
    buttonTransferfid.setToolTipText(tooltipFormatter.setText(text).format()); 
  }

}

/**
 * <p> $Log$
*/



