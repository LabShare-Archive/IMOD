package etomo.ui;

import java.awt.Container;
import java.awt.GridLayout;

import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JCheckBox;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
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
 * <p> Revision 1.1  2003/04/09 23:37:20  rickg
 * <p> In progress
 * <p> </p>
 */

public class TrimvolPanel {
  public static final String rcsid =
    "$Id$";

  private JPanel pnlTrimvol = new JPanel();

  private JPanel pnlRange = new JPanel();
  private LabeledTextField ltfXMin = new LabeledTextField("X min: ");
  private LabeledTextField ltfXMax = new LabeledTextField("X max: ");
  private LabeledTextField ltfYMin = new LabeledTextField("Y min: ");
  private LabeledTextField ltfYMax = new LabeledTextField("Y max: ");
  private LabeledTextField ltfZMin = new LabeledTextField("Z min: ");
  private LabeledTextField ltfZMax = new LabeledTextField("Z max: ");

  private JPanel pnlScale = new JPanel();
  private JPanel pnlScaleFixed = new JPanel();
  private JRadioButton rbScaleFixed = new JRadioButton("Fixed scaling - ");
  private LabeledTextField ltfFixedScaleMin = new LabeledTextField("min: ");
  private LabeledTextField ltfFixedScaleMax = new LabeledTextField("max: ");

  private JRadioButton rbScaleSection = new JRadioButton("Section based - ");
  private JPanel pnlScaleSection = new JPanel();
  private LabeledTextField ltfSectionScaleMin = new LabeledTextField("min: ");
  private LabeledTextField ltfSectionScaleMax = new LabeledTextField("max: ");

  private JCheckBox cbSwapXYZ = new JCheckBox();

  public TrimvolPanel() {

    //  Layout the range panel
    pnlRange.setLayout(new GridLayout(3, 2));
    pnlRange.setBorder(
         new EtchedBorder("Volume range").getBorder());

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
    pnlScaleSection.add(ltfSectionScaleMin.getContainer());
    pnlScaleSection.add(ltfSectionScaleMax.getContainer());
    
    ButtonGroup bgScale = new ButtonGroup();
    bgScale.add(rbScaleFixed);
    bgScale.add(rbScaleSection);
    
    pnlScale.setLayout(new BoxLayout(pnlScale, BoxLayout.Y_AXIS));    
    pnlScale.setBorder(
         new EtchedBorder("Scaling").getBorder());

    pnlScale.add(pnlScaleFixed);
    pnlScale.add(pnlScaleSection);
    
    pnlTrimvol.setLayout(new BoxLayout(pnlTrimvol, BoxLayout.Y_AXIS));
    pnlTrimvol.setBorder(
         new BeveledBorder("Volume trimming").getBorder());
         
    pnlTrimvol.add(pnlRange);
    pnlTrimvol.add(pnlScale); 
  }
  
  public Container getContainer() {
    return pnlTrimvol;
  }

}
