package etomo.ui;

import java.awt.*;
import java.awt.event.MouseListener;

import javax.swing.*;

import etomo.type.*;

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
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
public class DataSetTypePanel {
  public static final String rcsid = "$Id$";

  JPanel panelDataSetType = new JPanel();
  BeveledBorder borderDataSetType = new BeveledBorder("Data Set Type");

  GridLayout gridDataSetType = new GridLayout();

  JPanel panelAxis;
  String[] axisTypes = {"Single Axis", "Dual Axis"};
  HighlightList highlightAxisType = new HighlightList(axisTypes);

  JPanel panelViewType;
  String[] viewTypes = {"Single View", "Montaged"};
  HighlightList highlightViewType = new HighlightList(viewTypes);

  JPanel panelSectionType;
  String[] sectionTypes = {"Single Tomogram", "Serial Tomogram"};
  HighlightList highlightSectionType = new HighlightList(sectionTypes);


  public DataSetTypePanel() {
    //
    //  Build the axis type identifier
    //
    panelAxis = highlightAxisType.getPanel();
    panelAxis.setBorder(BorderFactory.createEtchedBorder());

    //
    //  Build the view type identifier
    //
    panelViewType = highlightViewType.getPanel();
    panelViewType.setBorder(BorderFactory.createEtchedBorder());

    //
    //  Build the section type identifier
    //
    panelSectionType = highlightSectionType.getPanel();
    panelSectionType.setBorder(BorderFactory.createEtchedBorder());

    //
    //  Build and decorate the panel
    //
    panelDataSetType.add(panelAxis);
    panelDataSetType.add(panelViewType);
    panelDataSetType.add(panelSectionType);

    panelDataSetType.setBorder(borderDataSetType.getBorder());

    panelDataSetType.setMinimumSize(new Dimension(342, 80));
    panelDataSetType.setPreferredSize(new Dimension(342, 80));
    panelDataSetType.setMaximumSize(new Dimension(2000, 80));
    panelDataSetType.setLayout(gridDataSetType);

    setToolTipText();
  }


  public JPanel getPanel() {
    return panelDataSetType;
  }


  public void setAxisType(AxisType axisType) {
    if(axisType == AxisType.SINGLE_AXIS) {
      highlightAxisType.setSelected(0);
    }
    else {
      highlightAxisType.setSelected(1);
    }
  }


  public void setViewType(ViewType viewType) {
    if(viewType == ViewType.SINGLE_VIEW) {
      highlightViewType.setSelected(0);
    }
    else {
      highlightViewType.setSelected(1);
    }
  }


  public void setSectionType(SectionType sectionType) {
    if(sectionType == SectionType.SINGLE) {
      highlightSectionType.setSelected(0);
    }
    else {
      highlightSectionType.setSelected(1);
    }
  }


  public void addMouseListener(MouseListener listener) {
    panelDataSetType.addMouseListener(listener);
    highlightAxisType.addMouseListener(listener);
    highlightViewType.addMouseListener(listener);
    highlightSectionType.addMouseListener(listener);
  }


  private void setToolTipText(){
    panelAxis.setToolTipText("<html>This highlight panel specifies the current axis type.<br>The axis type is selected in the Setup process below");
    panelViewType.setToolTipText("<html>This highlight panel specifies the current view type.<br>The view type is selected in the Setup process below");
    panelSectionType.setToolTipText("<html>This highlight panel specifies the current section type.<br>The section type is selected in the Setup process below");
  }
}
