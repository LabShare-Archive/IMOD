package etomo.ui;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;

import javax.swing.JPanel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import etomo.BaseManager;
import etomo.storage.LogFile;
import etomo.storage.XfjointomoLog;
import etomo.type.ConstJoinMetaData;
import etomo.type.JoinMetaData;
import etomo.type.JoinScreenState;
import etomo.type.SectionTableRowData;

/**
 * <p>Description:</p>
 * 
 * <p>Adjusted start and end:  Widen the sections at the boundaries with positive
 * gaps.  Narrow the sections at the boundaries with negative gaps.</p>
 * 
 * <p>If not inverted and positive gap:  add to end, subtract from start.</p>
 * <p>If not inverted and negative gap:  subtract from end, add to start.</p>
 * <p>If inverted and positive gap:  subtract from end, add to start.</p>
 * <p>If inverted and negative gap:  add to end, subtract from start.</p>
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
 * 
 * <p> $Log$
 * <p> Revision 1.1  2007/02/05 23:33:40  sueh
 * <p> bug# 962 Class representing a single row in the boundary table.
 * <p> </p>
 */
final class BoundaryRow {
  public static final String rcsid = "$Id$";

  private static final String INVERTED_TOOLTIP = "This value comes from an inverted section.";
  private static final String EMPTY_SLICE_WARNING = "Empty slices will be added to the section.";
  private final HeaderCell boundary = new HeaderCell();
  private final HeaderCell sections = new HeaderCell();
  private final FieldCell bestGap = new FieldCell();
  private final FieldCell meanError = new FieldCell();
  private final FieldCell maxError = new FieldCell();
  private final FieldCell origEnd = new FieldCell();
  private final FieldCell origStart = new FieldCell();
  private final SpinnerCell adjustedEnd;
  private final SpinnerCell adjustedStart;
  private final int zMaxEnd;
  private final int zMaxStart;
  private final JPanel panel;
  private final GridBagLayout layout;
  private final GridBagConstraints constraints;
  private final AdjustedEndChangeListener adjustedEndChangeListener;
  private final AdjustedStartChangeListener adjustedStartChangeListener;
  private Gap gap = null;
  private boolean endInverted = false;
  private boolean startInverted = false;

  BoundaryRow(int key, ConstJoinMetaData metaData,
      JoinScreenState screenState,
      JPanel panel, GridBagLayout layout, GridBagConstraints constraints) {
    this.panel = panel;
    this.layout = layout;
    this.constraints = constraints;
    //boundary
    String firstSection = Integer.toString(key);
    boundary.setText(firstSection);
    sections.setText(firstSection+" & "+Integer.toString(key+1));
    //bestGap
    bestGap.setEditable(false);
    bestGap.setValue(screenState.getBestGap(key));
    //meanError
    meanError.setEditable(false);
    meanError.setValue(screenState.getMeanError(key));
    //maxError
    maxError.setEditable(false);
    maxError.setValue(screenState.getMaxError(key));
    //origEnd
    origEnd.setEditable(false);
    SectionTableRowData data = (SectionTableRowData) metaData.getSectionTableData()
        .get(key - 1);
    origEnd.setValue(data.getJoinFinalEnd().getLong());
    endInverted = data.getInverted().is();
    if (endInverted) {
      origEnd.setWarning(true, INVERTED_TOOLTIP);
    }
    else {
      origEnd.setWarning(false, null);
    }
    zMaxEnd = data.getSetupZMax();
    //origStart
    origStart.setEditable(false);
    data = (SectionTableRowData) metaData.getSectionTableData().get(key);
    origStart.setValue(data.getJoinFinalStart().getLong());
    startInverted = data.getInverted().is();
    if (startInverted) {
      origStart.setWarning(true, INVERTED_TOOLTIP);
    }
    else {
      origStart.setWarning(false, null);
    }
    zMaxStart = data.getSetupZMax();
    //adjustedEnd and adjustedStart
    adjustedEnd = SpinnerCell.getLongInstance(zMaxEnd * 2 * -1, zMaxEnd * 2);
    adjustedEnd.setEditable(false);
    adjustedStart = SpinnerCell.getLongInstance(zMaxStart * 2 * -1,
        zMaxStart * 2);
    adjustedStart.setEditable(false);
    setAdjustedValues(metaData);
    //listeners
    adjustedEndChangeListener = new AdjustedEndChangeListener(this);
    adjustedStartChangeListener = new AdjustedStartChangeListener(this);
    adjustedEnd.addChangeListener(adjustedEndChangeListener);
    adjustedStart.addChangeListener(adjustedStartChangeListener);
  }

  /**
   * Sets adjustedEnd and adjustedStart.  Should be called whenever best gap is
   * Changed.
   */
  synchronized private void setAdjustedValues(ConstJoinMetaData metaData) {
    adjustedEnd.removeChangeListener(adjustedEndChangeListener);
    adjustedStart.removeChangeListener(adjustedStartChangeListener);
    gap = new Gap(Math.round(bestGap.getFloatValue()), origEnd.getLongValue(),
        origStart.getLongValue(), endInverted, startInverted, zMaxEnd,
        zMaxStart);
    if (metaData==null||metaData.isBoundaryRowEndListEmpty()) {
      adjustedEnd.setValue(gap.getAdjustedLeft());
      adjustedStart.setValue(gap.getAdjustedRight());
    }
    else {
      long end = metaData.getBoundaryRowEnd(boundary.getInt()).getLong();
      adjustedEnd.setValue(end);
      gap.msgLeftGapBoundaryChanged(end);
      adjustedStart.setValue(gap.getAdjustedRight());
    }
    setAdjustedValueWarnings();
    adjustedEnd.addChangeListener(adjustedEndChangeListener);
    adjustedStart.addChangeListener(adjustedStartChangeListener);
  }

  private void setAdjustedValueWarnings() {
    if (gap.isLeftOk()) {
      adjustedEnd.setWarning(false, null);
    }
    else {
      adjustedEnd.setWarning(true, EMPTY_SLICE_WARNING);
    }
    if (gap.isRightOk()) {
      adjustedStart.setWarning(false, null);
    }
    else {
      adjustedStart.setWarning(true, EMPTY_SLICE_WARNING);
    }
  }

  /**
   * Calculated negative adjustment for end and start.
   * @param roundedBestGap (absolute value of rounded bestGap)
   * @param orig (either origEnd or origStart)
   * @return
   */
  private long calculateNegativeAdjustment(int roundedBestGap, long orig) {
    if (roundedBestGap < 0 || orig <= 0) {
      throw new IllegalStateException(
          "Only pass the absolute value of roundedBestGap.  Orig is either origEnd or origStart and must be at least 1.\nroundedBestGap="
              + roundedBestGap + ",orig=" + orig);
    }
    long adjustment = roundedBestGap / 2;
    //Will have to add negative adjustment to orig and the result must be at
    //least 1.
    if (adjustment > orig - 1) {
      adjustment = orig - 1;
    }
    //Make adjustment negative.
    return adjustment * -1;
  }

  void display(JoinDialog.Tab tab) {
    if (tab == JoinDialog.Tab.MODEL) {
      displayModel();
    }
    else if (tab == JoinDialog.Tab.REJOIN) {
      displayRejoin();
    }
  }

  private void displayModel() {
    constraints.weightx = 0.0;
    constraints.weighty = 0.1;
    constraints.gridwidth = 1;
    boundary.add(panel, layout, constraints);
    constraints.weightx = 0.1;
    bestGap.add(panel, layout, constraints);
    meanError.add(panel, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    maxError.add(panel, layout, constraints);
  }

  private void displayRejoin() {
    constraints.weightx = 0.0;
    constraints.weighty = 0.1;
    constraints.gridwidth = 1;
    sections.add(panel, layout, constraints);
    constraints.weightx = 0.1;
    origEnd.add(panel, layout, constraints);
    origStart.add(panel, layout, constraints);
    bestGap.add(panel, layout, constraints);
    adjustedEnd.add(panel, layout, constraints);
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    adjustedStart.add(panel, layout, constraints);
  }

  void removeDisplay() {
    boundary.remove();
    bestGap.remove();
    meanError.remove();
    maxError.remove();
    origEnd.remove();
    origEnd.remove();
    adjustedEnd.remove();
    adjustedStart.remove();
  }

  void setXfjointomoResult(BaseManager manager) throws LogFile.ReadException {
    XfjointomoLog xfjointomoLog = XfjointomoLog.getInstance(manager);
    String boundary = this.boundary.getText();
    if (!xfjointomoLog.rowExists(boundary)) {
      return;
    }
    bestGap.setValue(xfjointomoLog.getBestGap(boundary));
    meanError.setValue(xfjointomoLog.getMeanError(boundary));
    maxError.setValue(xfjointomoLog.getMaxError(boundary));
    setAdjustedValues(null);
  }

  static void resetScreenState(JoinScreenState screenState) {
    screenState.resetBestGap();
    screenState.resetMeanError();
    screenState.resetMaxError();
  }

  void getScreenState(JoinScreenState screenState) {
    int key = boundary.getInt();
    screenState.setBestGap(key, bestGap.getValue());
    screenState.setMeanError(key, meanError.getValue());
    screenState.setMaxError(key, maxError.getValue());
  }

  static void resetMetaData(JoinMetaData metaData) {
    metaData.resetBoundaryRowStartList();
    metaData.resetBoundaryRowEndList();
  }

  void getMetaData(JoinMetaData metaData) {
    metaData.setBoundaryRowEnd(boundary.getInt(), adjustedEnd.getStringValue());
    metaData.setBoundaryRowStart(boundary.getInt(), adjustedStart.getStringValue());
  }

  synchronized void adjustedEndStateChanged(ChangeEvent event) {
    if (gap == null) {
      setAdjustedValues(null);
    }
    adjustedStart.removeChangeListener(adjustedStartChangeListener);
    gap.msgLeftGapBoundaryChanged(adjustedEnd.getLongValue());
    adjustedStart.setValue(gap.getAdjustedRight());
    setAdjustedValueWarnings();
    adjustedStart.addChangeListener(adjustedStartChangeListener);
  }

  synchronized void adjustedStartStateChanged(ChangeEvent event) {
    if (gap == null) {
      setAdjustedValues(null);
    }
    adjustedEnd.removeChangeListener(adjustedEndChangeListener);
    gap.msgRightGapBoundaryChanged(adjustedStart.getLongValue());
    adjustedEnd.setValue(gap.getAdjustedLeft());
    setAdjustedValueWarnings();
    adjustedEnd.addChangeListener(adjustedEndChangeListener);
  }

  private class AdjustedEndChangeListener implements ChangeListener {
    BoundaryRow adaptee;

    AdjustedEndChangeListener(BoundaryRow adaptee) {
      this.adaptee = adaptee;
    }

    public void stateChanged(ChangeEvent event) {
      adaptee.adjustedEndStateChanged(event);
    }
  }

  private class AdjustedStartChangeListener implements ChangeListener {
    BoundaryRow adaptee;

    AdjustedStartChangeListener(BoundaryRow adaptee) {
      this.adaptee = adaptee;
    }

    public void stateChanged(ChangeEvent event) {
      adaptee.adjustedStartStateChanged(event);
    }
  }

  private static final class Gap {
    private final long gap;
    private final GapBoundary leftBoundary;
    private final GapBoundary rightBoundary;

    private Gap(long gap, long origLeft, long origRight, boolean leftInverted,
        boolean rightInverted, int okMaxLeft, int okMaxRight) {
      this.gap = gap;
      leftBoundary = new GapBoundary(origLeft, leftInverted, okMaxLeft, true);
      rightBoundary = new GapBoundary(origRight, rightInverted, okMaxRight,
          false);
      if (gap == 0) {
        return;
      }
      boolean positiveGap = true;
      if (gap < 0) {
        positiveGap = false;
      }
      //Increment the absolute values of the left and right boundary
      //until the absolute values of the adjustments equal the absolute gap.
      //Try to stay in the ok range.
      long absGap = Math.abs(gap);
      boolean leftSucceeded;
      boolean rightSucceeded = true;
      boolean stayInOkRange = true;
      while (leftBoundary.getAdjustmentAbsValue()
          + rightBoundary.getAdjustmentAbsValue() < absGap) {
        leftSucceeded = leftBoundary.adjust(positiveGap, stayInOkRange);
        if (leftBoundary.getAdjustmentAbsValue()
            + rightBoundary.getAdjustmentAbsValue() < absGap) {
          rightSucceeded = rightBoundary.adjust(positiveGap, stayInOkRange);
        }
        //see if have to go outside of ok range
        stayInOkRange = leftSucceeded || rightSucceeded;
      }
    }

    void msgLeftGapBoundaryChanged(long adjustedLeft) {
      long gapChange = leftBoundary.move(adjustedLeft);
      //The gap has been changed on the left side, change the right side to get
      //back to the original gap
      boolean increasedGap = gapChange > 0;
      long absGapChange = Math.abs(gapChange);
      for (long l = 0; l < absGapChange; l++) {
        rightBoundary.adjust(increasedGap, false);
      }
    }

    void msgRightGapBoundaryChanged(long adjustedRight) {
      long gapChange = rightBoundary.move(adjustedRight);
      //The gap has been changed on the right side, change the left side to get
      //back to the original gap
      boolean increasedGap = gapChange > 0;
      long absGapChange = Math.abs(gapChange);
      for (long l = 0; l < absGapChange; l++) {
        leftBoundary.adjust(increasedGap, false);
      }
    }

    long getAdjustedLeft() {
      return leftBoundary.getAdjusted();
    }

    long getAdjustedRight() {
      return rightBoundary.getAdjusted();
    }

    boolean isLeftOk() {
      return leftBoundary.isOk();
    }

    boolean isRightOk() {
      return rightBoundary.isOk();
    }

    private static final class GapBoundary {
      private final long orig;
      private final boolean inverted;
      private final boolean leftSide;
      private final int okMin = 1;
      private final int okMax;
      private long adjustment = 0;

      private GapBoundary(long orig, boolean inverted, int okMax,
          boolean leftSide) {
        this.orig = orig;
        this.inverted = inverted;
        this.okMax = okMax;
        this.leftSide = leftSide;
      }

      /**
       * Change adjustment by 1 to work with either a positive or negative gap.
       * Widen a section to fill in a positive gap.  Narrow a section to handle
       * a negative gap.
       * Positive gap and not inverted:  add to left side (end), subtract
       * from right side (start).  Positive gap and inverted:  subtract from
       * left side, add to right side.  Negative gap and not inverted:
       * subtract from left side, add to right side.  Negative gap and
       * inverted:  add to left side, subtract from right side.
       * @param positiveGap
       * @param stayInOkRange
       * @return
       */
      boolean adjust(boolean positiveGap, boolean stayInOkRange) {
        if ((positiveGap && leftSide && !inverted)
            || (positiveGap && !leftSide && inverted)
            || (!positiveGap && leftSide && inverted)
            || (!positiveGap && !leftSide && !inverted)) {
          if (!stayInOkRange || orig + adjustment < okMax) {
            adjustment++;
            return true;
          }
          return false;
        }
        if ((positiveGap && !leftSide && !inverted)
            || (positiveGap && leftSide && inverted)
            || (!positiveGap && leftSide && !inverted)
            || (!positiveGap && !leftSide && inverted)) {
          if (!stayInOkRange || orig + adjustment > okMin) {
            adjustment--;
            return true;
          }
          return false;
        }
        return false;
      }

      /**
       * Change the gap size by moving one of the boundaries.
       * Set the new adjustment and get the change in adjustment
       * @param adjusted
       * @return change in gap size -- positive if gap was increased, negative if gap was decreased
       */
      long move(long adjusted) {
        long newAdjustment = adjusted - orig;
        long change = newAdjustment - adjustment;
        adjustment = newAdjustment;
        if (change == 0) {
          return 0;
        }
        //Find out if this move increased the gap size or decreased it
        if ((change > 0 && !leftSide && !inverted)
            || (change > 0 && leftSide && inverted)
            || (change < 0 && leftSide && !inverted)
            || (change < 0 && !leftSide && inverted)) {
          return Math.abs(change);
        }
        if ((change > 0 && leftSide && !inverted)
            || (change > 0 && !leftSide && inverted)
            || (change < 0 && leftSide && inverted)
            || (change < 0 && !leftSide && !inverted)) {
          return Math.abs(change) * -1;
        }
        throw new IllegalStateException();
      }

      boolean isOk() {
        long adjusted = orig + adjustment;
        return adjusted >= okMin && adjusted <= okMax;
      }

      long getAdjustmentAbsValue() {
        return Math.abs(adjustment);
      }

      long getAdjusted() {
        return orig + adjustment;
      }
    }
  }
}
