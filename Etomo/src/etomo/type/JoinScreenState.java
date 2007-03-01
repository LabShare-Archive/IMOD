package etomo.type;

import java.util.Properties;

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
 * 
 * <p> $Log$
 * <p> Revision 1.1  2007/02/05 23:28:28  sueh
 * <p> bug# 962 Screen state fields for join.
 * <p> </p>
 */

public class JoinScreenState extends BaseScreenState {
  public static final String rcsid = "$Id$";

  private final EtomoBoolean2 refineWithTrial = new EtomoBoolean2("RefineWithTrial");
  private final IntKeyList bestGapList =  IntKeyList.getStringInstance("BestGap");
  private final IntKeyList meanErrorList =  IntKeyList.getStringInstance("MeanError");
  private final IntKeyList maxErrorList =  IntKeyList.getStringInstance("MaxError");

  public JoinScreenState(AxisID axisID, AxisType axisType) {
    super(axisID, axisType);
  }

  public void store(Properties props, String prepend) {
    super.store(props, prepend);
    prepend = getPrepend(prepend);
    refineWithTrial.store(props, prepend);
    bestGapList.store(props, prepend);
    meanErrorList.store(props, prepend);
    maxErrorList.store(props, prepend);
  }

  public void load(Properties props, String prepend) {
    super.load(props, prepend);
    prepend = getPrepend(prepend);
    refineWithTrial.load(props, prepend);
    bestGapList.load(props, prepend);
    meanErrorList.load(props, prepend);
    maxErrorList.load(props, prepend);
  }

  public ConstEtomoNumber getRefineWithTrial() {
    return refineWithTrial;
  }

  public void setRefineWithTrial(boolean refineWithTrial) {
    this.refineWithTrial.set(refineWithTrial);
  }
  
  public void resetBestGap() {
    bestGapList.reset();
  }

  public void setBestGap(int key, String bestGap) {
    bestGapList.put(key, bestGap);
  }

  public String getBestGap(int key) {
    return bestGapList.getString(key);
  }
  
  public void resetMeanError() {
    meanErrorList.reset();
  }

  public void setMeanError(int key, String meanError) {
    meanErrorList.put(key, meanError);
  }

  public String getMeanError(int key) {
    if (meanErrorList == null) {
      return null;
    }
    return meanErrorList.getString(key);
  }

  public void resetMaxError() {
    maxErrorList.reset();
  }
  
  public void setMaxError(int key, String maxError) {
    maxErrorList.put(key, maxError);
  }

  public String getMaxError(int key) {
    return maxErrorList.getString(key);
  }
}
