package etomo.type;

import java.util.Properties;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2002, 2003, 2004</p>
*
*<p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
* 
* <p> $Log$ </p>
*/
public class JoinState implements BaseState {
  public static  final String  rcsid =  "$Id$";
  
  private static final String groupString = "JoinState";
  protected static final String sampleProducedString = "SampleProduced";
  protected static final boolean defaultSampleProduced = false;
  
  //set on the successful completion of finishjoin
  protected EtomoNumber trialBinning = new EtomoNumber(
      EtomoNumber.INTEGER_TYPE, "TrialBinning");
  protected EtomoNumber trialSizeInX = new EtomoNumber(
      EtomoNumber.INTEGER_TYPE, "TrialSizeInX");
  protected EtomoNumber trialSizeInY = new EtomoNumber(
      EtomoNumber.INTEGER_TYPE, "TrialSizeInY");
  protected EtomoNumber trialShiftInX = new EtomoNumber(
      EtomoNumber.INTEGER_TYPE, "TrialShiftInX");
  protected EtomoNumber trialShiftInY = new EtomoNumber(
      EtomoNumber.INTEGER_TYPE, "TrialShiftInY");
  //state variable for join setup tab
  protected boolean sampleProduced;
  
  public JoinState() {
    reset();
  }
  
  void reset() {
    trialBinning.reset();
    trialSizeInX.reset();
    trialSizeInY.reset();
    trialShiftInX.reset();
    trialShiftInY.reset();
    sampleProduced = defaultSampleProduced;
  }
  
  public void store(Properties props) {
    store(props, "");
  }
  
  public void store(Properties props, String prepend) {
    prepend = createPrepend(prepend);
    String group = prepend + ".";
    trialBinning.store(props, prepend);
    trialSizeInX.store(props, prepend);
    trialSizeInY.store(props, prepend);
    trialShiftInX.store(props, prepend);
    trialShiftInY.store(props, prepend);
    props.setProperty(group + sampleProducedString, Boolean.toString(sampleProduced));
  }

  
  public boolean equals(JoinState that) {
    return true;
  }
  
  protected static String createPrepend(String prepend) {
    if (prepend == "") {
      return groupString;
    }
    return prepend + "." + groupString;
  }

  public void load(Properties props) {
    load(props, "");
  }

  public void load(Properties props, String prepend) {
    reset();
    prepend = createPrepend(prepend);
    String group = prepend + ".";
    trialBinning.load(props, prepend);
    trialSizeInX.load(props, prepend);
    trialSizeInY.load(props, prepend);
    trialShiftInX.load(props, prepend);
    trialShiftInY.load(props, prepend);
    sampleProduced = Boolean.valueOf(props.getProperty(group
        + sampleProducedString, Boolean.toString(defaultSampleProduced))).booleanValue();
  }
  
  public int getNewShiftInX(int min, int max) {
    return trialShiftInX.getInteger()
        + (trialSizeInX.getInteger() + 1) / 2 - (max + min) / 2;
  }

  public int getNewShiftInY(int min, int max) {
    return trialShiftInY.getInteger()
        + (trialSizeInY.getInteger() + 1) / 2 - (max + min) / 2;
  }
  
  public ConstEtomoNumber getTrialBinning() {
    return trialBinning;
  }
  
  public boolean isSampleProduced() {
    return sampleProduced;
  }
  
  public void setTrialBinning(int trialBinning) {
    this.trialBinning.set(trialBinning);
  }
  
  public void setTrialShiftInX(int trialShiftInX) {
    this.trialShiftInX.set(trialShiftInX);
  }
  
  public void setTrialShiftInY(int trialShiftInY) {
    this.trialShiftInY.set(trialShiftInY);
  }
  
  public void setTrialSizeInX(int trialSizeInX) {
    this.trialSizeInX.set(trialSizeInX);
  }
  
  public void setTrialSizeInY(int trialSizeInY) {
    this.trialSizeInY.set(trialSizeInY);
  }
  
  public void setSampleProduced(boolean sampleProduced) {
    this.sampleProduced = sampleProduced;
  }

}
