package etomo.type;
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
* <p> Revision 1.1  2007/02/05 23:24:38  sueh
* <p> bug# 962 Const interface for JoinState.
* <p> </p>
*/
public interface ConstJoinState {
  public static  final String  rcsid =  "$Id$";
  
  public IntKeyList.Walker getJoinStartListWalker(boolean trial);
  public IntKeyList.Walker getJoinEndListWalker(boolean trial);
  public ConstEtomoNumber getJoinShiftInX(boolean trial);
  public ConstEtomoNumber getJoinShiftInY(boolean trial);
  public boolean isJoinLocalFits(boolean trial);
  public ScriptParameter getJoinShiftInXParameter(boolean trial);
  public ScriptParameter getJoinShiftInYParameter(boolean trial);
  public ConstEtomoNumber getJoinTrialBinning();
  public ConstEtomoNumber getJoinAlignmentRefSection(boolean trial);
  public ConstEtomoNumber getJoinSizeInX(boolean trial);
  public ConstEtomoNumber getJoinSizeInY(boolean trial);
  public ScriptParameter getJoinSizeInXParameter(boolean trial);
  public ScriptParameter getJoinSizeInYParameter(boolean trial);
  public ConstEtomoNumber getRefineTrial();
  public IntKeyList.Walker getRefineStartListWalker();
  public IntKeyList.Walker getRefineEndListWalker();
  public String getXfModelOutputFile();
}
