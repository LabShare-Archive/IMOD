package etomo.type;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2012</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
* 
* <p> $Log$ </p>
*/
public interface ConstSerialSectionsMetaData {
  public static final String rcsid = "$Id:$";

  public String getStack();

  public ViewType getViewType();

  public AutoAlignmentMetaData getAutoAlignmentMetaData();

  public ConstEtomoNumber getMidasBinning();

  public ConstEtomoNumber getReferenceSection();

  public String getRobustFitCriterion();

  public String getShiftX();

  public String getShiftY();

  public String getSizeX();

  public String getSizeY();

  public boolean isHybridFitsTranslations();

  public boolean isHybridFitsTranslationsRotations();

  public boolean isNoOptions();

  public boolean isNumberToFitGlobalAlignment();

  public int getTab();

  public boolean isTabEmpty();

  public boolean isUseReferenceSection();
}
