package etomo.comscript;
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
 * <p> Revision 2.2  2003/10/02 18:57:47  sueh
 * <p> bug236 added testing:
 * <p> NewstParamTest
 * <p> ComScriptTest
 * <p>
 * <p> Removed marks
 * <p>
 * <p> Revision 2.1  2003/09/29 23:34:57  sueh
 * <p> bug236 Added UseLinearInterpolation to
 * <p> TomogramGenerationDialog.
 * <p>
 * <p> UseLinearInterpolation:
 * <p> check box
 * <p> Advanced
 * <p> newst -linear
 * <p>
 * <p> Files:
 * <p> ComScriptManager.java
 * <p> ConstNewstParam.java
 * <p> NewstParam.java
 * <p> TomogramGenerationDialog.java
 * <p> ApplicationManager.java
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.1.2.1  2003/01/24 18:33:42  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */

public class ConstNewstParam {
  public static final String rcsid =
    "$Id$";

  protected String inputFile = "";

  protected String outputFile = "";

  protected boolean useTransformFile = false;
  protected String transformFile = "";

  protected boolean useSize = false;
  protected String size = "";

  protected boolean useOffset = false;
  protected String offset = "";
  protected boolean useLinearInterpolation = false;

  /**
   * @return
   */

  public boolean isUseLinearInterpolation() {
    return useLinearInterpolation;
  }

  public ConstNewstParam() {
  }

  public String getInputFile() {
    return inputFile;
  }

  public String getOutputFile() {
    return outputFile;
  }

  public String getTransformFile() {
    return transformFile;
  }

  public String getSize() {
    return size;
  }

  public String getOffset() {
    return offset;
  }
  
}
