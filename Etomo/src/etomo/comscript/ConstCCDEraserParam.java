package etomo.comscript;

/**
 * <p>Description: A read only model of the parameter interface for the
 *  ccderaser program</p>
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
 * <p> $Log$ </p>
 */

public class ConstCCDEraserParam {
  public static final String rcsid = "$Id$";

  protected String inputFile;
  protected String outputFile;
  protected String modelFile;
  protected String globalReplacementList;
  protected String localReplacementList;
  protected String borderPixels;
  protected String polynomialOrder;
  protected boolean includeAdjacentPoints;

  public String getInputFile() {
    return inputFile;
  }
  public String getOutputFile() {
    return outputFile;
  }
  public String getModelFile() {
    return modelFile;
  }
  public String getGlobalReplacementList() {
    return globalReplacementList;
  }
  public String getlocalReplacementList() {
    return localReplacementList;
  }
  public String getBorderPixels() {
    return borderPixels;
  }
  public String getPolynomialOrder() {
    return polynomialOrder;
  }
  public boolean getIncludeAdjacentPoints() {
    return includeAdjacentPoints;
  }

}
