package etomo.comscript;

import etomo.type.TiltAngleSpec;

/**
 * <p>Description: A read only model of the parameter interface for the
 *  tiltalign program</p>
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
 * <p> Revision 2.1  2003/10/14 20:30:43  rickg
 * <p> Bug#279  Label layout and name changes
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.5.2.1  2003/01/24 18:33:42  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.5  2002/12/24 01:09:41  rickg
 * <p> Min local patch size changed to double
 * <p>
 * <p> Revision 1.4  2002/12/18 19:07:09  rickg
 * <p> Added getters for metro factor and cycle limit
 * <p>
 * <p> Revision 1.3  2002/12/10 21:37:21  rickg
 * <p> changed reportStddevThreshold to residualThreshold
 * <p>
 * <p> Revision 1.2  2002/12/03 05:22:56  rickg
 * <p> added getLocalRotationSolutionGroupSize
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
public class ConstTiltalignParam {
  public static final String rcsid =
    "$Id$";

  protected String modelFile;
  protected String imageFile;
  protected FortranInputString imageParameters;
  protected String imodFiducialPosFile;
  protected String asciiFiducialPosFile;
  protected String tiltAngleSolutionFile;
  protected String transformSolutionFile;

  protected int solutionType;
  protected int includeExcludeType;
  protected StringList includeExcludeList;
  //  what is a better name for this parameter
  //  projected image rotation?
  protected double initialImageRotation;
  protected int rotationAngleSolutionType;

  protected int nSeparateViewGroups;
  protected StringList separateViewGroups;

  protected TiltAngleSpec tiltAngleSpec;
  protected double tiltAngleOffset;

  protected TiltalignSolution tiltAngleSolution;
  protected TiltalignSolution magnificationSolution;
  protected TiltalignSolution compressionSolution;

  protected int distortionSolutionType;
  protected TiltalignSolution xstretchSolution;
  protected TiltalignSolution skewSolution;

  protected double residualThreshold;
  protected int nSurfaceAnalysis;
  protected FortranInputString minimizationParams;

  protected double tiltAxisZShift;
  protected double tiltAxisXShift;

  // Local alignment parameters
  protected boolean localAlignments;
  protected String localTransformFile;
  protected FortranInputString nLocalPatches;
  protected FortranInputString minLocalPatchSize;
  protected FortranInputString minLocalFiducials;
  protected boolean fixLocalFiducialCoodinates;
  protected FortranInputString localOutputSelection;

  protected TiltalignSolution localRotationSolution;
  protected TiltalignSolution localTiltSolution;
  protected TiltalignSolution localMagnificationSolution;

  protected int localDistortionSolutionType;
  protected TiltalignSolution localXstretchSolution;
  protected TiltalignSolution localSkewSolution;

  public ConstTiltalignParam() {
    imageParameters = new FortranInputString(6);
    boolean[] temp = { true, true, true, true, true, true };
    imageParameters.setIntegerType(temp);

    includeExcludeList = new StringList(0);
    separateViewGroups = new StringList(0);

    tiltAngleSpec = new TiltAngleSpec();

    tiltAngleSolution = new TiltalignSolution();
    magnificationSolution = new TiltalignSolution();
    compressionSolution = new TiltalignSolution();
    xstretchSolution = new TiltalignSolution();
    skewSolution = new TiltalignSolution();

    minimizationParams = new FortranInputString(2);
    minimizationParams.setIntegerType(1, true);

    nLocalPatches = new FortranInputString(2);
    nLocalPatches.setIntegerType(0, true);
    nLocalPatches.setIntegerType(1, true);

    minLocalPatchSize = new FortranInputString(2);

    minLocalFiducials = new FortranInputString(2);
    minLocalFiducials.setIntegerType(0, true);
    minLocalFiducials.setIntegerType(1, true);

    localOutputSelection = new FortranInputString(3);
    localOutputSelection.setIntegerType(0, true);
    localOutputSelection.setIntegerType(1, true);
    localOutputSelection.setIntegerType(2, true);

    localRotationSolution = new TiltalignSolution();
    localTiltSolution = new TiltalignSolution();
    localMagnificationSolution = new TiltalignSolution();
    localXstretchSolution = new TiltalignSolution();
    localSkewSolution = new TiltalignSolution();
  }

  public String getModelFile() {
    return modelFile;
  }

  public String getImageFile() {
    return imageFile;
  }

  public String getImageParameters() {
    return imageParameters.toString();
  }

  public String getIMODFiducialPosFile() {
    return imodFiducialPosFile;
  }

  public String getAsciiFiducialPosFile() {
    return asciiFiducialPosFile;
  }

  public String getTiltAngleSolutionFile() {
    return tiltAngleSolutionFile;
  }

  public String getTransformSolutionFile() {
    return transformSolutionFile;
  }

  public int getSolutionType() {
    return solutionType;
  }

  public int getIncludeExcludeType() {
    return includeExcludeType;
  }

  public String getIncludeExcludeList() {
    return includeExcludeList.toString();
  }

  public double getInitialImageRotation() {
    return initialImageRotation;
  }

  public int getRotationAngleSolutionType() {
    return rotationAngleSolutionType;
  }

  public int getNSeparateViewGroups() {
    return nSeparateViewGroups;
  }

  public String getSeparateViewGroups() {
    return separateViewGroups.toString();
  }

  public TiltAngleSpec getTiltAngleSpec() {
    return tiltAngleSpec;
  }

  public double getTiltAngleOffset() {
    return tiltAngleOffset;
  }

  public int getTiltAngleSolutionType() {
    return tiltAngleSolution.type;
  }

  public String getTiltAngleSolutionParams() {
    return tiltAngleSolution.params.toString();
  }

  public int getTiltAngleSolutionGroupSize() {
    return tiltAngleSolution.params.getInt(0);
  }

  public String getTiltAngleSolutionAdditionalGroups() {
    return tiltAngleSolution.additionalGroups.toString();
  }

  public String getMagnificationSolutionReferenceView() {
    return magnificationSolution.referenceView.toString();
  }

  public int getMagnificationSolutionType() {
    return magnificationSolution.type;
  }

  public String getMagnificationSolutionParams() {
    return magnificationSolution.params.toString();
  }

  public int getMagnificationSolutionGroupSize() {
    return magnificationSolution.params.getInt(0);
  }

  public String getMagnificationSolutionAdditionalGroups() {
    return magnificationSolution.additionalGroups.toString();
  }

  public String getCompressionSolutionReferenceView() {
    return compressionSolution.referenceView.toString();
  }

  public int getCompressionSolutionType() {
    return compressionSolution.type;
  }

  public String getCompressionSolutionParams() {
    return compressionSolution.params.toString();
  }

  public int getCompressionSolutionGroupSize() {
    return compressionSolution.params.getInt(0);
  }

  public String getCompressionSolutionAdditionalGroups() {
    return compressionSolution.additionalGroups.toString();
  }

  public int getDistortionSolutionType() {
    return distortionSolutionType;
  }

  public int getXstretchSolutionType() {
    return xstretchSolution.type;
  }

  public String getXstretchSolutionParams() {
    return xstretchSolution.params.toString();
  }

  public int getXstretchSolutionGroupSize() {
    return xstretchSolution.params.getInt(0);
  }

  public String getXstretchSolutionAdditionalGroups() {
    return xstretchSolution.additionalGroups.toString();
  }

  public int getSkewSolutionType() {
    return skewSolution.type;
  }

  public String getSkewSolutionParams() {
    return skewSolution.params.toString();
  }

  public int getSkewSolutionGroupSize() {
    return skewSolution.params.getInt(0);
  }

  public String getSkewSolutionAdditionalGroups() {
    return skewSolution.additionalGroups.toString();
  }

  public double getResidualThreshold() {
    return residualThreshold;
  }

  public int getNSurfaceAnalysis() {
    return nSurfaceAnalysis;
  }

  public String getMinimizationParams() {
    return minimizationParams.toString();
  }

  public double getMetroFactor() {
    return minimizationParams.getDouble(0);
  }

  public int getCycleLimit() {
    return minimizationParams.getInt(1);
  }

  public double getTiltAxisZShift() {
    return tiltAxisZShift;
  }

  public double getTiltAxisXShift() {
    return tiltAxisXShift;
  }

  public boolean getLocalAlignments() {
    return localAlignments;
  }

  public String getLocalTransformFile() {
    return localTransformFile;
  }

  public String getNLocalPatches() {
    return nLocalPatches.toString();
  }

  public String getMinLocalPatchSize() {
    return minLocalPatchSize.toString();
  }

  public String getMinLocalFiducials() {
    return minLocalFiducials.toString();
  }

  public boolean getFixLocalFiducialCoodinates() {
    return fixLocalFiducialCoodinates;
  }

  public String getLocalOutputSelection() {
    return localOutputSelection.toString();
  }

  public int getLocalRotationSolutionType() {
    return localRotationSolution.type;
  }
  public int getLocalRotationSolutionGroupSize() {
    return localRotationSolution.params.getInt(0);
  }

  public String getLocalRotationSolutionParams() {
    return localRotationSolution.params.toString();
  }

  public String getLocalRotationAdditionalGroups() {
    return localRotationSolution.additionalGroups.toString();
  }

  public int getLocalTiltSolutionType() {
    return localTiltSolution.type;
  }

  public int getLocalTiltSolutionGroupSize() {
    return localTiltSolution.params.getInt(0);
  }

  public String getLocalTiltSolutionParams() {
    return localTiltSolution.params.toString();
  }

  public String getLocalTiltAdditionalGroups() {
    return localTiltSolution.additionalGroups.toString();
  }

  public String getLocalMagnificationSolutionReferenceView() {
    return localMagnificationSolution.referenceView.toString();
  }

  public int getLocalMagnificationSolutionType() {
    return localMagnificationSolution.type;
  }

  public String getLocalMagnificationSolutionParams() {
    return localMagnificationSolution.params.toString();
  }

  public int getLocalMagnificationSolutionGroupSize() {
    return localMagnificationSolution.params.getInt(0);
  }

  public String getLocalMagnificationSolutionAdditionalGroups() {
    return localMagnificationSolution.additionalGroups.toString();
  }

  public String getLocalMagnificationAdditionalGroups() {
    return localMagnificationSolution.additionalGroups.toString();
  }

  public int getLocalDistortionSolutionType() {
    return localDistortionSolutionType;
  }

  public int getLocalXstretchSolutionType() {
    return localXstretchSolution.type;
  }

  public String getLocalXstretchSolutionParams() {
    return localXstretchSolution.params.toString();
  }

  public int getLocalXstretchSolutionGroupSize() {
    return localXstretchSolution.params.getInt(0);
  }

  public String getLocalXstretchSolutionAdditionalGroups() {
    return localXstretchSolution.additionalGroups.toString();
  }

  public int getLocalSkewSolutionType() {
    return localSkewSolution.type;
  }

  public String getLocalSkewSolutionParams() {
    return localSkewSolution.params.toString();
  }

  public int getLocalSkewSolutionGroupSize() {
    return localSkewSolution.params.getInt(0);
  }

  public String getLocalSkewSolutionAdditionalGroups() {
    return localSkewSolution.additionalGroups.toString();
  }

}
