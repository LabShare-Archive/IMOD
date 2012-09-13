package etomo.comscript;

import java.io.File;

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstStringParameter;
import etomo.type.EtomoBoolean2;
import etomo.type.EtomoNumber;
import etomo.type.FileType;
import etomo.type.ProcessName;
import etomo.type.ScriptParameter;
import etomo.type.StringParameter;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2008</p>
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
 * <p> Revision 1.6  2011/02/21 21:24:05  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.5  2010/04/28 15:52:53  sueh
 * <p> bug# 1344 Implemented Command.  Added getOutputImageFileType
 * <p> functions.
 * <p>
 * <p> Revision 1.4  2009/10/19 16:28:11  sueh
 * <p> bug# 1253 Added invertTiltAngles.
 * <p>
 * <p> Revision 1.3  2009/02/25 00:14:20  sueh
 * <p> bug# 1182 Made sphericalAberration a double.
 * <p>
 * <p> Revision 1.2  2008/12/09 21:07:04  sueh
 * <p> bug# 1154 Added outputFileName, for updating in the comscript only -
 * <p> not necessary to parse from the comscript because it is always set to the
 * <p> default.
 * <p>
 * <p> Revision 1.1  2008/10/27 17:48:20  sueh
 * <p> bug# 1141 Parameters to update a phaseflip call.  The call is in
 * <p> ctfcorrection.com.
 * <p> </p>
 */
public final class CtfPhaseFlipParam implements ConstCtfPhaseFlipParam, CommandParam {
  public static final String rcsid = "$Id$";

  public static final String COMMAND = "ctfphaseflip";
  public static final String VOLTAGE_OPTION = "Voltage";
  public static final String SPHERICAL_ABERRATION_OPTION = "SphericalAberration";
  public static final String INVERT_TILT_ANGLES_OPTION = "InvertTiltAngles";
  public static final String AMPLITUDE_CONTRAST_OPTION = "AmplitudeContrast";
  public static final String INTERPOLATION_WIDTH_OPTION = "InterpolationWidth";
  public static final String DEFOCUS_TOL_OPTION = "DefocusTol";
  public static final String PIXEL_SIZE_OPTION = "PixelSize";

  private final ScriptParameter voltage = new ScriptParameter(VOLTAGE_OPTION);
  private final ScriptParameter sphericalAberration = new ScriptParameter(
      EtomoNumber.Type.DOUBLE, SPHERICAL_ABERRATION_OPTION);
  private final EtomoBoolean2 invertTiltAngles = new EtomoBoolean2(
      INVERT_TILT_ANGLES_OPTION);
  private final ScriptParameter amplitudeContrast = new ScriptParameter(
      EtomoNumber.Type.DOUBLE, AMPLITUDE_CONTRAST_OPTION);
  private final StringParameter defocusFile = new StringParameter("DefocusFile");
  private final ScriptParameter interpolationWidth = new ScriptParameter(
      INTERPOLATION_WIDTH_OPTION);
  private final ScriptParameter defocusTol = new ScriptParameter(DEFOCUS_TOL_OPTION);
  private final StringParameter outputFileName = new StringParameter("OutputFileName");
  private final ScriptParameter pixelSize = new ScriptParameter(EtomoNumber.Type.DOUBLE,
      PIXEL_SIZE_OPTION);

  private final BaseManager manager;
  private final AxisID axisID;

  CtfPhaseFlipParam(final BaseManager manager, final AxisID axisID) {
    this.manager = manager;
    this.axisID = axisID;
  }

  public void parseComScriptCommand(ComScriptCommand scriptCommand)
      throws BadComScriptException, InvalidParameterException,
      FortranInputSyntaxException {
    reset();
    voltage.parse(scriptCommand);
    sphericalAberration.parse(scriptCommand);
    invertTiltAngles.parse(scriptCommand);
    amplitudeContrast.parse(scriptCommand);
    defocusFile.parse(scriptCommand);
    interpolationWidth.parse(scriptCommand);
    defocusTol.parse(scriptCommand);
  }

  public void updateComScriptCommand(ComScriptCommand scriptCommand)
      throws BadComScriptException {
    voltage.updateComScript(scriptCommand);
    sphericalAberration.updateComScript(scriptCommand);
    invertTiltAngles.updateComScript(scriptCommand);
    amplitudeContrast.updateComScript(scriptCommand);
    defocusFile.updateComScript(scriptCommand);
    interpolationWidth.updateComScript(scriptCommand);
    defocusTol.updateComScript(scriptCommand);
    outputFileName.updateComScript(scriptCommand);
    pixelSize.updateComScript(scriptCommand);
  }

  public void initializeDefaults() {
  }

  private void reset() {
    voltage.reset();
    sphericalAberration.reset();
    invertTiltAngles.reset();
    amplitudeContrast.reset();
    defocusFile.reset();
    interpolationWidth.reset();
    defocusTol.reset();
  }

  public ConstEtomoNumber getVoltage() {
    return voltage;
  }

  public void setVoltage(String input) {
    voltage.set(input);
  }

  public void setOutputFileName(String input) {
    outputFileName.set(input);
  }

  public FileType getOutputImageFileType() {
    return FileType.CTF_CORRECTED_STACK;
  }

  public FileType getOutputImageFileType2() {
    return null;
  }

  public ConstEtomoNumber getSphericalAberration() {
    return sphericalAberration;
  }

  public boolean getInvertTiltAngles() {
    return invertTiltAngles.is();
  }

  public void setSphericalAberration(String input) {
    sphericalAberration.set(input);
  }

  public void setInvertTiltAngles(boolean input) {
    invertTiltAngles.set(input);
  }

  public ConstEtomoNumber getAmplitudeContrast() {
    return amplitudeContrast;
  }

  public void setAmplitudeContrast(String input) {
    amplitudeContrast.set(input);
  }

  public void setDefocusFile(String input) {
    defocusFile.set(input);
  }

  public ConstStringParameter getDefocusFile() {
    return defocusFile;
  }

  public ConstEtomoNumber getInterpolationWidth() {
    return interpolationWidth;
  }

  public void setInterpolationWidth(String input) {
    interpolationWidth.set(input);
  }

  public ConstEtomoNumber getDefocusTol() {
    return defocusTol;
  }

  public void setDefocusTol(String input) {
    defocusTol.set(input);
  }
  
  public void setPixelSize(double input) {
    pixelSize.set(input);
  }

  public AxisID getAxisID() {
    return axisID;
  }

  public String getCommand() {
    return FileType.CTF_CORRECTION_COMSCRIPT.getFileName(manager, axisID);
  }

  public String[] getCommandArray() {
    String[] array = { getCommandLine() };
    return array;
  }

  public File getCommandInputFile() {
    return null;
  }

  public String getCommandLine() {
    return getCommand();
  }

  public CommandMode getCommandMode() {
    return null;
  }

  public String getCommandName() {
    return ProcessName.CTF_CORRECTION.toString();
  }

  public File getCommandOutputFile() {
    return null;
  }

  public ProcessName getProcessName() {
    return ProcessName.CTF_CORRECTION;
  }

  public CommandDetails getSubcommandDetails() {
    return null;
  }

  public String getSubcommandProcessName() {
    return null;
  }

  public boolean isMessageReporter() {
    return false;
  }
}
