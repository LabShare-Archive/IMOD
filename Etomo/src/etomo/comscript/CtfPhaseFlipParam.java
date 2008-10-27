package etomo.comscript;

import etomo.type.ConstEtomoNumber;
import etomo.type.ConstStringParameter;
import etomo.type.EtomoNumber;
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
 * <p> $Log$ </p>
 */
public final class CtfPhaseFlipParam implements ConstCtfPhaseFlipParam,
    CommandParam {
  public static final String rcsid = "$Id$";

  public static final String COMMAND = "ctfphaseflip";
  public static final String VOLTAGE_OPTION = "Voltage";
  public static final String SPHERICAL_ABERRATION_OPTION = "SphericalAberration";
  public static final String AMPLITUDE_CONTRAST_OPTION = "AmplitudeContrast";
  public static final String INTERPOLATION_WIDTH_OPTION = "InterpolationWidth";
  public static final String DEFOCUS_TOL_OPTION = "DefocusTol";

  private final ScriptParameter voltage = new ScriptParameter(VOLTAGE_OPTION);
  private final ScriptParameter sphericalAberration = new ScriptParameter(
      SPHERICAL_ABERRATION_OPTION);
  private final ScriptParameter amplitudeContrast = new ScriptParameter(
      EtomoNumber.Type.FLOAT, AMPLITUDE_CONTRAST_OPTION);
  private final StringParameter defocusFile = new StringParameter("DefocusFile");
  private final ScriptParameter interpolationWidth = new ScriptParameter(
      INTERPOLATION_WIDTH_OPTION);
  private final ScriptParameter defocusTol = new ScriptParameter(
      DEFOCUS_TOL_OPTION);

  public void parseComScriptCommand(ComScriptCommand scriptCommand)
      throws BadComScriptException, InvalidParameterException,
      FortranInputSyntaxException {
    reset();
    voltage.parse(scriptCommand);
    sphericalAberration.parse(scriptCommand);
    amplitudeContrast.parse(scriptCommand);
    defocusFile.parse(scriptCommand);
    interpolationWidth.parse(scriptCommand);
    defocusTol.parse(scriptCommand);
  }

  public void updateComScriptCommand(ComScriptCommand scriptCommand)
      throws BadComScriptException {
    voltage.updateComScript(scriptCommand);
    sphericalAberration.updateComScript(scriptCommand);
    amplitudeContrast.updateComScript(scriptCommand);
    defocusFile.updateComScript(scriptCommand);
    interpolationWidth.updateComScript(scriptCommand);
    defocusTol.updateComScript(scriptCommand);
  }

  public void initializeDefaults() {
  }

  private void reset() {
    voltage.reset();
    sphericalAberration.reset();
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

  public ConstEtomoNumber getSphericalAberration() {
    return sphericalAberration;
  }

  public void setSphericalAberration(String input) {
    sphericalAberration.set(input);
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
}
