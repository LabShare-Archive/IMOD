package etomo.type;

import java.util.Properties;

import etomo.comscript.BadComScriptException;
import etomo.comscript.ComScriptCommand;
import etomo.comscript.InvalidParameterException;
import etomo.comscript.ParamUtilities;
import etomo.storage.Storable;

/*
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
 * <p> Revision 3.2  2004/12/29 00:12:36  sueh
 * <p> bug# 567 Added set(ComScriptCommand...) and
 * <p> update(ComScriptCommand) to allow TiltAngleSpec to retrieve and update
 * <p> its values in ComScriptCommand.  Added tiltAngleFilenameKey,
 * <p> rangeMinKey, and rangeStepKey to be the keywords to send to
 * <p> ComScriptCommand.  The set() function can also handle alternative
 * <p> keywords.  Added set(TiltAngleSpec) to copy another instance of
 * <p> TiltAngleSpec without copying keywords.
 * <p>
 * <p> Revision 3.1  2004/08/03 18:49:01  sueh
 * <p> bug# 519 removing rangeMax
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.3  2003/10/09 20:27:43  sueh
 * <p> bug264
 * <p> UI Changes
 * <p>
 * <p> Revision 2.2  2003/03/20 17:28:21  rickg
 * <p> Comment update
 * <p>
 * <p> Revision 2.1  2003/03/02 23:30:41  rickg
 * <p> Combine layout in progress
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.1.2.1  2003/01/24 18:37:54  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
public class TiltAngleSpec implements Storable {
  public static final String rcsid = "$Id$";

  TiltAngleType type;
  double rangeMin;
  double rangeStep;
  double tiltAngles[];
  String tiltAngleFilename;
  private String rangeMinKey = "";
  private String rangeStepKey = "";
  private String tiltAngleFilenameKey = "";

  public TiltAngleSpec() {
    reset();
  }

  public TiltAngleSpec(String rangeMinKey, String rangeStepKey,
      String tiltAngleFilenameKey) {
    reset();
    this.rangeMinKey = rangeMinKey;
    this.rangeStepKey = rangeStepKey;
    this.tiltAngleFilenameKey = tiltAngleFilenameKey;
  }

  public TiltAngleSpec(TiltAngleSpec src) {
    set(src);
    rangeMinKey = src.rangeMinKey;
    rangeStepKey = src.rangeStepKey;
    tiltAngleFilenameKey = src.tiltAngleFilenameKey;
  }
  
  public void reset() {
    type = TiltAngleType.EXTRACT;
    rangeMin = -60;
    rangeStep = 1;
    tiltAngles = null;
    tiltAngleFilename = "";
  }
  
  public void set(TiltAngleSpec src) {
    reset();
    type = src.getType();
    rangeMin = src.getRangeMin();
    rangeStep = src.getRangeStep();
    tiltAngleFilename = src.getTiltAngleFilename();
  }

  public void setType(TiltAngleType type) {
    this.type = type;
    // TODO what sort of clean do we need to do upon setting the type
    if (type == TiltAngleType.FILE) {

    }
    if (type == TiltAngleType.RANGE) {

    }
    if (type == TiltAngleType.LIST) {

    }
    if (type == TiltAngleType.EXTRACT) {
    }
  }

  public TiltAngleType getType() {
    return type;
  }

  public void setRangeMin(double rangeMin) {
    this.rangeMin = rangeMin;
  }

  public double getRangeMin() {
    return rangeMin;
  }

  public void setRangeStep(double rangeStep) {
    this.rangeStep = rangeStep;
  }

  public double getRangeStep() {
    return rangeStep;
  }

  public void setTiltAngleFilename(String tiltAngleFilename) {
    //
    // NOTE validation, does it need to exits, format?
    //
    this.tiltAngleFilename = tiltAngleFilename;
  }

  /**
   * Return the filename containing the tilt angles
   */
  public String getTiltAngleFilename() {
    return tiltAngleFilename;
  }

  /**
   * Return the appropriate tilt angle representation depending upon the state
   * of type attirbute.  If type specifies a file then the filename will be
   * returned.  If type specifices a range then the start and increment values
   * will be returned as a comma separated string.  If type specifies a list
   * then a string containing a comma separated list of the tilt angles.  If
   * type specifies extract an empty string is returned.
   */
  public String getTiltAngles() {
    if (type == TiltAngleType.FILE) {
      return tiltAngleFilename;
    }
    if (type == TiltAngleType.RANGE) {
      return String.valueOf(rangeMin) + "," + String.valueOf(rangeStep);
    }
    if (type == TiltAngleType.LIST) {
      StringBuffer list = new StringBuffer();
      for (int i = 0; i < tiltAngles.length - 1; i++) {
        list.append(String.valueOf(tiltAngles[i]) + ",");
      }
      list.append(String.valueOf(tiltAngles[tiltAngles.length - 1]));
      return list.toString();
    }
    return "";
  }

  /**
   *  Insert the objects attributes into the properties object.
   */
  public void store(Properties props) {
    store(props, "");
  }

  public void store(Properties props, String prepend) {
    String group;
    if (prepend == "") {
      group = "TiltAngle.";
    }
    else {
      group = prepend + ".TiltAngle.";
    }
    props.setProperty(group + "Type", type.toString());
    props.setProperty(group + "RangeMin", String.valueOf(rangeMin));
    props.setProperty(group + "RangeStep", String.valueOf(rangeStep));
    props.setProperty(group + "TiltAngleFilename", tiltAngleFilename);
  }

  /**
   *  Get the objects attributes from the properties object.
   */
  public void load(Properties props) {
    load(props, "");
  }

  public void load(Properties props, String prepend) {
    String group;
    if (prepend == "") {
      group = "TiltAngle.";
    }
    else {
      group = prepend + ".TiltAngle.";
    }
    type = TiltAngleType.fromString(props
        .getProperty(group + "Type", "Extract"));
    rangeMin = Double.parseDouble(props.getProperty(group + "RangeMin", "-90"));
    rangeStep = Double.parseDouble(props.getProperty(group + "RangeStep", "1"));
    tiltAngleFilename = props.getProperty(group + "TiltAngleFilename", "");
  }

  public void set(ComScriptCommand scriptCommand, String rangeMinShortKey,
      String rangeStepShortKey, String tiltAngleFilenameShortKey)
      throws InvalidParameterException {
    EtomoNumber number = new EtomoNumber(EtomoNumber.DOUBLE_TYPE);
    //Get rangeMin
    number.set(scriptCommand.getValue(rangeMinKey));
    if (!number.isSet() && rangeMinShortKey != null
        && !rangeMinShortKey.matches("\\s*")) {
      number.set(scriptCommand.getValue(rangeMinShortKey));
    }
    if (number.isSet()) {
      type = TiltAngleType.RANGE;
      rangeMin = number.getDouble();
    }
    //Get rangeStep
    number.set(scriptCommand.getValue(rangeStepKey));
    if (!number.isSet() && rangeStepShortKey != null
        && !rangeStepShortKey.matches("\\s*")) {
      number.set(scriptCommand.getValue(rangeStepShortKey));
    }
    if (number.isSet()) {
      rangeStep = number.getDouble();
    }
    else if ((rangeStepKey == null || rangeStepKey.matches("\\s*"))
        && (rangeMinShortKey == null || rangeMinShortKey.matches("\\s*"))) {
      throw new IllegalStateException("Missing rangeStep key.");
    }
    //Get tiltAngleFilename
    tiltAngleFilename = scriptCommand.getValue(tiltAngleFilenameKey);
    if ((tiltAngleFilename == null || tiltAngleFilename.matches("\\s*"))
        && tiltAngleFilenameShortKey != null
        && !tiltAngleFilenameShortKey.matches("\\s*")) {
      tiltAngleFilename = scriptCommand.getValue(tiltAngleFilenameShortKey);
    }
    if (tiltAngleFilename != null && !tiltAngleFilename.matches("\\s*")) {
      type = TiltAngleType.FILE;
    }
    else {
      tiltAngleFilename = "";
      if ((tiltAngleFilenameKey == null || tiltAngleFilenameKey.matches("\\s*"))
          && (tiltAngleFilenameShortKey == null || tiltAngleFilenameShortKey.matches("\\s*"))) {
        if ((rangeMinKey == null || rangeMinKey.matches("\\s*"))
            && (rangeMinShortKey == null || rangeMinShortKey.matches("\\s*"))) {
          throw new IllegalStateException("Missing keys.");
        }
        throw new IllegalStateException("Missing tiltAngleFilename key.");
      }
    }
  }

  public void update(ComScriptCommand scriptCommand)
      throws BadComScriptException {
    if (type == TiltAngleType.RANGE) {
      ParamUtilities.updateScriptParameter(scriptCommand, rangeMinKey,
          rangeMin);
      ParamUtilities.updateScriptParameter(scriptCommand, rangeStepKey,
          rangeStep);
    }
    else if (type == TiltAngleType.FILE) {
      ParamUtilities.updateScriptParameter(scriptCommand,
          tiltAngleFilenameKey, tiltAngleFilename);
    }
    else {
      throw new BadComScriptException("Type " + type + ", cannot be updated in ComScriptCommand");
    }
  }

}