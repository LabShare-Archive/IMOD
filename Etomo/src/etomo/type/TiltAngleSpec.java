package etomo.type;

import java.util.Properties;

import etomo.comscript.BadComScriptException;
import etomo.comscript.ComScriptCommand;
import etomo.comscript.FortranInputString;
import etomo.comscript.FortranInputStringList;
import etomo.comscript.FortranInputSyntaxException;
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
 * <p> Revision 3.9  2010/03/03 04:59:29  sueh
 * <p> bug# 1311 Added toString
 * <p>
 * <p> Revision 3.8  2007/12/26 22:19:42  sueh
 * <p> bug# 1052 Added functions with string parameters to set range functions to
 * <p> avoid doing conversions in calling objects.
 * <p>
 * <p> Revision 3.7  2007/02/05 23:31:33  sueh
 * <p> bug# 962 Moved EtomoNumber type info to inner class.
 * <p>
 * <p> Revision 3.6  2005/05/10 03:22:32  sueh
 * <p> bug# 658 Remove the constructor which sets keys, do this with set
 * <p> functions.    Change set(ComScriptCommand) to
 * <p> parse(ComScriptCommand).  Since ScriptParameter now handles short
 * <p> keys internally, simplify parse(ComScriptCommand).  Change update() to
 * <p> updateComScript().  Add tiltAngles to parse(), updateComScript(), store(),
 * <p> and load().
 * <p>
 * <p> Revision 3.5  2005/01/26 00:01:07  sueh
 * <p> Converted EtomoNumber parameters to ScriptParameters.
 * <p>
 * <p> Revision 3.4  2005/01/21 23:31:50  sueh
 * <p> bug# 509 bug# 591  Changed set(ComScriptCommand, String...) to use
 * <p> EtomoNumber.parse().
 * <p>
 * <p> Revision 3.3  2005/01/10 23:51:40  sueh
 * <p> bug# 578 Changing calls to ConstEtomoNumber.isNull() to !isSet().
 * <p>
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

  public static final String STORE_KEY = "TiltAngle";
  public static final String TYPE_STORE_KEY = "Type";
  public static final String RANGE_MIN_STORE_KEY = "RangeMin";
  public static final String TILT_ANGLES_STORE_KEY = "TiltAngles";

  TiltAngleType type;
  double rangeMin;
  double rangeStep;
  double tiltAngles[];
  String tiltAngleFilename;
  private String rangeMinKey = null;
  private String rangeStepKey = null;
  private String tiltAngleFilenameKey = null;
  private String tiltAnglesKey = null;
  private String rangeMinShortKey = null;
  private String rangeStepShortKey = null;
  private String tiltAngleFilenameShortKey = null;
  private String tiltAnglesShortKey = null;

  public String toString() {
    return "[type=" + type + ",rangeMin=" + rangeMin + ",rangeStep=" + rangeStep
        + ",tiltAngles=" + tiltAngles + ",tiltAngleFilename=" + tiltAngleFilename + "]";
  }

  public TiltAngleSpec() {
    reset();
  }

  public TiltAngleSpec(TiltAngleSpec src) {
    set(src);
    rangeMinKey = src.rangeMinKey;
    rangeStepKey = src.rangeStepKey;
    tiltAngleFilenameKey = src.tiltAngleFilenameKey;
    tiltAnglesKey = src.tiltAnglesKey;
  }

  public void setRangeMinKey(String rangeMinKey, String rangeMinShortKey) {
    this.rangeMinKey = rangeMinKey;
    this.rangeMinShortKey = rangeMinShortKey;
  }

  public void setRangeStepKey(String rangeStepKey, String rangeStepShortKey) {
    this.rangeStepKey = rangeStepKey;
    this.rangeStepShortKey = rangeStepShortKey;
  }

  public void setTiltAngleFilenameKey(String tiltAngleFilenameKey,
      String tiltAngleFilenameShortKey) {
    this.tiltAngleFilenameKey = tiltAngleFilenameKey;
    this.tiltAngleFilenameShortKey = tiltAngleFilenameShortKey;
  }

  public void setTiltAnglesKey(String tiltAnglesKey, String tiltAnglesShortKey) {
    this.tiltAnglesKey = tiltAnglesKey;
    this.tiltAnglesShortKey = tiltAnglesShortKey;
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
    if (src != null) {
      type = src.getType();
      rangeMin = src.getRangeMin();
      rangeStep = src.getRangeStep();
      tiltAngleFilename = src.getTiltAngleFilename();
    }
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

  public void setRangeMin(String rangeMin) {
    this.rangeMin = Double.parseDouble(rangeMin);
  }

  public double getRangeMin() {
    return rangeMin;
  }

  public void setRangeStep(double rangeStep) {
    this.rangeStep = rangeStep;
  }

  public void setRangeStep(String rangeStep) {
    this.rangeStep = Double.parseDouble(rangeStep);
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
      group = STORE_KEY;
    }
    else {
      group = prepend + "." + STORE_KEY;
    }
    props.setProperty(group + "." + TYPE_STORE_KEY, type.toString());
    props.setProperty(group + "." + RANGE_MIN_STORE_KEY, String.valueOf(rangeMin));
    props.setProperty(group + ".RangeStep", String.valueOf(rangeStep));
    props.setProperty(group + ".TiltAngleFilename", tiltAngleFilename);
    if (tiltAngles != null && tiltAngles.length > 0) {
      FortranInputString list = FortranInputString.getInstance(tiltAngles);
      list.store(props, group + "." + TILT_ANGLES_STORE_KEY);
    }
  }

  /**
   *  Get the objects attributes from the properties object.
   */
  public void load(Properties props) {
    load(props, "");
  }

  /**
   *  Get the objects attributes from the properties object.
   */
  public void load(Properties props, String prepend) {
    String group;
    if (prepend == "") {
      group = STORE_KEY;
    }
    else {
      group = prepend + "." + STORE_KEY;
    }
    type = TiltAngleType.fromString(props.getProperty(group + "." + TYPE_STORE_KEY,
        "Extract"));
    rangeMin = Double.parseDouble(props.getProperty(group + ".RangeMin", "-90"));
    rangeStep = Double.parseDouble(props.getProperty(group + ".RangeStep", "1"));
    tiltAngleFilename = props.getProperty(group + ".TiltAngleFilename", "");
    try {
      FortranInputString list = FortranInputString.getInstance(props, group + "."
          + TILT_ANGLES_STORE_KEY);
      if (list != null && list.size() > 0) {
        tiltAngles = list.getDouble();
      }
    }
    catch (FortranInputSyntaxException e) {
      e.printStackTrace();
    }
  }

  public void parse(ComScriptCommand scriptCommand) throws InvalidParameterException,
      FortranInputSyntaxException {
    //Get rangeMin
    ScriptParameter rangeMin = new ScriptParameter(EtomoNumber.Type.DOUBLE, rangeMinKey,
        rangeMinShortKey);
    rangeMin.parse(scriptCommand);
    if (!rangeMin.isNull()) {
      type = TiltAngleType.RANGE;
      this.rangeMin = rangeMin.getDouble();
      //Get rangeStep
      ScriptParameter rangeStep = new ScriptParameter(EtomoNumber.Type.DOUBLE,
          rangeStepKey, rangeStepShortKey);
      rangeStep.parse(scriptCommand);
      if (!rangeStep.isNull()) {
        this.rangeStep = rangeStep.getDouble();
      }
      else if (rangeStepKey == null && rangeMinShortKey == null) {
        throw new IllegalStateException("Missing rangeStep key.");
      }
      else {
        throw new InvalidParameterException(
            "First tilt angle is set, but increment is missing.");
      }
    }
    //Get tiltAngleFilename
    tiltAngleFilename = scriptCommand.getValue(tiltAngleFilenameKey);
    if ((tiltAngleFilename == null || tiltAngleFilename.matches("\\s*"))
        && tiltAngleFilenameShortKey != null) {
      tiltAngleFilename = scriptCommand.getValue(tiltAngleFilenameShortKey);
    }
    if (tiltAngleFilename != null && !tiltAngleFilename.matches("\\s*")) {
      type = TiltAngleType.FILE;
    }
    else {
      tiltAngleFilename = "";
    }
    //Get tiltAngles (Successive entries accumulate)
    if (tiltAnglesKey != null && !tiltAnglesKey.matches("\\s*")) {
      FortranInputStringList list = new FortranInputStringList(tiltAnglesKey);
      list.parse(scriptCommand);
      //Currently consolidating successive entries.  If this is problem, will
      //have to change how tiltAngles is stored.
      tiltAngles = list.getDouble();
    }
  }

  public void updateComScript(ComScriptCommand scriptCommand)
      throws BadComScriptException {
    if (type == TiltAngleType.RANGE) {
      ParamUtilities.updateScriptParameter(scriptCommand, rangeMinKey, rangeMin);
      ParamUtilities.updateScriptParameter(scriptCommand, rangeStepKey, rangeStep);
    }
    else if (type == TiltAngleType.FILE) {
      ParamUtilities.updateScriptParameter(scriptCommand, tiltAngleFilenameKey,
          tiltAngleFilename);
    }
    else if (type == TiltAngleType.LIST) {
      if (tiltAngles != null && tiltAngles.length > 0) {
        FortranInputString list = FortranInputString.getInstance(tiltAngles);
        ParamUtilities.updateScriptParameter(scriptCommand, tiltAnglesKey, list);
      }
    }
    else {
      throw new BadComScriptException("Type " + type
          + ", cannot be updated in ComScriptCommand");
    }
  }
}