package etomo.type;

import java.util.Properties;
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
 * <p> Revision 1.1.2.1  2003/01/24 18:37:54  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
public class TiltAngleSpec implements Storable {
  public static final String rcsid =
    "$Id$";

  TiltAngleType type = TiltAngleType.EXTRACT;
  double rangeMin = -90;
  double rangeMax = 90;
  double rangeStep = 1;
  double tiltAngles[];
  String tiltAngleFilename = "";

  public TiltAngleSpec() {
  }

  public TiltAngleSpec(TiltAngleSpec src) {
    type = src.getType();
    rangeMin = src.getRangeMin();
    rangeMax = src.getRangeMax();
    rangeStep = src.getRangeStep();
    tiltAngleFilename = src.getTiltAngleFilename();
  }

  public void setType(TiltAngleType type) {
    this.type = type;
    // FIXME: what sort of clean do we need to do upon setting the type
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
    //
    //  FIXME:rjg
    //  do we validate the range here, what value
    //
    this.rangeMin = rangeMin;
  }

  public double getRangeMin() {
    return rangeMin;
  }

  public void setRangeMax(double rangeMax) {
    //
    //  FIXME:rjg
    //  do we validate the range here, what value
    //
    this.rangeMax = rangeMax;
  }

  public double getRangeMax() {
    return rangeMax;
  }

  public void setRangeStep(double rangeStep) {
    //
    //  FIXME:rjg
    //  do we validate the range here, what value
    //
    this.rangeStep = rangeStep;
  }

  public double getRangeStep() {
    return rangeStep;
  }

  public void setTiltAngleFilename(String tiltAngleFilename) {
    //
    //  FIXME:rjg
    //  validation, does it need to exits, format?
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
    props.setProperty(group + "RangeMax", String.valueOf(rangeMax));
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
    type =
      TiltAngleType.fromString(props.getProperty(group + "Type", "Extract"));
    rangeMin = Double.parseDouble(props.getProperty(group + "RangeMin", "-90"));
    rangeMax = Double.parseDouble(props.getProperty(group + "RangeMax", "90"));
    rangeStep = Double.parseDouble(props.getProperty(group + "RangeStep", "1"));
    tiltAngleFilename = props.getProperty(group + "TiltAngleFilename", "");
  }

}
