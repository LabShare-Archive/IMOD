package etomo.type;

/**
 * <p>Description:
 * Represents hours, minutes, and seconds.  Can pull hours, minutes, and seconds
 * out of a date string.  Can compare times with almost equals, which allows
 * off-by-one errors in seconds.</p>
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
 */
public final class Time{
  public static final String rcsid = "$Id$";

  private static final char DIVIDER = ':';
  private int hours = 0;
  private int minutes = 0;
  private int seconds = 0;

  public Time(String date) {
    parse(date);
  }

  private void reset() {
    hours = 0;
    minutes = 0;
    seconds = 0;
  }

  /**
   * Finds a string of the format HH:MM:SS or HH:MMAM in the date parameter and
   * uses it to set its member variables.
   * @param date
   */
  private void parse(String date) {
    if (date == null) {
      return;
    }
    String[] dateArray = date.split("\\s+");
    if (dateArray == null) {
      return;
    }
    for (int i = 0; i < dateArray.length; i++) {
      if (dateArray[i] != null && dateArray[i].indexOf(DIVIDER) != -1) {
        String[] timeArray = dateArray[i].split(String.valueOf(DIVIDER));
        if (timeArray == null) {
          continue;
        }
        if (timeArray.length == 3) {
          if (parseHHMMSS(timeArray)) {
            break;
          }
        }
        else if (timeArray.length == 2) {
          if (parseHHMMAMPM(timeArray)) {
            break;
          }
        }
      }
    }
  }
  
  private boolean parseHHMMSS(String[] timeArray) {
    reset();
    try {
      hours = Integer.parseInt(timeArray[0]);
      minutes = Integer.parseInt(timeArray[1]);
      seconds = Integer.parseInt(timeArray[2]);
    }
    catch (NumberFormatException e) {
      reset();
      return false;
    }
    return true;
  }
  
  private boolean parseHHMMAMPM(String[] timeArray) {
    reset();
    try {
      hours = Integer.parseInt(timeArray[0]);
      minutes = Integer.parseInt(timeArray[1].substring(0,3));
    }
    catch (NumberFormatException e) {
      reset();
      return false;
    }
    String amPm = timeArray[1].substring(3,5);
    if (amPm.equals("PM")){
      hours +=12;
    }
    else if (!amPm.equals("AM")) {
      reset();
      return false;
    }
    return true;
  }

  public String toString() {
    return String.valueOf(hours) + DIVIDER + String.valueOf(minutes) + DIVIDER + String.valueOf(seconds);
  }

  /**
   * True if anotherTime equals the instance or equals the instance except for
   * an off-by-one error in seconds.
   * @param anotherTime
   * @return
   */
  public boolean almostEquals(Time anotherTime) {
    return Math.abs(getTime() - anotherTime.getTime()) <= 1;
  }
  
  /**
   * Gets total time in seconds
   * @return
   */
  private long getTime() {
    return hours * 60 + minutes * 60 + seconds * 60;
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.1  2006/06/05 18:07:09  sueh
 * <p> bug# 766 A class that stores a time string and can compare handle off-by-one
 * <p> errors in the seconds value.
 * <p> </p>
 */
