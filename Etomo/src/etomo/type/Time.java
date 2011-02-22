package etomo.type;

/**
 * <p>Description:
 * Represents hours, minutes, and seconds.  Can pull hours, minutes, and seconds
 * out of a longer string.  Can compare times with almost equals, which allows
 * off-by-one errors in seconds.
 * 
 * Formats parsed:
 * hours:minutes:seconds
 * hours:minutesAM/PM</p>
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
public final class Time {
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
   * Finds the first instance of a substring with the format
   * hours:minutes:seconds or hours:minutesAM/PM in the date parameter and uses
   * it to set the member variables.
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
          if (parseHoursMinutesSeconds(timeArray)) {
            break;
          }
        }
        else if (timeArray.length == 2) {
          if (parseHoursMinutesAMPM(timeArray)) {
            break;
          }
        }
      }
    }
  }

  private boolean parseHoursMinutesSeconds(String[] timeArray) {
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

  private boolean parseHoursMinutesAMPM(String[] timeArray) {
    reset();
    try {
      hours = Integer.parseInt(timeArray[0]);
    }
    catch (NumberFormatException e) {
      reset();
      return false;
    }
    int amPmStartIndex = timeArray[1].length() - 2;
    String amPm = timeArray[1].substring(amPmStartIndex, timeArray[1].length());
    if (amPm.equals("PM")) {
      hours += 12;
    }
    else if (!amPm.equals("AM")) {
      reset();
      return false;
    }
    minutes = Integer.parseInt(timeArray[1].substring(0, amPmStartIndex));
    return true;
  }

  public String toString() {
    return String.valueOf(hours) + DIVIDER + String.valueOf(minutes) + DIVIDER
        + String.valueOf(seconds);
  }

  /**
   * True if anotherTime equals the instance give or take one minute.
   * @param anotherTime
   * @return
   */
  public boolean almostEquals(Time anotherTime) {
    return Math.abs(getTime() - anotherTime.getTime()) <= 3600;
  }

  /**
   * Gets total time in milliseconds
   * @return
   */
  private long getTime() {
    return hours * 60 + minutes * 60 + seconds * 60;
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.5  2010/02/05 00:46:55  sueh
 * <p> bug# 1309 In almostEquals allowing up to 1 minute of difference.
 * <p>
 * <p> Revision 1.4  2006/08/01 20:06:32  sueh
 * <p> Fixed almostEquals - difference should be compared with 60
 * <p>
 * <p> Revision 1.3  2006/06/07 21:05:26  sueh
 * <p> bug# 766 Fixed parseHoursMinutesAMPM
 * <p>
 * <p> Revision 1.2  2006/06/07 20:38:36  sueh
 * <p> bug# 766 Added HH:MMAM time format.  Fix almostEquals to add up all the
 * <p> seconds before comparing.
 * <p>
 * <p> Revision 1.1  2006/06/05 18:07:09  sueh
 * <p> bug# 766 A class that stores a time string and can compare handle off-by-one
 * <p> errors in the seconds value.
 * <p> </p>
 */
