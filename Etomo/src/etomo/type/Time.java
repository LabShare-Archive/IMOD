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
public final class Time {
  public static final String rcsid = "$Id$";

  private static final char DIVIDER = ':';
  private String hours = null;
  private String minutes = null;
  private String seconds = null;

  public Time(String date) {
    set(date);
  }

  private void reset() {
    hours = null;
    minutes = null;
    seconds = null;
  }

  /**
   * Finds a string of the format HH:MM:SS in the date parameter and uses it to
   * set its member variables.
   * @param date
   */
  public void set(String date) {
    reset();
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
        if (timeArray == null || timeArray.length != 3) {
          continue;
        }
        try {
          Integer.parseInt(timeArray[0]);
          Integer.parseInt(timeArray[1]);
          Integer.parseInt(timeArray[2]);
        }
        catch (NumberFormatException e) {
          continue;
        }
        hours = timeArray[0].trim();
        minutes = timeArray[1].trim();
        seconds = timeArray[2].trim();
      }
    }
  }

  public String toString() {
    return hours + DIVIDER + minutes + DIVIDER + seconds;
  }

  private int getSeconds() {
    if (seconds == null) {
      return 0;
    }
    return Integer.parseInt(seconds);
  }

  /**
   * True if anotherTime equals the instance or equals the instance except for
   * an off-by-one error in seconds.
   * @param anotherTime
   * @return
   */
  public boolean almostEquals(Time anotherTime) {
    if (!hours.equals(anotherTime.hours)
        || !minutes.equals(anotherTime.minutes)) {
      return false;
    }
    int iSeconds = getSeconds();
    int anotherTimeSeconds = anotherTime.getSeconds();
    return iSeconds == anotherTimeSeconds || iSeconds == anotherTimeSeconds + 1
        || iSeconds == anotherTimeSeconds - 1;
  }
}
/**
 * <p> $Log$ </p>
 */
