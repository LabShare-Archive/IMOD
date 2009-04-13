package etomo.process;

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
public final class FailureReason {
  public static final String rcsid = "$Id$";

  static final FailureReason UNKOWN = new FailureReason("",
      "Unable to get the load averages for this computer.");
  static final FailureReason COMPUTER_DOWN = new FailureReason("down",
      "This computer in not running.");
  static final FailureReason LOGIN_FAILED = new FailureReason("no login",
      "You must have an account on this computer.  "
          + "You must also have a passwordless login on this computer.");

  private final String reason;
  private final String tooltip;

  private FailureReason(String reason, String tooltip) {
    this.reason = reason;
    this.tooltip = tooltip;
  }

  public String getReason() {
    return reason;
  }

  public String getTooltip() {
    return tooltip;
  }

  public String toString() {
    return reason;
  }
}