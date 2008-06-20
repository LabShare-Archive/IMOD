package etomo.type;

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
public final class DebugLevel {
  public static final String rcsid = "$Id$";

  public static final DebugLevel OFF = new DebugLevel(0);
  public static final DebugLevel LOW = new DebugLevel(1);
  public static final DebugLevel HIGH = new DebugLevel(2);

  private final int level;

  private DebugLevel(int level) {
    this.level = level;
  }

  public boolean ge(DebugLevel debugLevel) {
    return level >= debugLevel.level;
  }
}
