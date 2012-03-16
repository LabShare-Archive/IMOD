package etomo.type;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2010</p>
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
public final class ToolType {
  public static final String rcsid = "$Id$";

  public static final ToolType FLATTEN_VOLUME = new ToolType("Flatten Volume");
  public static final ToolType GPU_TILT_TEST = new ToolType("GPU Test");

  private final String string;

  private ToolType(String string) {
    this.string = string;
  }

  public String toString() {
    return string;
  }
}
