package etomo.ui;

/**
 * <p>Description: </p>
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
 * 
 * <p> $Log$ </p>
 */
public final class Run3dmodProcess {
  public static final String rcsid = "$Id$";

  private String key = null;

  public boolean equals(String string) {
    if (string.equals(key)) {
      return true;
    }
    return false;
  }

  void setKey(final String input) {
    key = input;
  }
}
