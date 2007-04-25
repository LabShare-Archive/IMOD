package etomo.storage.autodoc;

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
public abstract class WritableStatement implements ReadOnlyStatement {
  public static final String rcsid = "$Id$";

  abstract WritableStatement remove();

  public abstract Statement.Type getType();

  public abstract String getString();

  public abstract int sizeLeftSide();

  public abstract String getLeftSide(int index);

  public abstract String getRightSide();
}
