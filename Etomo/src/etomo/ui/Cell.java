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
 * <p> $Log$
 * <p> Revision 1.1  2007/04/02 21:44:11  sueh
 * <p> bug# 964 Interface for HeaderCell and InputCell.
 * <p> </p>
 */
interface Cell {
  public static final String rcsid = "$Id$";

  public void setEnabled(boolean enable);

  /**
   * Message from row header or column header that their label has changed.
   */
 public void msgLabelChanged();
}
