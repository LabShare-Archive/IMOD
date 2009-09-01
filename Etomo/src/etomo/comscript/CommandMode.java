package etomo.comscript;

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
 * <p> Revision 1.2  2007/11/06 19:06:11  sueh
 * <p> bug# 1047 Made CommandMode public.
 * <p>
 * <p> Revision 1.1  2007/02/05 21:35:41  sueh
 * <p> bug# 962 An interface for inner Mode classes.
 * <p> </p>
 */
public interface CommandMode {
  public static final String rcsid = "$Id$";
  
  public String toString();
}
