package etomo.comscript;

import java.util.Hashtable;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2005 - 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
public interface ProcessDetails {
  public static final String rcsid = "$Id$";

  public int getIntValue(etomo.comscript.Fields field);
  public boolean getBooleanValue(etomo.comscript.Fields field);
  public double getDoubleValue(etomo.comscript.Fields field);
  public Hashtable getHashtable(etomo.comscript.Fields field);
}
/**
 * <p> $Log$
 * <p> Revision 1.3  2006/04/06 19:36:31  sueh
 * <p> bug# 808 Passing Fields to generic get classes.  Fields is an interface for
 * <p> inner enum-type classes.  This is more reliable then using integers to
 * <p> symbolize different fields.
 * <p>
 * <p> Revision 1.2  2006/01/20 20:47:57  sueh
 * <p> updated copyright year
 * <p>
 * <p> Revision 1.1  2005/11/14 21:22:27  sueh
 * <p> bug# 744 An interface for params that are used in post processing.
 * <p> Command can be used for params which need to be passed to the
 * <p> process but aren't used in post processing.
 * <p> </p>
 */