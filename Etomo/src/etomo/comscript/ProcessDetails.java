package etomo.comscript;

import java.io.File;
import java.util.Hashtable;

import etomo.type.AxisID;

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
public interface ProcessDetails extends Command {
  public static final String rcsid = "$Id$";

  public int getCommandMode();
  public File getCommandOutputFile();
  public int getIntValue(etomo.comscript.Fields field);
  public boolean getBooleanValue(etomo.comscript.Fields field);
  public double getDoubleValue(etomo.comscript.Fields field);
  public AxisID getAxisID();
  public Hashtable getHashtable(etomo.comscript.Fields field);
}
/**
 * <p> $Log$
 * <p> Revision 1.2  2006/01/20 20:47:57  sueh
 * <p> updated copyright year
 * <p>
 * <p> Revision 1.1  2005/11/14 21:22:27  sueh
 * <p> bug# 744 An interface for params that are used in post processing.
 * <p> Command can be used for params which need to be passed to the
 * <p> process but aren't used in post processing.
 * <p> </p>
 */