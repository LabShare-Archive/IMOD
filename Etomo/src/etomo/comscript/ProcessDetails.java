package etomo.comscript;

import java.util.Hashtable;

import etomo.type.ConstEtomoNumber;
import etomo.type.ConstIntKeyList;

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

  public int getIntValue(etomo.comscript.Field field);
  public float getFloatValue(etomo.comscript.Field field);
  public boolean getBooleanValue(etomo.comscript.Field field);
  public double getDoubleValue(etomo.comscript.Field field);
  public Hashtable getHashtable(etomo.comscript.Field field);
  public ConstEtomoNumber getEtomoNumber(etomo.comscript.Field field);
  public ConstIntKeyList getIntKeyList(etomo.comscript.Field field);
  public String getString(etomo.comscript.Field field);
  public String[] getStringArray(etomo.comscript.Field field);
}
/**
 * <p> $Log$
 * <p> Revision 1.7  2007/11/06 19:16:02  sueh
 * <p> bug# 1047 Added getFloatValue.
 * <p>
 * <p> Revision 1.6  2007/05/11 15:32:58  sueh
 * <p> bug# 964 Added getStringArray().
 * <p>
 * <p> Revision 1.5  2007/02/05 22:40:16  sueh
 * <p> bug# 962 Added getEtomoNumber, getIntKeyList, and getString.
 * <p>
 * <p> Revision 1.4  2006/05/11 19:48:05  sueh
 * <p> bug# 838 Add CommandDetails, which extends Command and
 * <p> ProcessDetails.  Changed ProcessDetails to only contain generic get
 * <p> functions.  Command contains all the command oriented functions.
 * <p>
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