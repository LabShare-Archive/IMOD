package etomo.comscript;

import java.util.Hashtable;

import etomo.storage.Loggable;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstIntKeyList;
import etomo.type.IteratorElementList;

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
public interface ProcessDetails extends Loggable{
  public static final String rcsid = "$Id$";

  public int getIntValue(FieldInterface field);

  public boolean getBooleanValue(FieldInterface field);

  public double getDoubleValue(FieldInterface field);

  public Hashtable getHashtable(FieldInterface field);

  public ConstEtomoNumber getEtomoNumber(FieldInterface field);

  public ConstIntKeyList getIntKeyList(FieldInterface field);

  public String getString(FieldInterface field);

  public String[] getStringArray(FieldInterface field);
  
  public IteratorElementList getIteratorElementList(FieldInterface field);
}
/**
 * <p> $Log$
 * <p> Revision 1.10  2009/09/05 00:35:39  sueh
 * <p> bug# 1256 Added blank getIteratorElementList.
 * <p>
 * <p> Revision 1.9  2009/09/01 03:17:46  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.8  2007/12/13 01:06:08  sueh
 * <p> bug# 1056 Changed etomo.comscript.Fields to etomo.comscript.FieldInterface.
 * <p>
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
