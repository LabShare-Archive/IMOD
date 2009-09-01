package etomo.comscript;

import etomo.storage.Storable;
import etomo.type.ConstEtomoNumber;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2002 - 2006</p>
 *
 *<p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.20  2009/06/05 01:45:43  sueh
 * <p> bug# 1219 Changed class to an interface.  Moved functionality to
 * <p> SqueezevolParam.
 * <p>
 * <p> Revision 1.19  2007/12/13 01:03:41  sueh
 * <p> bug# 1056 Changed etomo.comscript.Fields to etomo.comscript.FieldInterface.
 * <p>
 * <p> Revision 1.18  2007/11/06 19:07:16  sueh
 * <p> bug# 1047 Added getSubcommandDetails.
 * <p>
 * <p> Revision 1.17  2007/05/11 15:25:57  sueh
 * <p> bug# 964 Added getStringArray().
 * <p>
 * <p> Revision 1.16  2007/02/05 21:40:58  sueh
 * <p> bug# 962  Put EtomoNumber type info into an inner class.
 * <p>
 * <p> Revision 1.15  2006/05/22 22:36:01  sueh
 * <p> bug# 577 Added getCommand().
 * <p>
 * <p> Revision 1.14  2006/05/11 19:39:30  sueh
 * <p> bug# 838 Add CommandDetails, which extends Command and
 * <p> ProcessDetails.  Changed ProcessDetails to only contain generic get
 * <p> functions.  Command contains all the command oriented functions.
 * <p>
 * <p> Revision 1.13  2006/04/06 18:49:18  sueh
 * <p> bug# 808 Implementing ProcessDetails.  Added Fields to pass requests to
 * <p> the generic gets.
 * <p>
 * <p> Revision 1.12  2006/01/20 20:46:08  sueh
 * <p> updated copyright year
 * <p>
 * <p> Revision 1.11  2005/11/19 01:51:54  sueh
 * <p> bug# 744 Moved functions only used by process manager post
 * <p> processing and error processing from Commands to ProcessDetails.
 * <p> This allows ProcesschunksParam to be passed to DetackedProcess
 * <p> without having to add unnecessary functions to it.
 * <p>
 * <p> Revision 1.10  2005/10/27 00:10:07  sueh
 * <p> bug# 708 Squeezevol gets it input file only from TrimvolParam's output
 * <p> file.
 * <p>
 * <p> Revision 1.9  2005/07/29 00:44:36  sueh
 * <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> because the current manager changes when the user changes the tab.
 * <p> Passing the manager where its needed.
 * <p>
 * <p> Revision 1.8  2005/07/26 17:27:25  sueh
 * <p> bug# 701 Get the PID from squeezevol
 * <p>
 * <p> Revision 1.7  2005/04/25 20:38:57  sueh
 * <p> bug# 615 Passing the axis where a command originates to the message
 * <p> functions so that the message will be popped up in the correct window.
 * <p> This requires adding AxisID to many objects.
 * <p>
 * <p> Revision 1.6  2005/01/25 21:27:05  sueh
 * <p> Renaming EtomoNumber.resetValue to displayValue.
 * <p>
 * <p> Revision 1.5  2005/01/08 01:33:40  sueh
 * <p> bug# 578 Changed the names of the statics used to make variables
 * <p> available in the Command interface.  Add GET_.
 * <p>
 * <p> Revision 1.4  2004/12/16 02:12:28  sueh
 * <p> bug# 564 Implemented Command.  Saved flipped status.
 * <p>
 * <p> Revision 1.3  2004/12/14 21:32:29  sueh
 * <p> bug# 557 Made separate variables for x and y reduction factors to handle
 * <p> an unflipped tomogram.
 * <p>
 * <p> Revision 1.2  2004/12/08 21:19:54  sueh
 * <p> bug# 564 Changed TrimvolParam set and get, input and output File
 * <p> functions to ...FileName to avoid confusion with the new getOutputFile()
 * <p> function.
 * <p>
 * <p> Revision 1.1  2004/12/02 18:24:23  sueh
 * <p> bug# 557 Manages squeezevol parameters.  Creates command line.
 * <p> Stores parameters.
 * <p> </p>
 */
public interface ConstSqueezevolParam extends  CommandDetails, Storable{
  public static final String rcsid = "$Id$";

  public ConstEtomoNumber getReductionFactorX();

  public ConstEtomoNumber getReductionFactorY();

  public ConstEtomoNumber getReductionFactorZ();

  public boolean isLinearInterpolation();
}
