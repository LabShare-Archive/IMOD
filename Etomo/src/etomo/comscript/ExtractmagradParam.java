package etomo.comscript;

import java.util.ArrayList;

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.EtomoNumber;
import etomo.util.DatasetFiles;
import etomo.util.MRCHeader;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2005</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
public class ExtractmagradParam {
  public static final String rcsid = "$Id$";

  public static final String COMMAND_NAME = "extractmagrad";

  private final AxisID axisID;
  private final BaseManager manager;

  private final EtomoNumber rotationAngle = new EtomoNumber(EtomoNumber.Type.DOUBLE);

  private String gradientTable = null;
  private String[] commandArray = null;
  private EtomoNumber pixelSize = null;

  public ExtractmagradParam(BaseManager manager, AxisID axisID) {
    this.axisID = axisID;
    this.manager = manager;
  }

  public final String[] getCommand() {
    if (commandArray == null) {
      buildCommand();
    }
    return commandArray;
  }

  private final void buildCommand() {
    ArrayList command = new ArrayList();
    command.add(BaseManager.getIMODBinPath() + COMMAND_NAME);
    command.add("-rot");
    command.add(rotationAngle.toString());
    command.add("-grad");
    command.add(gradientTable);
    String dataset = manager.getName();
    command.add(DatasetFiles.getStackName(manager, axisID));
    command.add(DatasetFiles.getMagGradientName(manager, axisID));
    int commandSize = command.size();
    commandArray = new String[commandSize];
    for (int i = 0; i < commandSize; i++) {
      commandArray[i] = (String) command.get(i);
    }
  }

  public final void setGradientTable(String gradientTable) {
    this.gradientTable = gradientTable;
  }

  public final void setRotationAngle(ConstEtomoNumber rotationAngle) {
    this.rotationAngle.set(rotationAngle);
  }

  public final void setPixelSize(double pixelSize) {
    MRCHeader header = MRCHeader.getInstance(manager.getPropertyUserDir(), DatasetFiles
        .getStackName(manager, axisID), axisID);
    if (header.getXPixelSpacing() == 1) {
      if (this.pixelSize == null) {
        this.pixelSize = new EtomoNumber(EtomoNumber.Type.DOUBLE);
      }
      this.pixelSize.set(pixelSize);
    }
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.8  2011/02/24 23:34:42  sueh
 * <p> bug# 1452 imageRotation needs to be double everywhere.
 * <p>
 * <p> Revision 1.7  2011/02/21 21:26:24  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.6  2010/02/17 04:47:54  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.5  2009/03/17 00:31:44  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.4  2007/02/05 21:52:03  sueh
 * <p> bug# 962  Put EtomoNumber type info into an inner class.
 * <p>
 * <p> Revision 1.3  2006/03/20 17:51:11  sueh
 * <p> bug# 835 Added getName (a convenience function) to managers.
 * <p>
 * <p> Revision 1.2  2005/10/28 18:48:16  sueh
 * <p> bug# 725 Passing pixel size when header pixel size is 1.
 * <p>
 * <p> Revision 1.1  2005/10/27 00:12:21  sueh
 * <p> bug# 725 Param to create a extractmagrad command line.
 * <p> </p>
 */
