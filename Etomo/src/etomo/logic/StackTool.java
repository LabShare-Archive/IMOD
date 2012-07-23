package etomo.logic;

import java.io.IOException;

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.type.ViewType;
import etomo.ui.swing.UIHarness;
import etomo.util.InvalidParameterException;
import etomo.util.MRCHeader;
import etomo.util.Montagesize;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2012</p>
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
public final class StackTool {
  public static final String rcsid = "$Id:$";

  private StackTool() {
  }

  /**
   * Pops up an error message and returns false if the view type doesn't match the stack
   * type.
   * @param manager
   * @param axisID
   * @param viewType
   * @param stackFileName
   * @return
   */
  public static boolean validateViewType(final ViewType viewType,
      final String absolutePath, final String stackFileName, final BaseManager manager,
      final AxisID axisID) {
    if (stackFileName == null) {
      return true;
    }
    Montagesize montagesize = Montagesize
        .getInstance(absolutePath, stackFileName, axisID);
    // Run montagesize without the piece list file to see what the stack looks like.
    montagesize.setIgnorePieceListFile(true);
    int exitValue = readMontagesize(montagesize, manager);
    if (!montagesize.pieceListFileExists()) {
      if (exitValue == 0) {
        return validateMontage(viewType, montagesize, absolutePath, stackFileName,
            manager, axisID);
      }
      else if (exitValue == 1) {
        // Its single frame.
        if (viewType == ViewType.MONTAGE) {
          UIHarness.INSTANCE.openMessageDialog(manager,
              "The dataset is not a montage.  Please select single frame type.",
              "Incorrect Frame Type", axisID);
          return false;
        }
      }
    }
    // Ignored existing piece list file.
    else if (exitValue == 0) {
      return validateMontage(viewType, montagesize, absolutePath, stackFileName, manager,
          axisID);
    }
    else if (exitValue == 1) {
      // No piece list information available in the stack - run montagesize with with
      montagesize.setIgnorePieceListFile(false);
      exitValue = readMontagesize(montagesize, manager);
      if (exitValue == 0) {
        return validateMontage(viewType, montagesize, absolutePath, stackFileName,
            manager, axisID);
      }
      else if (exitValue == 2 || exitValue == 3) {
        // If they selected single view, go with that and ignore the piece list file
        if (viewType == ViewType.MONTAGE) {
          UIHarness.INSTANCE.openMessageDialog(manager,
              "The piece list file associated with this dataset does not match and the "
                  + "stack does not contain piece list information.  Please select "
                  + "single frame type.", "Incorrect Frame Type", axisID);
          return false;
        }
      }
    }
    return true;
  }
  
  private static int readMontagesize(final Montagesize montagesize, final BaseManager manager) {
    int exitValue;
    try {
      montagesize.read(manager);
      return montagesize.getExitValue();
    }
    catch (InvalidParameterException e) {
      exitValue = montagesize.getExitValue();
      if (montagesize.getExitValue() == 0) {
        return 1;
      }
      return exitValue;
    }
    catch (IOException e) {
      exitValue = montagesize.getExitValue();
      if (montagesize.getExitValue() == 0) {
        return 1;
      }
      return exitValue;
    }
  }

  private static boolean validateMontage(final ViewType viewType,
      final Montagesize montagesize, final String absolutePath,
      final String stackFileName, final BaseManager manager, final AxisID axisID) {
    if (viewType != ViewType.MONTAGE) {
      // Currently 1x1 montage works with single view, so only fail if X or Y are
      // different.
      MRCHeader header = MRCHeader.getInstance(absolutePath, stackFileName, axisID);
      try {
        if (!header.read(manager)) {
          UIHarness.INSTANCE.openMessageDialog(manager, "File does not exist.",
              "Entry Error", axisID);
          return false;
        }
      }
      catch (InvalidParameterException except) {
        UIHarness.INSTANCE.openMessageDialog(manager, except.getMessage(),
            "Invalid Parameter Exception", axisID);
        return false;
      }
      catch (IOException except) {
        UIHarness.INSTANCE.openMessageDialog(manager, except.getMessage(),
            "IO Exception", axisID);
        return false;
      }
      if (montagesize.getX().getInt() > header.getNColumns()
          || montagesize.getY().getInt() > header.getNRows()) {
        UIHarness.INSTANCE.openMessageDialog(manager,
            "The dataset is a montage.  Please select montage frame type.", "Incorrect Frame Type",
            axisID);
        return false;
      }
    }
    return true;
  }
}
