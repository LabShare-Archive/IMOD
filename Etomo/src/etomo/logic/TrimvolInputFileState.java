package etomo.logic;

import java.io.IOException;

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.type.TomogramState;
import etomo.ui.swing.UIHarness;
import etomo.util.InvalidParameterException;
import etomo.util.MRCHeader;

/**
* <p>Description: Holds information about the input file of trimvol.  In the case of post
* processing, it also compares the input file to the post processing trimvol result and
* sets the changed variables.</p>
* 
* <p>If this is used with a non-post-processing trimvol, and new getInstance function
* should be created which doesn't call setChanged.</p>
* 
* <p>Copyright: Copyright 2011</p>
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
public final class TrimvolInputFileState {
  public static final String rcsid = "$Id$";

  private final BaseManager manager;
  private final AxisID axisID;
  private final boolean volumeFlipped;

  private boolean nColumnsChanged = false;
  private boolean nRowsChanged = false;
  private boolean nSectionsChanged = false;
  private boolean changed = false;
  private MRCHeader mrcHeader = null;
  private boolean inputFileMissing = false;

  private TrimvolInputFileState(final BaseManager manager, final AxisID axisID,
      final boolean volumeFlipped) {
    this.manager = manager;
    this.axisID = axisID;
    this.volumeFlipped = volumeFlipped;
  }

  /**
   * Gets an instance of this class for post-processing.  Assumes that the trimvol input
   * file is flipped.  If the file exists, sets changed member variables.
   * @param manager
   * @param axisID
   * @param inputFileName
   * @param state
   * @return
   * @throws IOException
   * @throws InvalidParameterException
   */
  public static TrimvolInputFileState getPostProcessingInstance(
      final BaseManager manager, final AxisID axisID, final String inputFileName,
      final TomogramState state) throws IOException, InvalidParameterException {
    TrimvolInputFileState instance = new TrimvolInputFileState(manager, axisID, true);
    if (instance.initMrcHeader(inputFileName)) {
      instance.setChanged(state);
    }
    else {
      instance.inputFileMissing = true;
      UIHarness.INSTANCE.openMessageDialog(manager, inputFileName + " does not exist.",
          "Missing Input File", axisID);
    }
    return instance;
  }

  /**
   * Returns false when files is not found.
   * @param inputFileName
   * @return
   * @throws IOException
   * @throws InvalidParameterException
   */
  private boolean initMrcHeader(final String inputFileName) throws IOException,
      InvalidParameterException {
    mrcHeader = MRCHeader.getInstance(manager.getPropertyUserDir(), inputFileName,
        AxisID.ONLY);
    if (!mrcHeader.read(manager)) {
      return false;
    }
    return true;
  }

  /**
   * Compares the trimvol input file to the last post-processing trimvol input file.
   * @param state
   */
  private void setChanged(final TomogramState state) {
    if (!state.isPostProcTrimVolInputNColumnsNull()
        && mrcHeader.getNColumns() != state.getPostProcTrimVolInputNColumns()) {
      changed = true;
      nColumnsChanged = true;
    }
    if (!state.isPostProcTrimVolInputNSectionsNull()
        && getNRows() != state.getPostProcTrimVolInputNSections()) {
      changed = true;
      nRowsChanged = true;
    }
    if (!state.isPostProcTrimVolInputNRowsNull()
        && getNSections() != state.getPostProcTrimVolInputNRows()) {
      changed = true;
      nSectionsChanged = true;
    }
  }

  public boolean isInputFileMissing() {
    return inputFileMissing;
  }

  public boolean isChanged() {
    return changed;
  }

  public boolean isNColumnsChanged() {
    return nColumnsChanged;
  }

  public boolean isNRowsChanged() {
    return nRowsChanged;
  }

  public boolean isNSectionsChanged() {
    return nSectionsChanged;
  }

  public int getNColumns() {
    if (mrcHeader != null) {
      return mrcHeader.getNColumns();
    }
    return -1;
  }

  /**
   * Returns the number of rows, taking whether the volume is flipped into account.
   * @return
   */
  public int getNRows() {
    if (mrcHeader != null) {
      if (volumeFlipped) {
        return mrcHeader.getNSections();
      }
      return mrcHeader.getNRows();
    }
    return -1;
  }

  /**
   * Returns the number of sections, taking whether the volume is flipped into account.
   * @return
   */
  public int getNSections() {
    if (mrcHeader != null) {
      if (volumeFlipped) {
        return mrcHeader.getNRows();
      }
      return mrcHeader.getNSections();
    }
    return -1;
  }

  public boolean isVolumeFlipped() {
    return volumeFlipped;
  }
}
