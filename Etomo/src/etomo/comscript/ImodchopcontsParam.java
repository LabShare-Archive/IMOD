package etomo.comscript;

import java.io.IOException;

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.type.EtomoNumber;
import etomo.type.FileType;
import etomo.type.ScriptParameter;
import etomo.util.MRCHeader;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2013</p>
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
public final class ImodchopcontsParam implements CommandParam {
  public static final String rcsid = "$Id:$";

  public static final String LENGTH_OF_PIECES_KEY = "LengthOfPieces";
  public static final String MINIMUM_OVERLAP_KEY = "MinimumOverlap";
  private static final int LENGTH_OF_PIECES_DEFAULT = -1;
  public static final String GOTO_LABEL = "dochop";

  private final ScriptParameter lengthOfPieces = new ScriptParameter(LENGTH_OF_PIECES_KEY);
  private final ScriptParameter minimumOverlap = new ScriptParameter(MINIMUM_OVERLAP_KEY);

  public ImodchopcontsParam() {
    minimumOverlap.setDisplayValue(4);
  }

  /**
   * @deprecated
   * @return
   */
  public static ImodchopcontsParam getBackwardCompatableInstance(
      final TiltxcorrParam tiltxcorrParam) {
    ImodchopcontsParam imodchopcontsParam = new ImodchopcontsParam();
    imodchopcontsParam.lengthOfPieces.set(tiltxcorrParam.getLengthFromLengthAndOverlap());
    imodchopcontsParam.minimumOverlap
        .set(tiltxcorrParam.getOverlapFromLengthAndOverlap());
    return imodchopcontsParam;
  }

  private void reset() {
    lengthOfPieces.reset();
    minimumOverlap.reset();
  }

  public void parseComScriptCommand(ComScriptCommand scriptCommand)
      throws BadComScriptException, InvalidParameterException,
      FortranInputSyntaxException {
    reset();
    lengthOfPieces.parse(scriptCommand);
    minimumOverlap.parse(scriptCommand);
  }

  public void updateComScriptCommand(ComScriptCommand scriptCommand)
      throws BadComScriptException {
    lengthOfPieces.updateComScript(scriptCommand);
    minimumOverlap.updateComScript(scriptCommand);
  }

  public void initializeDefaults() {
  }

  public String getLengthOfPieces() {
    if (!lengthOfPieces.equals(LENGTH_OF_PIECES_DEFAULT)) {
      return lengthOfPieces.toString();
    }
    return null;
  }

  public boolean isLengthOfPiecesDefault() {
    return lengthOfPieces.equals(LENGTH_OF_PIECES_DEFAULT);
  }

  public boolean isLengthOfPiecesNull() {
    return lengthOfPieces.isNull();
  }

  public void resetLengthOfPieces() {
    lengthOfPieces.reset();
  }

  public void setLengthOfPiecesDefault() {
    lengthOfPieces.set(LENGTH_OF_PIECES_DEFAULT);
  }

  public void setLengthOfPieces(final String input) {
    lengthOfPieces.set(input);
  }

  public String getMinimumOverlap() {
    return minimumOverlap.toString();
  }

  public void setMinimumOverlap(final String input) {
    minimumOverlap.set(input);
  }

  /**
   * For patch tracking of the prealigned stack.  Returns the default values of
   * LengthAndOverlap.
   * @param manager
   * @param axisID
   * @return
   */
  public static String getLengthOfPiecesDefault(final BaseManager manager,
      final AxisID axisID, final FileType fileType) {
    EtomoNumber length = new EtomoNumber();
    int floor = 16;
    length.setFloor(floor);
    length.setDisplayValue(floor);
    MRCHeader header = MRCHeader.getInstance(manager.getPropertyUserDir(),
        fileType.getFileName(manager, axisID), axisID);
    try {
      header.read(manager);
      int z = header.getNSections();
      if (z != -1) {
        length.set(Math.round((z / 5)));
      }
    }
    catch (IOException e) {
      e.printStackTrace();
    }
    catch (etomo.util.InvalidParameterException e) {
      e.printStackTrace();
    }
    return length.toString();// + ",4";
  }
}
