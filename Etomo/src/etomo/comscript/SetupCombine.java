package etomo.comscript;

import java.io.*;

import etomo.type.*;
import etomo.process.SystemProgram;

/**
 * <p>Description: </p>
 *
 * <p>Copyright: Copyright (c) 2002</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 * <p> $Log$
 * <p> Revision 2.1  2003/03/18 23:49:02  rickg
 * <p> Changed method name to get CombineParams reference
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.7  2003/01/06 05:51:23  rickg
 * <p> Corrected fiducial list order, it was backwards
 * <p>
 * <p> Revision 1.6  2003/01/04 00:06:08  rickg
 * <p> Swapped order for to/from fiducial mapping when
 * <p> matching from b to a.
 * <p>
 * <p> Revision 1.5  2002/10/24 23:49:53  rickg
 * <p> Added getStdout method
 * <p>
 * <p> Revision 1.4  2002/10/10 18:55:38  rickg
 * <p> Output blank line when there is no match lists
 * <p> Enabled SystemProgram debugging and remove local
 * <p> writing to stdout.
 * <p>
 * <p> Revision 1.3  2002/10/08 23:57:11  rickg
 * <p> Added remaining parameters for stdin sequence to script
 * <p>
 * <p> Revision 1.2  2002/09/30 23:45:21  rickg
 * <p> Reformatted after emacs trashed it
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
public class SetupCombine {
  public static final String rcsid =
    "$Id$";

  SystemProgram setupcombine;
  int exitValue;
  ConstMetaData metaData;

  public SetupCombine(ConstMetaData metaData) {

    this.metaData = metaData;
    ConstCombineParams combineParams = metaData.getConstCombineParams();

    //  Create a new SystemProgram object for setupcombine, set the
    //  working directory and stdin array.
    setupcombine = new SystemProgram("setupcombine");
    setupcombine.setWorkingDirectory(new File(metaData.getWorkingDirectory()));

    String[] tempStdInput = new String[15];

    //  compile the input sequence to setupcombine
    int lineCount = 0;

    //  Fileset name
    tempStdInput[lineCount++] = metaData.getFilesetName();

    //  Matching relationship
    if (combineParams.getMatchBtoA()) {
      tempStdInput[lineCount++] = "a";
      if (combineParams.getFiducialMatchListA() != "") {
        tempStdInput[lineCount++] = combineParams.getFiducialMatchListA();
        tempStdInput[lineCount++] = combineParams.getFiducialMatchListB();
      }
      else {
        tempStdInput[lineCount++] = "";
      }
    }
    else {
      tempStdInput[lineCount++] = "b";
      if (combineParams.getFiducialMatchListB() != "") {
        tempStdInput[lineCount++] = combineParams.getFiducialMatchListB();
        tempStdInput[lineCount++] = combineParams.getFiducialMatchListA();
      }
      else {
        tempStdInput[lineCount++] = "";
      }
    }

    //  Fiducial surfaces / use model
    if (combineParams.getFiducialMatch() == FiducialMatch.BOTH_SIDES) {
      tempStdInput[lineCount++] = "2";
    }

    if (combineParams.getFiducialMatch() == FiducialMatch.ONE_SIDE) {
      tempStdInput[lineCount++] = "1";
    }

    if (combineParams.getFiducialMatch() == FiducialMatch.ONE_SIDE_INVERTED) {
      tempStdInput[lineCount++] = "-1";
    }

    if (combineParams.getFiducialMatch() == FiducialMatch.USE_MODEL) {
      tempStdInput[lineCount++] = "0";
    }

    //  Patch sizes
    if (combineParams.getPatchSize() == CombinePatchSize.LARGE) {
      tempStdInput[lineCount++] = "l";
    }

    if (combineParams.getPatchSize() == CombinePatchSize.MEDIUM) {
      tempStdInput[lineCount++] = "m";
    }

    if (combineParams.getPatchSize() == CombinePatchSize.SMALL) {
      tempStdInput[lineCount++] = "s";
    }

    tempStdInput[lineCount++] = String.valueOf(combineParams.getPatchXMin());
    tempStdInput[lineCount++] = String.valueOf(combineParams.getPatchXMax());
    tempStdInput[lineCount++] = String.valueOf(combineParams.getPatchYMin());
    tempStdInput[lineCount++] = String.valueOf(combineParams.getPatchYMax());
    tempStdInput[lineCount++] = String.valueOf(combineParams.getPatchZMin());
    tempStdInput[lineCount++] = String.valueOf(combineParams.getPatchZMax());

    tempStdInput[lineCount++] = combineParams.patchRegionModel;

    tempStdInput[lineCount++] = combineParams.tempDirectory;
    if (!combineParams.tempDirectory.equals("")) {
      if (combineParams.getManualCleanup()) {
        tempStdInput[lineCount++] = "y";
      }
      else {
        tempStdInput[lineCount++] = "n";
      }
    }

    //
    //  Copy the temporary stdInput to the real stdInput to get the number
    //  of array elements correct
    String[] stdInput = new String[lineCount];
    for (int i = 0; i < lineCount; i++) {
      stdInput[i] = tempStdInput[i];
    }
    setupcombine.setStdInput(stdInput);
  }

  public int run() throws IOException {
    int exitValue;

    //  Execute the script
    setupcombine.enableDebug(true);
    setupcombine.run();
    exitValue = setupcombine.getExitValue();

    //  TODO we really need to find out what the exception/error condition was
    if (exitValue != 0) {
      throw (new IOException(setupcombine.getExceptionMessage()));
    }
    return exitValue;
  }

  public String[] getStdError() {
    return setupcombine.getStdError();
  }

  public String[] getStdOutput() {
    return setupcombine.getStdOutput();
  }

}
