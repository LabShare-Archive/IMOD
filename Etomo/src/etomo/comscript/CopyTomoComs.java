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
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.4  2002/10/10 18:54:01  rickg
 * <p> Enabled SystemProgram debugging and remove local
 * <p> writing to stdout.
 * <p>
 * <p> Revision 1.3  2002/10/09 21:35:44  rickg
 * <p> Removed stdout messages, can now be gotten from the enableDebug method in SystemProgram
 * <p>
 * <p> Revision 1.2  2002/10/09 21:19:40  rickg
 * <p> Reformat from emacs
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */

public class CopyTomoComs {
  public static final String rcsid =
    "$Id$";
  SystemProgram copytomocoms;
  int exitValue;
  ConstMetaData metaData;

  public CopyTomoComs(ConstMetaData metaData) {

    this.metaData = metaData;

    //
    //  Create a new SystemProgram object for copytomocom, set the
    //  working directory and stdin array.
    //
    copytomocoms = new SystemProgram("copytomocoms");
    copytomocoms.setWorkingDirectory(new File(metaData.getWorkingDirectory()));

    String[] tempStdInput = new String[19];

    //  compile the input sequence to copytomocoms
    int lineCount = 0;
    if (metaData.getAxisType() == AxisType.SINGLE_AXIS) {
      tempStdInput[lineCount++] = "1";
    }
    else {
      tempStdInput[lineCount++] = "2";
    }

    if (metaData.getDataSource() == DataSource.CCD) {
      tempStdInput[lineCount++] = "c";
    }
    else {
      tempStdInput[lineCount++] = "f";
    }

    if (metaData.getViewType() == ViewType.SINGLE_VIEW) {
      tempStdInput[lineCount++] = "n";
    }
    else {
      tempStdInput[lineCount++] = "y";
    }

    //  CCDEraser and local align entries
    //  Always yes tiltalign relies on local entries to save default values
    //  even if they are not used.
    tempStdInput[lineCount++] = "y";
    tempStdInput[lineCount++] = "y";

    tempStdInput[lineCount++] = metaData.getFilesetName();
    tempStdInput[lineCount++] = metaData.getBackupDirectory();

    tempStdInput[lineCount++] = String.valueOf(metaData.getPixelSize());
    tempStdInput[lineCount++] = String.valueOf(metaData.getFiducialDiameter());

    //
    // Image rotation
    //
    tempStdInput[lineCount++] = String.valueOf(metaData.getImageRotation());

    //  tilt angle data
    if (metaData.getTiltAngleSpecA().getType() == TiltAngleType.EXTRACT) {
      tempStdInput[lineCount++] = "y";
      tempStdInput[lineCount++] = "0";
    }
    else if (metaData.getTiltAngleSpecA().getType() == TiltAngleType.RANGE) {
      tempStdInput[lineCount++] = "n";
      tempStdInput[lineCount++] = "1";
      tempStdInput[lineCount++] =
        String.valueOf(
          metaData.getTiltAngleSpecA().getRangeMin()
            + ","
            + String.valueOf(metaData.getTiltAngleSpecA().getRangeStep()));
    }
    else if (metaData.getTiltAngleSpecA().getType() == TiltAngleType.FILE) {
      tempStdInput[lineCount++] = "n";
      tempStdInput[lineCount++] = "0";
    }
    else {
      //  FIXME rjg:
      tempStdInput[lineCount++] = "n";
      tempStdInput[lineCount++] = "-1";
      System.err.println(
        "Specification of all tilt alngles is not yet implemented");
    }

    //  Exclude list
    tempStdInput[lineCount++] = metaData.getExcludeProjectionsA();

    //  Second axis angle specification
    if (metaData.getAxisType() == AxisType.DUAL_AXIS) {

      tempStdInput[lineCount++] = String.valueOf(metaData.getImageRotation());

      if (metaData.getTiltAngleSpecB().getType() == TiltAngleType.EXTRACT) {
        tempStdInput[lineCount++] = "y";
        tempStdInput[lineCount++] = "0";
      }
      else if (metaData.getTiltAngleSpecB().getType() == TiltAngleType.RANGE) {
        tempStdInput[lineCount++] = "n";
        tempStdInput[lineCount++] = "1";
        tempStdInput[lineCount++] =
          String.valueOf(
            metaData.getTiltAngleSpecB().getRangeMin()
              + ","
              + String.valueOf(metaData.getTiltAngleSpecB().getRangeStep()));
      }
      else if (metaData.getTiltAngleSpecB().getType() == TiltAngleType.FILE) {
        tempStdInput[lineCount++] = "n";
        tempStdInput[lineCount++] = "0";
      }
      else {
        //  FIXME rjg:
        tempStdInput[lineCount++] = "n";
        tempStdInput[lineCount++] = "-1";
        System.err.println(
          "Specification of all tilt alngles is not yet implemented");
      }

      //  Exclude list
      tempStdInput[lineCount++] = metaData.getExcludeProjectionsB();
    }

    //  Copy the temporary stdInput to the real stdInput to get the number
    //  of array elements correct
    String[] stdInput = new String[lineCount];
    for (int i = 0; i < lineCount; i++) {
      stdInput[i] = tempStdInput[i];
    }
    copytomocoms.setStdInput(stdInput);
  }

  public int run() throws IOException {
    int exitValue;

    //  Delete the rawtilt files if extract raw tilts is selected
    checkTiltAngleFiles();

    //  Execute the script
    copytomocoms.enableDebug(true);
    copytomocoms.run();
    exitValue = copytomocoms.getExitValue();

    //  FIXME we really need to find out what the exception/error condition was
    if (exitValue != 0) {
      throw (new IOException(copytomocoms.getExceptionMessage()));
    }
    return exitValue;
  }

  public String[] getStdError() {
    return copytomocoms.getStdError();
  }

  /**
   * Check to see if the tilt angle files exist and the tilt angle type is
   * extract. They need to be deleted because the copytomocoms script is not
   * consistent in the sequence of responses expected.
   */
  private void checkTiltAngleFiles() {
    if (metaData.getAxisType() == AxisType.SINGLE_AXIS) {
      if (metaData.getTiltAngleSpecA().getType() == TiltAngleType.EXTRACT) {
        File rawTiltFile =
          new File(
            metaData.getWorkingDirectory(),
            metaData.getFilesetName() + ".rawtlt");
        if (rawTiltFile.exists()) {
          rawTiltFile.delete();
        }
      }
    }
    else {
      if (metaData.getTiltAngleSpecA().getType() == TiltAngleType.EXTRACT) {
        File rawTiltFile =
          new File(
            metaData.getWorkingDirectory(),
            metaData.getFilesetName() + "a.rawtlt");
        if (rawTiltFile.exists()) {
          rawTiltFile.delete();
        }
      }
      if (metaData.getTiltAngleSpecB().getType() == TiltAngleType.EXTRACT) {
        File rawTiltFile =
          new File(
            metaData.getWorkingDirectory(),
            metaData.getFilesetName() + "b.rawtlt");
        if (rawTiltFile.exists()) {
          rawTiltFile.delete();
        }
      }
    }
  }
}
