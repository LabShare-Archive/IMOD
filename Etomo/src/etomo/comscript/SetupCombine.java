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
 * <p> $Log$ </p>
 */
public class SetupCombine {
  public static final String rcsid = "$Id$";

  SystemProgram setupcombine;
  int exitValue;
  ConstMetaData metaData;

  public SetupCombine(ConstMetaData metaData) {

    this.metaData = metaData;
    ConstCombineParams combineParams = metaData.getCombineParams();

    //  Create a new SystemProgram object for setupcombine, set the
    //  working directory and stdin array.
    setupcombine = new SystemProgram("setupcombine");
    setupcombine.setWorkingDirectory(new File(metaData.getWorkingDirectory()));


    String[] tempStdInput = new String[1];


    //  compile the input sequence to setupcombine
    int lineCount = 0;

    //  Fileset name
    tempStdInput[lineCount++] = metaData.getFilesetName();

    //  Matching relationship
    if(combineParams.getMatchBtoA()) {
      tempStdInput[lineCount++] = "a";
      if(combineParams.getFiducialMatchListA() != "") {
	tempStdInput[lineCount++] = combineParams.getFiducialMatchListA();
	tempStdInput[lineCount++] = combineParams.getFiducialMatchListB();
      }
    }
    else {
      tempStdInput[lineCount++] = "b";
      if(combineParams.getFiducialMatchListB() != "") {
	tempStdInput[lineCount++] = combineParams.getFiducialMatchListA();
	tempStdInput[lineCount++] = combineParams.getFiducialMatchListB();
      }

    }

    //  Fiducial surfaces / use model
    if(combineParams.getFiducialMatch() == FiducialMatch.BOTH_SIDES) {
      tempStdInput[lineCount++] = "2";
    }

    if(combineParams.getFiducialMatch() == FiducialMatch.ONE_SIDE) {
      tempStdInput[lineCount++] = "1";
    }

    if(combineParams.getFiducialMatch() == FiducialMatch.ONE_SIDE_INVERTED) {
      tempStdInput[lineCount++] = "-1";
    }

    if(combineParams.getFiducialMatch() == FiducialMatch.USE_MODEL) {
      tempStdInput[lineCount++] = "0";
    }

    //  Patch sizes
    if(combineParams.getPatchSize() == CombinePatchSize.LARGE) {
      tempStdInput[lineCount++] = "l";
    }

    if(combineParams.getPatchSize() == CombinePatchSize.MEDIUM) {
      tempStdInput[lineCount++] = "m";
    }

    if(combineParams.getPatchSize() == CombinePatchSize.SMALL) {
      tempStdInput[lineCount++] = "s";
    }

    //
    //  Copy the temporary stdInput to the real stdInput to get the number
    //  of array elements correct
    System.out.println("Stdin:");
    System.out.println(
      "------------------------------------------------------------");

    String[] stdInput = new String[lineCount];
    for(int i = 0; i < lineCount; i++) {
      stdInput[i] = tempStdInput[i];
      System.out.println(stdInput[i]);
    }
    setupcombine.setStdInput(stdInput);
  }

  public int run () throws IOException {
    int exitValue;

    //  Execute the script
    setupcombine.run();
    exitValue = setupcombine.getExitValue();

    System.out.println("Stdout:");
    System.out.println(
      "------------------------------------------------------------");
    String[] stdout = setupcombine.getStdOutput();
    for(int i=0; i < stdout.length; i++) {
      System.out.println(stdout[i]);
    }
    System.out.println("");

    System.out.println("Stderr:");
    System.out.println(
      "------------------------------------------------------------");
    String[] stderr = setupcombine.getStdError();
    for(int i=0; i < stderr.length; i++) {
      System.out.println(stderr[i]);
    }
    System.out.println("");

    //  FIXME we really need to find out what the exception/error condition was
    if(exitValue != 0) {
      throw(new IOException(setupcombine.getExceptionMessage()));
    }
      return exitValue;
  }

  public String[] getStdError() {
    return setupcombine.getStdError();
  }

}
