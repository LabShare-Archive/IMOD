package etomo.comscript;

import java.util.ArrayList;

/*
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
 * <p> Revision 2.7  2003/10/28 18:46:59  sueh
 * <p> removing prints
 * <p>
 * <p> Revision 2.6  2003/10/02 18:57:46  sueh
 * <p> bug236 added testing:
 * <p> NewstParamTest
 * <p> ComScriptTest
 * <p>
 * <p> Removed marks
 * <p>
 * <p> Revision 2.5  2003/09/29 23:34:57  sueh
 * <p> bug236 Added UseLinearInterpolation to
 * <p> TomogramGenerationDialog.
 * <p>
 * <p> UseLinearInterpolation:
 * <p> check box
 * <p> Advanced
 * <p> newst -linear
 * <p>
 * <p> Files:
 * <p> ComScriptManager.java
 * <p> ConstNewstParam.java
 * <p> NewstParam.java
 * <p> TomogramGenerationDialog.java
 * <p> ApplicationManager.java
 * <p>
 * <p> Revision 2.4  2003/07/25 22:54:14  rickg
 * <p> CommandParam method name changes
 * <p>
 * <p> Revision 2.3  2003/06/25 22:16:29  rickg
 * <p> changed name of com script parse method to parseComScript
 * <p>
 * <p> Revision 2.2  2003/03/20 17:23:37  rickg
 * <p> Comment update
 * <p>
 * <p> Revision 2.1  2003/03/02 23:30:41  rickg
 * <p> Combine layout in progress
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.1.2.1  2003/01/24 18:33:42  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
public class NewstParam extends ConstNewstParam implements CommandParam {
  public static final String rcsid =
    "$Id$";

  /**
   * Get the parameters from the ComScriptCommand
   * @param scriptCommand the ComScriptCommand containg the newst command
   * and parameters.
   */
  public void parseComScriptCommand(ComScriptCommand scriptCommand) {
    // TODO error checking - throw exceptions for bad syntax
    String[] cmdLineArgs = scriptCommand.getCommandLineArgs();

    reset();

    for (int i = 0; i < cmdLineArgs.length - 2; i++) {
      if (cmdLineArgs[i].startsWith("-si")) {
        i++;
        size = cmdLineArgs[i];
        useSize = true;
      }
      if (cmdLineArgs[i].startsWith("-o")) {
        i++;
        offset = cmdLineArgs[i];
        useOffset = true;
      }
      if (cmdLineArgs[i].startsWith("-x")) {
        i++;
        transformFile = cmdLineArgs[i];
        useTransformFile = true;
      }
      if (cmdLineArgs[i].startsWith("-l")) {
				useLinearInterpolation = true;
      }
    }
    inputFile = cmdLineArgs[cmdLineArgs.length - 2];
    outputFile = cmdLineArgs[cmdLineArgs.length - 1];
  }

  /**
   * Update the script command with the current valus of this NewstParam
   * object
   * @param scriptCommand the script command to be updated
   */
  public void updateComScriptCommand(ComScriptCommand scriptCommand)
    throws BadComScriptException {
    // Create a new command line argument array

    ArrayList cmdLineArgs = new ArrayList(20);

    if (useSize) {
      cmdLineArgs.add("-size");
      cmdLineArgs.add(size.toString());
    }

    if (useOffset) {
      cmdLineArgs.add("-offset");
      cmdLineArgs.add(offset.toString());
    }

    if (useTransformFile) {
      cmdLineArgs.add("-xform");
      cmdLineArgs.add(transformFile);
    }
    
    if (useLinearInterpolation) {
    	cmdLineArgs.add("-linear");
    }

    cmdLineArgs.add(inputFile);
    cmdLineArgs.add(outputFile);

    int nArgs = cmdLineArgs.size();
    scriptCommand.setCommandLineArgs(
      (String[]) cmdLineArgs.toArray(new String[nArgs]));
  }

  public void setInputFile(String filename) {
    inputFile = filename;
  }

  public void setOutputFile(String filename) {
    outputFile = filename;
  }

  public void setTransformFile(String filename) {
    transformFile = filename;
    useTransformFile = true;
  }

  public void setSize(String newSize) throws FortranInputSyntaxException {
    size = newSize;
    useSize = true;
  }

  public void setOffset(String newOffset) throws FortranInputSyntaxException {
    offset = newOffset;
    useOffset = true;
  }


	public void setUseLinearInterpolation(boolean b) {
		useLinearInterpolation = b;
	}

  private void reset() {
    inputFile = "";
    outputFile = "";

    useTransformFile = false;
    transformFile = "";

    useSize = false;
    size = "";

    useOffset = false;
    offset = "";
    
		useLinearInterpolation = false;
  }
}
