package etomo.process;

import etomo.type.AxisID;
import etomo.type.MetaData;
import etomo.ApplicationManager;
import etomo.type.ConstMetaData;
import etomo.comscript.CopyTomoComs;
import etomo.comscript.BadComScriptException;
import etomo.comscript.ConstCombineParams;
import etomo.comscript.SetupCombine;

import java.io.File;
import java.io.IOException;

import javax.swing.JOptionPane;

/**
 * <p>Description: This object manages the execution of com scripts in the
 * background and the opening and sending messages to imod.</p>
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

public class ProcessManager{
  public static final String rcsid = "$Id$";

  ApplicationManager appManager;

  public ProcessManager(ApplicationManager appMgr) {
    appManager = appMgr;
  }


  /**
   * Run the copytomocoms script
   * @param metaData a read-only MetaData object containing the information
   * to run the copytomocoms script
   */
  public void setupComScripts(ConstMetaData metaData)
    throws BadComScriptException, IOException {

    CopyTomoComs copyTomoComs = new CopyTomoComs(metaData);

    int exitValue = copyTomoComs.run();

    if(exitValue != 0) {
      System.out.println("Exit value: " + String.valueOf(exitValue));

      //  Compile the exception message from the stderr stream
      String[] stdError = copyTomoComs.getStdError();
      if(stdError.length < 1) {
	stdError = new String[1];
	stdError[0] =
	  "Get David to add some std error reporting to copytomocoms";
      }
      StringBuffer buffer = new StringBuffer();
      buffer.append("Copytomocoms Error\n");
      buffer.append("Standard error output:\n");
      for(int i = 0; i < stdError.length; i++) {
	buffer.append(stdError[i]);
	buffer.append("\n");
      }

      throw(new BadComScriptException(buffer.toString()));
    }
  }

  /**
   * Erase the specified pixels
   * @param axisID the AxisID to cross-correlate.
   */
  public void eraser(AxisID axisID) {

    //  Create the required command string
    String command = "eraser" + axisID.getExtension() + ".com";

    //  Start the com script in the background
    startComScript(command);
  }

  /**
   * Open imod on the raw stack with the er
   * @param axisID the AxisID to coarse align.
   */
  public void imodErase(AxisID axisID) {
    String extension = "";
    String filesetName = appManager.getFilesetName();
    String axisLabel = axisID.getExtension();

    //  Create the required imod command
    String command  = "imod " + filesetName + axisLabel + ".st "
      + filesetName + axisLabel + ".erase";

    //  Start the program thread
    startSystemProgramThread(command);
  }

  /**
   * Calculate the cross-correlation for the specified axis
   * @param axisID the AxisID to cross-correlate.
   */
  public void crossCorrelate(AxisID axisID) {

    //  Create the required command string
    String command = "xcorr" + axisID.getExtension() + ".com";

    //  Start the com script in the background
    startComScript(command);
  }

  /**
   * Calculate the coarse alignment for the specified axis
   * @param axisID the identifyer of the axis to coarse align.
   */
  public void coarseAlign(AxisID axisID) {

    //  Create the required tiltalign command
    String command = "prenewst" + axisID.getExtension() + ".com";

    //  Start the com script in the background
    startComScript(command);
  }

  /**
   * Open imod on the coarse aligned stack for the selected axis
   * @param axisID the AxisID to coarse align.
   */
  public void imodAlign(AxisID axisID) {
    String extension = "";

    //  Create the required imod command
    if(axisID == AxisID.ONLY) {
      extension = ".preali";
    }
    if(axisID == AxisID.FIRST) {
      extension = "a.preali";
    }
    if(axisID == AxisID.SECOND) {
      extension = "b.preali";
    }
    String command  = "imod " + appManager.getFilesetName() + extension;

    //  Start the program thread
    startSystemProgramThread(command);
  }

  /**
   * Run midas on the specified raw stack
   * @param axisID the AxisID to run midas on.
   */
  public void midasRawStack(AxisID axisID) {
    String stack = "";
    String xform = "";

    //  Create the required midas command
    if(axisID == AxisID.ONLY) {
      stack = appManager.getFilesetName() + ".st ";
      xform = appManager.getFilesetName() + ".prexf ";
    }
    if(axisID == AxisID.FIRST) {
      stack = appManager.getFilesetName() + "a.st ";
      xform = appManager.getFilesetName() + "a.prexf ";
    }
    if(axisID == AxisID.SECOND) {
      stack = appManager.getFilesetName() + "b.st ";
      xform = appManager.getFilesetName() + "b.prexf ";
    }

    String command  = "midas " + stack + xform;

    //  Start the system program thread
    startSystemProgramThread(command);
  }


  /**
   * Open imod to create the seed model for the specified axis
   * @param axisID the AxisID to create a seed model for.
   */
  public void imodSeedFiducials(AxisID axisID) {
    String filesetName = appManager.getFilesetName();
    String axisExt = axisID.getExtension();

    String command  = "imod "
      + filesetName + axisExt + ".preali "
      + filesetName + axisExt + ".seed";

    //  Start the system program thread
    startSystemProgramThread(command);
  }


   /**
   * Run the appropriate track com file for the given axis ID
   * @param axisID the AxisID to run track.com on.
   */
  public void fiducialModelTrack(AxisID axisID) {
    //
    //  Create the required beadtrack command
    //
    String command = "track" + axisID.getExtension() + ".com";

    //  Start the com script in the background
    startComScript(command);
  }


  /**
   * Open imod to fix the fiducial model for the specified axis
   * @param axisID the AxisID to run imod on.
   */
  public void imodFixFiducials(AxisID axisID) {
    String filesetName = appManager.getFilesetName();
    String axisExt = axisID.getExtension();
    String command  = "imod "
      + filesetName + axisExt + ".preali "
      + filesetName + axisExt + ".fid";

    //  Start the system program thread
    startSystemProgramThread(command);
  }


  /**
   * Run the appropriate align com file for the given axis ID
   * @param axisID the AxisID to run align.com on.
   */
  public void fineAlignment(AxisID axisID) {
    //
    //  Create the required tiltalign command
    //
    String command = "align" + axisID.getExtension() + ".com";

    //  Start the com script in the background
    startComScript(command);
  }


  /**
   * Open imod on the coarse aligned stack for the selected axis
   * @param axisID the AxisID to coarse align.
   */
  public void imodFineAlign(AxisID axisID) {
    String extension = "";
    //
    //  Create the required imod command
    //
    if(axisID == AxisID.ONLY) {
      extension = ".ali";
    }
    if(axisID == AxisID.FIRST) {
      extension = "a.ali";
    }
    if(axisID == AxisID.SECOND) {
      extension = "b.ali";
    }

    String command  = "imod " + appManager.getFilesetName() + extension;

    //  Start the system program thread
    startSystemProgramThread(command);
  }


  /**
   * Run the appropriate sample com file for the given axis ID
   * @param axisID the AxisID to run sample.com on.
   */
  public void createSample(AxisID axisID) {
    //
    //  Create the required tiltalign command
    //
    String command = "sample" + axisID.getExtension() + ".com";

    //  Start the com script in the background
    startComScript(command);
  }


  /**
   * Open imod on the coarse aligned stack for the selected axis
   * @param axisID the AxisID to coarse align.
   */
  public void imodSample(AxisID axisID) {
    String arguments = "";

    //
    //  Create the required imod command
    //
    if(axisID == AxisID.ONLY) {
      arguments = "top.rec mid.rec bot.rec tomopitch.mod";
    }
    if(axisID == AxisID.FIRST) {
      arguments = "topa.rec mida.rec bota.rec tomopitcha.mod";
    }
    if(axisID == AxisID.SECOND) {
      arguments = "topb.rec midb.rec botb.rec tomopitchb.mod";
    }

    String command  = "imod " + arguments;

    //  Start the system program thread
    startSystemProgramThread(command);
  }


  /**
   * Run the appropriate tomopitch com file for the given axis ID
   * @param axisID the AxisID to run tomoptich on.
   */
  public void tomopitch(AxisID axisID) {
    //
    //  Create the required tiltalign command
    //
    String command = "tomopitch" + axisID.getExtension() + ".com";

    //  Start the com script in the background
    startComScript(command);
  }


  /**
   * Run the appropriate newst com file for the given axis ID
   * @param axisID the AxisID to run newst on.
   */
  public void newst(AxisID axisID) {
    //
    //  Create the required newst command
    //
    String command = "newst" + axisID.getExtension() + ".com";

    //  Start the com script in the background
    startComScript(command);
  }


  /**
   * Run the appropriate tilt com file for the given axis ID
   * @param axisID the AxisID to run tilt on.
   */
  public void tilt(AxisID axisID) {
    //
    //  Create the required tilt command
    //
    String command = "tilt" + axisID.getExtension() + ".com";

    //  Start the com script in the background
    startComScript(command);
  }

  /**
   * Open imod on the tomogram for the selected axis
   * @param axisID the AxisID to coarse align.
   */
  public void imodTomogram(AxisID axisID) {
    String extension = "";
    //
    //  Create the required imod command
    //
    if(axisID == AxisID.ONLY) {
      extension = ".rec";
    }
    if(axisID == AxisID.FIRST) {
      extension = "a.rec";
    }
    if(axisID == AxisID.SECOND) {
      extension = "b.rec";
    }

    String command  = "imod -Y " + appManager.getFilesetName() + extension;


    //  Start the system program thread
    startSystemProgramThread(command);
  }

  /**
   * Get the IMOD_DIR from the application manager and pass on to the calling
   * object
   */
  public String getIMODDirectory() {
    return appManager.getIMODDirectory();
  }


  /**
   * Execute the setupcombine script
   * @param combineParam A read-only object containing the parameters for
   * setupcombine script
   */
  public void createCombineScripts(ConstMetaData metaData)
    throws BadComScriptException, IOException {

    SetupCombine setupCombine = new SetupCombine(metaData);

    int exitValue = setupCombine.run();

    if(exitValue != 0) {
      System.out.println("Exit value: " + String.valueOf(exitValue));

      //  Compile the exception message from the stderr stream
      String[] stdError = setupCombine.getStdError();
      if(stdError.length < 1) {
	stdError = new String[1];
	stdError[0] =
	  "Get David to add some std error reporting to setupCombine";
      }
      StringBuffer buffer = new StringBuffer();
      buffer.append("SetupCombine Error\n");
      buffer.append("Standard error output:\n");
      for(int i = 0; i < stdError.length; i++) {
	buffer.append(stdError[i]);
	buffer.append("\n");
      }

      throw(new BadComScriptException(buffer.toString()));
    }
  }


  /**
   * A message specifying that a com script has finished execution
   * @param exitValue the exit value for the com script
   */
  public void msgComScriptDone(int exitValue, String command){
    //
    //  increment the state machine forward to allow for
    //
    System.out.println("Process complete:");
    System.out.println("  script: " + command);
    System.out.println("  Exit value: " + String.valueOf(exitValue));
  }


  //  Internal utility functions
  private void startSystemProgramThread(String command) {
    SystemProgram sysProgram = new SystemProgram(command);
    sysProgram.setWorkingDirectory(new File(appManager.getWorkingDirectory()));

    //  Start the system program thread
    Thread sysProgThread = new Thread(sysProgram);
    sysProgThread.start();
    System.out.println("Started " + command);
    System.out.println("  working directory: " +
      appManager.getWorkingDirectory());
  }


  private void startComScript(String command) {
    //  Run the script as a thread in the background
    RunComScript comScript = new RunComScript(command, this);
    comScript.setWorkingDirectory(new File(appManager.getWorkingDirectory()));

    comScript.start();

    System.out.println("Started " + command);
    System.out.println("  Name: " + comScript.getName());
  }
}
