package etomo.type;

import java.io.File;
import java.util.Properties;

import etomo.ApplicationManager;
import etomo.EtomoDirector;
import etomo.comscript.TiltalignParam;
import etomo.comscript.TrimvolParam;
import etomo.util.MRCHeader;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2002, 2003, 2004</p>
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
* <p> Revision 1.5  2005/01/06 18:19:05  sueh
* <p> bug# 578 added alignSkewOption and alignXStretchOption.
* <p>
* <p> Revision 1.4  2004/12/16 02:31:24  sueh
* <p> bug# 564 Manage trimvol flipped state and squeezevol flipped state
* <p> separately.  If trimvol flipped state changes and squeezevol is not rerun,
* <p> the squeezevol parameters with still load onto the screen correctly.
* <p>
* <p> Revision 1.3  2004/12/14 21:49:04  sueh
* <p> bug# 572:  Removing state object from meta data and managing it with a
* <p> manager class.  All state variables saved after a process is run belong in
* <p> the state object.
* <p>
* <p> Revision 1.2  2004/12/08 21:32:04  sueh
* <p> bug# 564 Added access to flipped.
* <p>
* <p> Revision 1.1  2004/12/07 22:54:07  sueh
* <p> bug# 564 Contains state variables to be saved in the .edf file.
* <p> </p>
*/
public class TomogramState implements BaseState {
  public static  final String  rcsid =  "$Id$";
  
  private static final String groupString = "ReconstructionState";
  
  EtomoBoolean trimvolFlipped = new EtomoBoolean("TrimvolFlipped");
  EtomoBoolean squeezevolFlipped = new EtomoBoolean("SqueezevolFlipped");
  EtomoBoolean madeZFactors = new EtomoBoolean("MadeZFactors");
  EtomoBoolean newstFiducialessAlignment = new EtomoBoolean("NewstFiducialessAlignment");
  
  public TomogramState() {
    trimvolFlipped.setBackwardCompatibleValue(new EtomoBoolean()).setResetValue(false);
    reset();
  }
  
  void reset() {
    trimvolFlipped.reset();
    squeezevolFlipped.reset();
    madeZFactors.reset();
    newstFiducialessAlignment.reset();
  }
  
  public void store(Properties props) {
    store(props, "");
  }
  
  public void store(Properties props, String prepend) {
    prepend = createPrepend(prepend);
    String group = prepend + ".";
    trimvolFlipped.store(props, prepend);
    squeezevolFlipped.store(props, prepend);
    madeZFactors.store(props, prepend);
    newstFiducialessAlignment.store(props, prepend);
  }

  public boolean equals(TomogramState that) {
    if (!trimvolFlipped.equals(that.trimvolFlipped)) {
      return false;
    }
    if (!squeezevolFlipped.equals(that.squeezevolFlipped)) {
      return false;
    }
    if (!madeZFactors.equals(that.madeZFactors)) {
      return false;
    }
    if (!newstFiducialessAlignment.equals(that.newstFiducialessAlignment)) {
      return false;
    }
    return true;
  }
  
  protected static String createPrepend(String prepend) {
    if (prepend == "") {
      return groupString;
    }
    return prepend + "." + groupString;
  }

  public void load(Properties props) {
    load(props, "");
  }

  public void load(Properties props, String prepend) {
    reset();
    prepend = createPrepend(prepend);
    String group = prepend + ".";
    trimvolFlipped.load(props, prepend);
    squeezevolFlipped.load(props, prepend);
    madeZFactors.load(props, prepend);
    newstFiducialessAlignment.load(props, prepend);
  }
  
  public ConstEtomoBoolean setTrimvolFlipped(boolean trimvolFlipped) {
    return this.trimvolFlipped.set(trimvolFlipped);
  }
  
  public ConstEtomoBoolean setSqueezevolFlipped(boolean squeezevolFlipped) {
    return this.squeezevolFlipped.set(squeezevolFlipped);
  }
  
  public ConstEtomoBoolean setMadeZFactors(boolean madeZFactors) {
    return this.madeZFactors.set(madeZFactors);
  }
  
  public ConstEtomoBoolean setNewstFiducialessAlignment(boolean newstFiducialessAlignment) {
    return this.newstFiducialessAlignment.set(newstFiducialessAlignment);
  }
  
  public ConstEtomoBoolean getTrimvolFlipped() {
    return trimvolFlipped;
  }
  
  public ConstEtomoBoolean getSqueezevolFlipped() {
    return squeezevolFlipped;
  }
  
  public ConstEtomoBoolean getMadeZFactors() {
    return madeZFactors;
  }
  
  public ConstEtomoBoolean getNewstFiducialessAlignment() {
    return newstFiducialessAlignment;
  }
  
  /**
   * Backward compatibility
   * function decide whether trimvol is flipped based on the header
   * @return
   */
  public boolean getBackwordCompatibleTrimvolFlipped() {
    //If trimvol has not been run, then assume that the tomogram has not been
    //flipped.
    EtomoDirector etomoDirector = EtomoDirector.getInstance();
    ApplicationManager manager = (ApplicationManager) etomoDirector
        .getCurrentManager();
    String datasetName = manager.getMetaData().getDatasetName();
    File trimvolFile = new File(etomoDirector.getCurrentPropertyUserDir(),
        TrimvolParam.getOutputFileName(datasetName));
    if (!trimvolFile.exists()) {
      return false;
    }
    MRCHeader header = new MRCHeader(trimvolFile.getAbsolutePath());
    try {
      header.read();
    }
    catch (Exception e) {
      e.printStackTrace();
      return false;
    }
    if (header.getNRows() < header.getNSections()) {
      System.err.println("Assuming that " + trimvolFile.getName() + " has not been flipped\n"
          + "because the Y is less then Z in the header.");
      return false;
    }
    System.err.println("Assuming that " + trimvolFile.getName() + " has been flipped\n" 
        + "because the Y is greater or equal to Z in the header.");
    return true;
  }
  
  /**
   * Backward compatibility
   * function decide whether z factors where made based on the relationship
   * between .zfac file and the .tltxf file
   * @return
   */
  public boolean getBackwordCompatibleMadeZFactors(AxisID axisID) {
    EtomoDirector etomoDirector = EtomoDirector.getInstance();
    String userDir = etomoDirector.getCurrentPropertyUserDir();
    ApplicationManager manager = (ApplicationManager) etomoDirector
        .getCurrentManager();
    String datasetName = manager.getMetaData().getDatasetName();
    File zFactorFile = new File(userDir, TiltalignParam.getOutputZFactorFileName(datasetName, axisID));
    File tltxfFile = new File(userDir, datasetName + axisID.getExtension() + ".tltxf");
    if (!zFactorFile.exists()) {
      System.err.println("Assuming that madeZFactors is false\n" 
          + "because " + zFactorFile.getName() + " does not exist.");
      return false;
    }
    if (!tltxfFile.exists()) {
      System.err.println("Assuming that madeZFactors is false\n" 
          + "because " + tltxfFile.getName() + " does not exist.");
      return false;
    }
    if (zFactorFile.lastModified() < tltxfFile.lastModified()) {
      System.err.println("Assuming that madeZFactors is false\n" 
          + "because " + zFactorFile.getName() + " is older then " + tltxfFile.getName() + ".");

      return false;
    }
    System.err.println("Assuming that madeZFactors is true\n" 
        + "because " + zFactorFile.getName() + " was modified after " + tltxfFile.getName() + ".");
    return true;
  }
  
  /**
   * Backward compatibility
   * function to decide whether the .ali file was created with fiducials or not.
   * @return
   */
  public boolean getBackwordCompatibleNewstFiducialessAlignment(AxisID axisID) {
    EtomoDirector etomoDirector = EtomoDirector.getInstance();
    String userDir = etomoDirector.getCurrentPropertyUserDir();
    ApplicationManager manager = (ApplicationManager) etomoDirector
        .getCurrentManager();
    String datasetName = manager.getMetaData().getDatasetName();
    File alignFile = new File(userDir, datasetName + axisID.getExtension() + ".ali");
    File rotationFile = new File(userDir, "rotation" + axisID.getExtension() + ".xf");
    File nonfidFile = new File(userDir, datasetName + axisID.getExtension() + "_nonfid.xf");
    if (!alignFile.exists()) {
      System.err.println("Assuming that newstFiducialessAlignment is false\n" 
          + "because " + alignFile.getName() + " does not exist.");
      return false;
    }
    if (!nonfidFile.exists()) {
      System.err.println("Assuming that newstFiducialessAlignment is false\n" 
          + "because " + nonfidFile.getName() + " does not exist.");
      return false;
    }
    //rotation.xf is always modified before running newst.
    //_nonfid.xf is created from rotation.xf when in nonfiducial mode
    if (rotationFile.lastModified() > nonfidFile.lastModified()) {
      System.err.println("Assuming that newstFiducialessAlignment is false\n" 
          + "because " + rotationFile.getName() + " is older then " + nonfidFile.getName() + ".");

      return false;
    }
    System.err.println("Assuming that newstFiducialessAlignment is true\n" 
        + "because " + rotationFile.getName() + " was modified after " + nonfidFile.getName() + ".");
    return true;
  }

}
