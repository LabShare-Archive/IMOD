package etomo.storage.autodoc;

import java.io.FileNotFoundException;
import java.io.IOException;

import etomo.type.AxisID;
import etomo.type.EtomoBoolean2;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
public class CpuAdoc {
  public static final String rcsid = "$Id$";

  public static final CpuAdoc INSTANCE = new CpuAdoc();

  private Autodoc autodoc = null;
  private EtomoBoolean2 separateChunks = null;

  private CpuAdoc() {
  }

  public CpuAdoc getInstance() {
    return INSTANCE;
  }

  private Autodoc getAutodoc(AxisID axisID) {
    if (autodoc == null) {
      try {
        autodoc = Autodoc.getInstance(Autodoc.CPU, axisID);
      }
      catch (FileNotFoundException e) {
        e.printStackTrace();
      }
      catch (IOException e) {
        e.printStackTrace();
      }
      if (autodoc == null) {
        System.err
            .println("Missing $IMOD_CALIB_DIR/cpu.adoc file./nSee $IMOD_DIR/autodoc/cpu.adoc.");
      }
    }
    return autodoc;
  }

  public boolean isSeparateChunks(AxisID axisID) {
    if (separateChunks == null) {
      separateChunks = new EtomoBoolean2();
      try {
        Attribute attrib = getAutodoc(axisID)
            .getAttribute("separate-chunks");
        if (attrib != null && !attrib.getValue().equals("0")) {
          separateChunks.set(true);
        }
        else {
          separateChunks.set(false);
        }
      }
      catch (NullPointerException e) {
        separateChunks.set(false);
      }
    }
    return separateChunks.is();
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.1  2006/06/08 19:04:38  sueh
 * <p> bug# 867 Class to read the cpu autodoc.
 * <p> </p>
 */
