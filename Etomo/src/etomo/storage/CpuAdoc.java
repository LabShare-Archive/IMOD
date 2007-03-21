package etomo.storage;

import java.io.FileNotFoundException;
import java.io.IOException;

import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAttribute;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.type.AxisID;
import etomo.type.EtomoBoolean2;
import etomo.util.EnvironmentVariable;

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

  private ReadOnlyAutodoc autodoc = null;
  private EtomoBoolean2 separateChunks = null;

  private CpuAdoc() {
  }

  public CpuAdoc getInstance() {
    return INSTANCE;
  }

  private ReadOnlyAutodoc getAutodoc(AxisID axisID) {
    if (autodoc == null) {
      try {
        autodoc = AutodocFactory.getInstance(AutodocFactory.CPU, axisID);
      }
      catch (FileNotFoundException e) {
        e.printStackTrace();
      }
      catch (IOException e) {
        e.printStackTrace();
      }
      catch (LogFile.ReadException e) {
        e.printStackTrace();
      }
      if (autodoc == null) {
        System.err.println("Missing $"+EnvironmentVariable.CALIB_DIR+"/cpu.adoc file.\n"
            + "Parallel processing cannot be used.\n"
            + "See $IMOD_DIR/autodoc/cpu.adoc.");
      }
    }
    return autodoc;
  }

  public boolean isSeparateChunks(AxisID axisID) {
    if (separateChunks == null) {
      separateChunks = new EtomoBoolean2();
      try {
        ReadOnlyAttribute attrib = getAutodoc(axisID).getAttribute("separate-chunks");
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
 * <p> Revision 1.5  2007/03/15 21:46:20  sueh
 * <p> bug# 964 Added ReadOnlyAttribute, which is used as an interface for Attribute,
 * <p> unless the Attribute needs to be modified.
 * <p>
 * <p> Revision 1.4  2007/03/01 01:19:30  sueh
 * <p> bug# 964 Added LogFile to Autodoc.
 * <p>
 * <p> Revision 1.3  2006/07/21 22:11:49  sueh
 * <p> bug# 901 Getting the calibration directory environment variable name from
 * <p> EnvironmentVariable.
 * <p>
 * <p> Revision 1.2  2006/06/30 17:02:15  sueh
 * <p> Improved warning about missing cpu.adoc.
 * <p>
 * <p> Revision 1.1  2006/06/14 00:33:47  sueh
 * <p> bug# 852 Moved classes to the autodoc package that parse an autodoc or find
 * <p> attributes specific to a type of autdoc.
 * <p>
 * <p> Revision 1.1  2006/06/08 19:04:38  sueh
 * <p> bug# 867 Class to read the cpu autodoc.
 * <p> </p>
 */
