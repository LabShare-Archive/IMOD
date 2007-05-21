package etomo.storage;

import java.io.FileNotFoundException;
import java.io.IOException;

import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAttribute;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.type.AxisID;
import etomo.type.EtomoNumber;
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
 * 
 * @threadsafe
 */
public class CpuAdoc {
  public static final String rcsid = "$Id$";

  private static CpuAdoc INSTANCE = null;

  private boolean separateChunks;
  private int minNice;
  private boolean usersColumn;

  private CpuAdoc() {
  }

  public static CpuAdoc getInstance(AxisID axisID) {
    if (INSTANCE != null) {
      return INSTANCE;
    }
    return createInstance(axisID);
  }

  private static synchronized CpuAdoc createInstance(AxisID axisID) {
    if (INSTANCE != null) {
      return INSTANCE;
    }
    INSTANCE = new CpuAdoc();
    INSTANCE.load(axisID);
    return INSTANCE;
  }

  public boolean isSeparateChunks() {
    return separateChunks;
  }
  
  public boolean isUsersColumn() {
    return usersColumn;
  }

  public boolean isMinNiceNull() {
    return EtomoNumber.isNull(minNice);
  }

  public int getMinNice() {
    return minNice;
  }

  private void load(AxisID axisID) {
    ReadOnlyAutodoc autodoc = getAutodoc(axisID);
    if (autodoc == null) {
      return;
    }
    separateChunks = loadBoolean(autodoc, "separate-chunks");
    minNice = loadInt(autodoc, "min", "nice");
    usersColumn = loadBoolean(autodoc,"users-column");
  }

  private ReadOnlyAutodoc getAutodoc(AxisID axisID) {
    ReadOnlyAutodoc autodoc = null;
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
      System.err.println("Missing $" + EnvironmentVariable.CALIB_DIR
          + "/cpu.adoc file.\n" + "Parallel processing cannot be used.\n"
          + "See $IMOD_DIR/autodoc/cpu.adoc.");
    }
    return autodoc;
  }

  private boolean loadBoolean(ReadOnlyAutodoc autodoc, String key) {
    ReadOnlyAttribute attrib = autodoc.getAttribute(key);
    if (attrib != null && !attrib.getValue().equals("0")) {
      return true;
    }
    return false;
  }

  private int loadInt(ReadOnlyAutodoc autodoc, String key1, String key2) {
    EtomoNumber number = new EtomoNumber();
    ReadOnlyAttribute attrib = autodoc.getAttribute(key1);
    if (attrib == null) {
      return number.getInt();
    }
    attrib = attrib.getAttribute(key2);
    if (attrib == null) {
      return number.getInt();
    }
    number.set(attrib.getValue());
    return number.getInt();
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.4  2007/05/18 23:52:22  sueh
 * <p> bug# 987 Made CpuAdoc thread-safe.  Added minNice.
 * <p>
 * <p> Revision 1.3  2007/03/21 18:10:49  sueh
 * <p> bug# 964 Moved Adoc classes out of the autodoc package because
 * <p> they not part of the autodoc.
 * <p>
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
