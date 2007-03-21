package etomo.storage;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAttribute;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.type.ConstEtomoNumber;
import etomo.type.EtomoNumber;
import etomo.ui.UIHarness;

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
 * <p> $Log$
 * <p> Revision 1.3  2007/03/20 23:02:27  sueh
 * <p> bug# 964 Distinguishing between tiltRange={} and tiltRange={[],[]}.  Returning
 * <p> relativeOrient elements separately.
 * <p>
 * <p> Revision 1.2  2007/03/20 00:43:39  sueh
 * <p> bug# 964 Added Volume.tiltRange and Volume.relativeOrient.
 * <p>
 * <p> Revision 1.1  2007/03/15 21:42:17  sueh
 * <p> bug# 964 A class which represents a .prm file.
 * <p> </p>
 */
public final class MatlabParamFile {
  public static final String rcsid = "$Id$";

  private static final char DIVIDER = ',';
  private final ArrayList volumeList = new ArrayList();
  private final File file;
  private InitMotlCode initMotlCode = null;
  private boolean tiltRangeEmpty = false;

  public MatlabParamFile(File file) {
    this.file = file;
  }

  public synchronized void read() {
    volumeList.clear();
    try {
      ReadOnlyAutodoc autodoc = (AutodocFactory.getMatlabInstance(file));
      if (autodoc == null) {
        UIHarness.INSTANCE.openMessageDialog("Unable to read " + file.getName()
            + ".", "File Error");
        return;
      }
      //Parse each attributes' value.
      String[] fnVolumeList = getList(autodoc.getAttribute("fnVolume"));
      String[] fnModParticleList = getList(autodoc
          .getAttribute("fnModParticle"));
      //see if initMotl is using a single code
      String initMotl = autodoc.getAttribute("initMOTL").getValue();
      initMotlCode = InitMotlCode.getInstance(initMotl);
      String[] initMotlList = null;
      if (initMotlCode == null) {
        initMotlList = getList(initMotl);
      }
      String[] tiltRangeList = getList(autodoc.getAttribute("tiltRange"));
      String[][] tiltRangeArrayList = null;
      //Distinguish between {} (an empty list) and {[],[]}
      if (tiltRangeList.length == 0) {
        //empty list
        tiltRangeEmpty = true;
      }
      else {
        //not an empty list
        tiltRangeArrayList = getListOfArrays(tiltRangeList);
      }
      String[][] relativeOrientArrayList = getListOfArrays(getList(autodoc
          .getAttribute("relativeOrient")));
      //Add entries to volumeList, ignoring any entries for which there is no
      //corresponding fnVolume.
      for (int i = 0; i < fnVolumeList.length; i++) {
        Volume volume = new Volume();
        volume.setFnVolume(removeQuotes(fnVolumeList[i]));
        if (i < fnModParticleList.length) {
          volume.setFnModParticle(removeQuotes(fnModParticleList[i]));
        }
        if (initMotlList != null && i < initMotlList.length) {
          volume.setInitMotl(removeQuotes(initMotlList[i]));
        }
        if (!tiltRangeEmpty && i < tiltRangeArrayList.length) {
          volume.setTiltRange(tiltRangeArrayList[i]);
        }
        if (i < relativeOrientArrayList.length) {
          volume.setRelativeOrient(relativeOrientArrayList[i]);
        }
        volumeList.add(volume);
      }
    }
    catch (IOException e) {
      UIHarness.INSTANCE.openMessageDialog("Unable to load " + file.getName()
          + ".  IOException:  " + e.getMessage(), "File Error");
    }
    catch (LogFile.ReadException e) {
      UIHarness.INSTANCE.openMessageDialog("Unable to read " + file.getName()
          + ".  LogFile.ReadException:  " + e.getMessage(), "File Error");
    }
  }

  public void write() {
    try {
      ReadOnlyAutodoc autodoc = (AutodocFactory.getMatlabInstance(file));
      if (autodoc == null) {
        autodoc = (AutodocFactory.getEmptyMatlabInstance(file));
        //build(autodoc);
      }
    }
    catch (IOException e) {
      UIHarness.INSTANCE.openMessageDialog("Unable to load " + file.getName()
          + ".  IOException:  " + e.getMessage(), "File Error");
    }
    catch (LogFile.ReadException e) {
      UIHarness.INSTANCE.openMessageDialog("Unable to read " + file.getName()
          + ".  LogFile.ReadException:  " + e.getMessage(), "File Error");
    }
  }

  public int getVolumeListSize() {
    return volumeList.size();
  }

  public Volume getVolume(int index) {
    return (Volume) volumeList.get(index);
  }

  public String getFnVolume(int index) {
    return ((Volume) volumeList.get(index)).getFnVolume();
  }

  public String getFnModParticle(int index) {
    return ((Volume) volumeList.get(index)).getFnModParticle();
  }

  public InitMotlCode getInitMotlCode() {
    return initMotlCode;
  }

  public void setInitMotlCode(ConstEtomoNumber code) {
    initMotlCode = InitMotlCode.getInstance(code.toString());
  }

  public boolean isTiltRangeEmpty() {
    return tiltRangeEmpty;
  }

  public void setTiltRangeEmpty(boolean tiltRangeEmpty) {
    this.tiltRangeEmpty = tiltRangeEmpty;
  }

  /**
   * Arrays are surrounded by {}'s.  Array elements are separated by ",".  Find
   * the array in the value parameter and return it.  If there is no array,
   * return the whole value parameter.
   * @param ReadOnlyAttribute
   * @return
   */
  private String[] getList(final ReadOnlyAttribute attribute) {
    if (attribute == null) {
      return new String[0];
    }
    return getList(attribute.getValue());
  }

  /**
   * Lists are surrounded by {}'s.  List elements are separated by ",".  Find
   * the list in the value parameter and return it.  If there is no list, return
   * the whole value parameter.
   * @param String
   * @return String array of elements of list.  Zero length array is there is no attribute, an empty value, or an empty list, "{}".
   */
  private String[] getList(String value) {
    if (value == null) {
      return new String[0];
    }
    value = value.trim();
    if (value.charAt(0) == '{' && value.charAt(value.length() - 1) == '}') {
      //remove "{" and "}"
      value = value.substring(1, value.length() - 1).trim();
      if (value.length() == 0) {
        return new String[0];
      }
      //split into list
      return value.split("\\s*,\\s*");
    }
    return new String[] { value };
  }

  /**
   * Arrays are surrounded by []'s.  Array elements are separated by whitespace
   * or ",".  Find the array in each element of the list parameter and return
   * them.  If there is no array in a list element, use the whole element.
   * @param array
   * @return
   */
  private String[][] getListOfArrays(String[] list) {
    if (list.length == 0) {
      return new String[0][0];
    }
    String[][] listOfArrays = new String[list.length][];
    for (int i = 0; i < list.length; i++) {
      System.out.println("list[" + i + "]=" + list[i]);
      list[i] = list[i].trim();
      if (list[i].charAt(0) == '['
          && list[i].charAt(list[i].length() - 1) == ']') {
        //remove "[" and "]"
        list[i] = list[i].substring(1, list[i].length() - 1).trim();
        if (list[i].length() == 0) {
          listOfArrays[i] = new String[0];
        }
        //split into array
        listOfArrays[i] = list[i].split("\\s*[\\s,]\\s*");
      }
      else {
        listOfArrays[i] = new String[] { list[i] };
      }
    }
    return listOfArrays;
  }

  private String removeQuotes(String string) {
    string = string.trim();
    if (string.charAt(0) == '\'' && string.charAt(string.length() - 1) == '\'') {
      return string.substring(1, string.length() - 1);
    }
    return string;
  }

  public static final class InitMotlCode {
    private static final EtomoNumber ZERO_CODE = new EtomoNumber().set(0);
    private static final EtomoNumber Z_AXIS_CODE = new EtomoNumber().set(1);
    private static final EtomoNumber X_AND_Z_AXIS_CODE = new EtomoNumber()
        .set(2);

    public static final InitMotlCode ZERO = new InitMotlCode(ZERO_CODE);
    public static final InitMotlCode Z_AXIS = new InitMotlCode(Z_AXIS_CODE);
    public static final InitMotlCode X_AND_Z_AXIS = new InitMotlCode(
        X_AND_Z_AXIS_CODE);

    private final EtomoNumber code;

    private InitMotlCode(EtomoNumber code) {
      this.code = code;
    }

    private static InitMotlCode getInstance(String code) {
      if (ZERO_CODE.equals(code)) {
        return ZERO;
      }
      if (Z_AXIS_CODE.equals(code)) {
        return Z_AXIS;
      }
      if (X_AND_Z_AXIS_CODE.equals(code)) {
        return X_AND_Z_AXIS;
      }
      return null;
    }

    public ConstEtomoNumber getCode() {
      return code;
    }
  }

  public static final class Volume {
    private static final int TILT_RANGE_START_INDEX = 0;
    private static final int TILT_RANGE_END_INDEX = 1;
    private static final int RELATIVE_ORIENT_X_INDEX = 0;
    private static final int RELATIVE_ORIENT_Y_INDEX = 1;
    private static final int RELATIVE_ORIENT_Z_INDEX = 2;
    private String fnVolume = "";
    private String fnModParticle = "";
    private String initMotl = "";
    private final EtomoNumber[] tiltRange = new EtomoNumber[] {
        new EtomoNumber(), new EtomoNumber() };
    private final EtomoNumber[] relativeOrient = new EtomoNumber[] {
        new EtomoNumber(EtomoNumber.Type.FLOAT),
        new EtomoNumber(EtomoNumber.Type.FLOAT),
        new EtomoNumber(EtomoNumber.Type.FLOAT) };

    public void setFnVolume(String fnVolume) {
      this.fnVolume = fnVolume;
    }

    public String getFnVolume() {
      return fnVolume;
    }

    public void setFnModParticle(String fnModParticle) {
      this.fnModParticle = fnModParticle;
    }

    public String getFnModParticle() {
      return fnModParticle;
    }

    public void setInitMotl(String initMotl) {
      this.initMotl = initMotl;
    }

    public String getInitMotl() {
      return initMotl;
    }

    public String getTiltRangeStart() {
      return tiltRange[TILT_RANGE_START_INDEX].toString();
    }

    public void setTiltRangeStart(String tiltRangeStart) {
      tiltRange[TILT_RANGE_START_INDEX].set(tiltRangeStart);
    }

    public String getTiltRangeEnd() {
      return tiltRange[TILT_RANGE_END_INDEX].toString();
    }

    public void setTiltRangeEnd(String tiltRangeEnd) {
      tiltRange[TILT_RANGE_END_INDEX].set(tiltRangeEnd);
    }

    public String getRelativeOrientX() {
      return relativeOrient[RELATIVE_ORIENT_X_INDEX].toString();
    }

    public void setRelativeOrientX(String relativeOrientX) {
      relativeOrient[RELATIVE_ORIENT_X_INDEX].set(relativeOrientX);
    }

    public String getRelativeOrientY() {
      return relativeOrient[RELATIVE_ORIENT_Y_INDEX].toString();
    }

    public void setRelativeOrientY(String relativeOrientY) {
      relativeOrient[RELATIVE_ORIENT_Y_INDEX].set(relativeOrientY);
    }

    public String getRelativeOrientZ() {
      return relativeOrient[RELATIVE_ORIENT_Z_INDEX].toString();
    }

    public void setRelativeOrientZ(String relativeOrientZ) {
      relativeOrient[RELATIVE_ORIENT_Z_INDEX].set(relativeOrientZ);
    }

    private void setTiltRange(String[] tiltRange) {
      for (int i = 0; i < this.tiltRange.length; i++) {
        if (i >= tiltRange.length) {
          return;
        }
        this.tiltRange[i].set(tiltRange[i]);
      }
    }

    private void setRelativeOrient(String[] relativeOrient) {
      for (int i = 0; i < this.relativeOrient.length; i++) {
        if (i >= relativeOrient.length) {
          return;
        }
        this.relativeOrient[i].set(relativeOrient[i]);
      }
    }
  }
}
