package etomo.storage;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

import etomo.storage.autodoc.Autodoc;
import etomo.storage.autodoc.ReadOnlyAttribute;
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

  public MatlabParamFile(File file) {
    this.file = file;
  }

  public synchronized void read() {
    volumeList.clear();
    Autodoc autodoc;
    try {
      autodoc = (Autodoc.getMatlabInstance(file, false));
      if (autodoc == null) {
        UIHarness.INSTANCE.openMessageDialog("Unable to read " + file.getName()
            + ".", "File Error");
        return;
      }
      String[] fnVolumeArray = getList(autodoc.getAttribute("fnVolume"));
      String[] fnModParticleArray = getList(autodoc
          .getAttribute("fnModParticle"));
      String initMotl = autodoc.getAttribute("initMOTL").getValue();
      initMotlCode = InitMotlCode.getInstance(initMotl);
      String[] initMotlArray = null;
      if (initMotlCode == null) {
        initMotlArray = getList(initMotl);
      }
      String[][] tiltRangeArray = getListOfArrays(getList(autodoc
          .getAttribute("tiltRange")));
      String[][] relativeOrientArray = getListOfArrays(getList(autodoc
          .getAttribute("relativeOrient")));
      //Add entries to volumeList, ignoring any entries for which there is no
      //corresponding fnVolume.
      for (int i = 0; i < fnVolumeArray.length; i++) {
        Volume volume = new Volume();
        volume.setFnVolume(removeQuotes(fnVolumeArray[i]));
        if (i < fnModParticleArray.length) {
          volume.setFnModParticle(removeQuotes(fnModParticleArray[i]));
        }
        if (initMotlArray != null && i < initMotlArray.length) {
          volume.setInitMotl(removeQuotes(initMotlArray[i]));
        }
        if (i < tiltRangeArray.length) {
          volume.setTiltRange(tiltRangeArray[i]);
        }
        if (i < relativeOrientArray.length) {
          volume.setRelativeOrient(relativeOrientArray[i]);
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

  public int getVolumeListSize() {
    return volumeList.size();
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

  public String getInitMotlFile(int index) {
    return ((Volume) volumeList.get(index)).getInitMotl();
  }

  public ConstEtomoNumber getTiltRangeStart(int index) {
    return ((Volume) volumeList.get(index)).getTiltRangeStart();
  }

  public ConstEtomoNumber getTiltRangeEnd(int index) {
    return ((Volume) volumeList.get(index)).getTiltRangeEnd();
  }

  public String getRelativeOrient(int index) {
    return ((Volume) volumeList.get(index)).getRelativeOrient();
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
   * @return
   */
  private String[] getList(String value) {
    System.out.println("value=" + value);
    if (value == null) {
      return new String[0];
    }
    value = value.trim();
    System.out.println("value=" + value);
    if (value.charAt(0) == '{' && value.charAt(value.length() - 1) == '}') {
      //remove "{" and "}"
      value = value.substring(1, value.length() - 1).trim();
      System.out.println("value=" + value);
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

    private static InitMotlCode getInstance(String initMotl) {
      if (ZERO_CODE.equals(initMotl)) {
        return ZERO;
      }
      if (Z_AXIS_CODE.equals(initMotl)) {
        return Z_AXIS;
      }
      if (X_AND_Z_AXIS_CODE.equals(initMotl)) {
        return X_AND_Z_AXIS;
      }
      return null;
    }
  }

  private static final class Volume {
    private String fnVolume = "";
    private String fnModParticle = "";
    private String initMotl = "";
    private final EtomoNumber[] tiltRange = new EtomoNumber[] {
        new EtomoNumber(), new EtomoNumber() };
    private final EtomoNumber[] relativeOrient = new EtomoNumber[] {
        new EtomoNumber(EtomoNumber.Type.FLOAT),
        new EtomoNumber(EtomoNumber.Type.FLOAT),
        new EtomoNumber(EtomoNumber.Type.FLOAT) };

    private void setFnVolume(String fnVolume) {
      this.fnVolume = fnVolume;
    }

    private String getFnVolume() {
      return fnVolume;
    }

    private void setFnModParticle(String fnModParticle) {
      this.fnModParticle = fnModParticle;
    }

    private String getFnModParticle() {
      return fnModParticle;
    }

    private void setInitMotl(String initMotl) {
      this.initMotl = initMotl;
    }

    private String getInitMotl() {
      return initMotl;
    }

    private void setTiltRange(String[] tiltRange) {
      for (int i = 0; i < this.tiltRange.length; i++) {
        if (i >= tiltRange.length) {
          return;
        }
        this.tiltRange[i].set(tiltRange[i]);
      }
    }

    private ConstEtomoNumber getTiltRangeStart() {
      return tiltRange[0];
    }

    private ConstEtomoNumber getTiltRangeEnd() {
      return tiltRange[1];
    }

    private void setRelativeOrient(String[] relativeOrient) {
      for (int i = 0; i < this.relativeOrient.length; i++) {
        if (i >= relativeOrient.length) {
          return;
        }
        this.relativeOrient[i].set(relativeOrient[i]);
      }
    }

    private String getRelativeOrient() {
      StringBuffer buffer = new StringBuffer();
      for (int i = 0; i < relativeOrient.length; i++) {
        buffer.append(relativeOrient.toString());
        if (i < relativeOrient.length - 1) {
          buffer.append(DIVIDER + ' ');
        }
      }
      return buffer.toString();
    }
  }
}
