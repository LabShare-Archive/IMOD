package etomo.storage;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

import etomo.storage.autodoc.Autodoc;
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
 * <p> $Log$ </p>
 */
public final class MatlabParamFile {
  public static final String rcsid = "$Id$";

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
      String[] fnVolumeArray = getArray(autodoc.getAttribute("fnVolume")
          .getValue());
      String[] fnModParticleArray = getArray(autodoc.getAttribute(
          "fnModParticle").getValue());
      String initMotl = autodoc.getAttribute("initMOTL").getValue();
      initMotlCode = InitMotlCode.getInstance(initMotl);
      String[] initMotlArray = null;
      if (initMotlCode == null) {
        initMotlArray = getArray(initMotl);
      }
      //Add entries to volumeList, ignoring any entries for which there is no
      //corresponding fnVolume.
      for (int i = 0; i < fnVolumeArray.length; i++) {
        Volume volume = new Volume();
        volume.setFnVolume(removeQuotes(fnVolumeArray[i]));
        volume.setFnModParticle(removeQuotes(fnModParticleArray[i]));
        if (initMotlArray != null) {
          volume.setInitMotl(removeQuotes(initMotlArray[i]));
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
    return ((Volume)volumeList.get(index)).getFnVolume();
  }
  
  public String getFnModParticle(int index) {
    return ((Volume)volumeList.get(index)).getFnModParticle();
  }
  
  public InitMotlCode getInitMotlCode() {
    return initMotlCode;
  }
  
  public String getInitMotlFile(int index) {
    return ((Volume)volumeList.get(index)).getInitMotl();
  }

  /**
   * Arrays are surrounded by {}'s.  Array elements are separated by ",".  Find
   * the array in the string parameter and return it.  If there is no array, return
   * the whole string parameter.
   * @param string
   * @return
   */
  private String[] getArray(String string) {
    string = string.trim();
    if (string.charAt(0) == '{' && string.charAt(string.length() - 1) == '}') {
      return string.substring(1, string.length() - 1).split("\\s*,\\s*");
    }
    return new String[] { string };
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
    private static final EtomoNumber PHI_CODE = new EtomoNumber().set(1);
    private static final EtomoNumber PHI_AND_THETA_CODE = new EtomoNumber()
        .set(2);

    private static final InitMotlCode ZERO = new InitMotlCode(ZERO_CODE);
    private static final InitMotlCode PHI = new InitMotlCode(PHI_CODE);
    private static final InitMotlCode PHI_AND_THETA = new InitMotlCode(
        PHI_AND_THETA_CODE);

    private final EtomoNumber code;

    private InitMotlCode(EtomoNumber code) {
      this.code = code;
    }

    private static InitMotlCode getInstance(String initMotl) {
      if (ZERO_CODE.equals(initMotl)) {
        return ZERO;
      }
      if (PHI_CODE.equals(initMotl)) {
        return PHI;
      }
      if (PHI_AND_THETA_CODE.equals(initMotl)) {
        return PHI_AND_THETA;
      }
      return null;
    }
  }

  private static final class Volume {
    private String fnVolume = "";
    private String fnModParticle = "";
    private String initMotl = "";

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
  }
}
