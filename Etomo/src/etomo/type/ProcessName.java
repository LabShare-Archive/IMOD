package etomo.type;

/**
 * <p>Description: </p>
 *
 * <p>Copyright: Copyright (c) 2004</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $$Author$$
 *
 * @version $$Revision$$
 *
 * <p> $$Log$
 * <p> $Revision 1.1  2004/04/16 02:08:32  sueh
 * <p> $bug# 409 Enum-like class that can contain all known process names.  Can
 * <p> $derived a process name from a file name.  Contains strings that can be used
 * <p> $to build process file names.
 * <p> $$ </p>
 */
public class ProcessName {
  public static final String rcsid = "$$Id$$";
  //known process names
  private static final String eraser = "eraser";
  private static final String xcorr = "xcorr";
  private static final String prenewst = "prenewst";
  private static final String track = "track";
  private static final String align = "align";
  private static final String newst = "newst";
  private static final String tilt = "tilt";
  private static final String mtffilter = "mtffilter";
  private static final String solvematchshift = "solvematchshift";
  private static final String solvematchmod = "solvematchmod";
  private static final String patchcorr = "patchcorr";
  private static final String matchorwarp = "matchorwarp";
  private static final String tomopitch = "tomopitch";
  private static final String sample = "sample";
  private static final String combine = "combine";
  private static final String matchvol1 = "matchvol1";
  private static final String volcombine = "volcombine";
  private static final String preblend = "preblend";
  private static final String blend = "blend";
  private static final String undistort = "undistort";
  
  private final String name;
  
  private ProcessName(String name) {
    this.name = name;
  }

  public static final ProcessName ERASER = new ProcessName(eraser);
  public static final ProcessName XCORR = new ProcessName(xcorr);
  public static final ProcessName PRENEWST = new ProcessName(prenewst);
  public static final ProcessName TRACK = new ProcessName(track);
  public static final ProcessName ALIGN = new ProcessName(align);
  public static final ProcessName NEWST = new ProcessName(newst);
  public static final ProcessName TILT = new ProcessName(tilt);
  public static final ProcessName MTFFILTER = new ProcessName(mtffilter);
  public static final ProcessName SOLVEMATCHSHIFT = new ProcessName(solvematchshift);
  public static final ProcessName SOLVEMATCHMOD = new ProcessName(solvematchmod);
  public static final ProcessName PATCHCORR = new ProcessName(patchcorr);
  public static final ProcessName MATCHORWARP = new ProcessName(matchorwarp);
  public static final ProcessName TOMOPITCH = new ProcessName(tomopitch);
  public static final ProcessName SAMPLE = new ProcessName(sample);
  public static final ProcessName COMBINE = new ProcessName(combine);
  public static final ProcessName MATCHVOL1 = new ProcessName(matchvol1);
  public static final ProcessName VOLCOMBINE = new ProcessName(volcombine);
  public static final ProcessName PREBLEND = new ProcessName(preblend);
  public static final ProcessName BLEND = new ProcessName(blend);
  public static final ProcessName UNDISTORT = new ProcessName(undistort);
  
  /**
   * Returns a string representation of the object.
   */
  public String toString() {
    return name;
  }

  /**
   * Takes a string representation of an ProcessName type and returns the 
   * correct
   * static object.  The string is case insensitive.  Null is returned if the
   * string is not one of the known process names.
   */
  public static ProcessName fromString(String name) {
    if (name.compareToIgnoreCase(eraser) == 0) {
      return ERASER;
    }
    if (name.compareToIgnoreCase(xcorr) == 0) {
      return XCORR;
    }
    if (name.compareToIgnoreCase(prenewst) == 0) {
      return PRENEWST;
    }
    if (name.compareToIgnoreCase(track) == 0) {
      return TRACK;
    }
    if (name.compareToIgnoreCase(align) == 0) {
      return ALIGN;
    }
    if (name.compareToIgnoreCase(newst) == 0) {
      return NEWST;
    }
    if (name.compareToIgnoreCase(tilt) == 0) {
      return TILT;
    }
    if (name.compareToIgnoreCase(mtffilter) == 0) {
      return MTFFILTER;
    }
    if (name.compareToIgnoreCase(solvematchshift) == 0) {
      return SOLVEMATCHSHIFT;
    }
    if (name.compareToIgnoreCase(solvematchmod) == 0) {
      return SOLVEMATCHMOD;
    }
    if (name.compareToIgnoreCase(patchcorr) == 0) {
      return PATCHCORR;
    }
    if (name.compareToIgnoreCase(matchorwarp) == 0) {
      return MATCHORWARP;
    }
    if (name.compareToIgnoreCase(tomopitch) == 0) {
      return TOMOPITCH;
    }
    if (name.compareToIgnoreCase(sample) == 0) {
      return SAMPLE;
    }
    if (name.compareToIgnoreCase(combine) == 0) {
      return COMBINE;
    }
    if (name.compareToIgnoreCase(matchvol1) == 0) {
      return MATCHVOL1;
    }
    if (name.compareToIgnoreCase(volcombine) == 0) {
      return VOLCOMBINE;
    }
    if (name.compareToIgnoreCase(preblend) == 0) {
      return PREBLEND;
    }
    if (name.compareToIgnoreCase(blend) == 0) {
      return BLEND;
    }
    if (name.compareToIgnoreCase(undistort) == 0) {
      return UNDISTORT;
    }
    return null;
  }
  
  public static ProcessName fromFileName(
    String fileName,
    AxisID axisID,
    String extension) {
    String base = stripExtension(fileName, extension);
    if (axisID != AxisID.ONLY) {
      base = stripExtension(base, axisID.getExtension());
    }
    return fromString(base);
  }
  
  private static String stripExtension(String fileName, String extension) {
    int extensionIndex = fileName.lastIndexOf(extension);
    if (extensionIndex > 0) {
      return fileName.substring(0, extensionIndex);
    }
    return fileName;
  }
}
