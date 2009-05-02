package etomo.type;

import java.io.File;

import etomo.util.DatasetFiles;

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
 * <p> $Revision 1.20  2009/04/01 20:07:39  sueh
 * <p> $bug# 1208 Added CLIP.
 * <p> $
 * <p> $Revision 1.19  2008/11/20 01:36:58  sueh
 * <p> $bug# 1147 Added CCD_ERASER - for running ccderaser from the
 * <p> $command line.
 * <p> $
 * <p> $Revision 1.18  2008/10/27 19:25:23  sueh
 * <p> $bug# 1141 Added ctfPlotter, ctfCorrection, and splitCorrection
 * <p> $
 * <p> $Revision 1.17  2007/12/10 22:03:58  sueh
 * <p> $bug# 1041 Standardized the getInstance functions and made sure that they will
 * <p> $work whether they get a process name, a process name plus an axis extension,
 * <p> $or a file name.  Removed fromFileName and stripExtension.
 * <p> $
 * <p> $Revision 1.16  2007/11/06 19:51:22  sueh
 * <p> $bug# 1047 Added nad_eed_3d and chunksetup.
 * <p> $
 * <p> $Revision 1.15  2007/08/29 21:45:33  sueh
 * <p> $bug# 1041 Added getInstance(File,String) and getInstance(String,AxisID).
 * <p> $
 * <p> $Revision 1.14  2007/07/10 00:35:55  sueh
 * <p> $bug# 1022 Changed peetParser back to prmParser.
 * <p> $
 * <p> $Revision 1.13  2007/04/27 23:39:47  sueh
 * <p> $bug# 964 Changed prmParser to peetParser.
 * <p> $
 * <p> $Revision 1.12  2007/04/26 02:48:16  sueh
 * <p> $bug# 964 Added PRMPARSER.
 * <p> $
 * <p> $Revision 1.11  2007/02/05 23:30:17  sueh
 * <p> $bug# 962 Added xfjointomo, xftoxg, xfmodel, and remapmodel.
 * <p> $
 * <p> $Revision 1.10  2006/10/24 23:24:08  sueh
 * <p> $bug# 947
 * <p> $
 * <p> $Revision 1.9  2006/06/05 16:04:17  sueh
 * <p> $bug# 766 Added the rest of the non-comscript processes.  Changed
 * <p> $getCommand() and getCommandArray() to getComscript... because the fuctions
 * <p> $are specialized for comscripts.
 * <p> $
 * <p> $Revision 1.8  2006/04/06 20:13:59  sueh
 * <p> $bug# 808 Added the ability to strip the extension in equals().  Getting the
 * <p> $extension from DatasetFiles.
 * <p> $
 * <p> $Revision 1.7  2005/12/09 20:28:36  sueh
 * <p> $bug# 776 Added tomosnapshot and equals()
 * <p> $
 * <p> $Revision 1.6  2005/11/19 02:42:06  sueh
 * <p> $bug# 744 Added processchunks.
 * <p> $
 * <p> $Revision 1.5  2005/08/31 19:11:41  sueh
 * <p> $bug# 719 Added startjoin.
 * <p> $
 * <p> $Revision 1.4  2005/08/25 01:49:04  sueh
 * <p> $bug# 715 added solvematch to ProcessName.
 * <p> $
 * <p> $Revision 1.3  2005/08/24 22:39:46  sueh
 * <p> $bug# 715 Added getCommand() and getCommandArray() so that
 * <p> $comscript commands can be built in one place.  ProcessName shouldn't
 * <p> $contain anything but comscripts.
 * <p> $
 * <p> $Revision 1.2  2005/04/07 21:58:25  sueh
 * <p> $bug# 626 Added preblend, blend, and undistort.
 * <p> $
 * <p> $Revision 1.1  2004/04/16 02:08:32  sueh
 * <p> $bug# 409 Enum-like class that can contain all known process names.  Can
 * <p> $derived a process name from a file name.  Contains strings that can be used
 * <p> $to build process file names.
 * <p> $$ </p>
 */
public class ProcessName {
  public static final String rcsid = "$$Id$$";

  //known process names  
  private static final String eraser = "eraser";//comscript which runs ccderaser
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
  private static final String solvematch = "solvematch";
  private static final String startjoin = "startjoin";
  private static final String processchunks = "processchunks";
  private static final String tomosnapshot = "tomosnapshot";
  private static final String transferfid = "transferfid";
  private static final String clipflipyz = "clipflipyz";
  private static final String finishjoin = "finishjoin";
  private static final String makejoincom = "makejoincom";
  private static final String trimvol = "trimvol";
  private static final String squeezevol = "squeezevol";
  private static final String archiveorig = "archiveorig";
  private static final String splittilt = "splittilt";
  private static final String splitcombine = "splitcombine";
  private static final String extractmagrad = "extractmagrad";
  private static final String extracttilts = "extracttilts";
  private static final String extractpieces = "extractpieces";
  private static final String xfalign = "xfalign";
  private static final String xfjointomo = "xfjointomo";
  private static final String xftoxg = "xftoxg";
  private static final String xfmodel = "xfmodel";
  private static final String remapmodel = "remapmodel";
  private static final String peetParser = "prmParser";
  private static final String anisotropicDiffusion = "nad_eed_3d";
  private static final String chunksetup = "chunksetup";
  private static final String ctfPlotter = "ctfplotter";
  private static final String ctfCorrection = "ctfcorrection";
  private static final String splitCorrection = "splitcorrection";
  private static final String ccderaser = "ccderaser";
  private static final String clip = "clip";
  private static final String runraptor ="runraptor";

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
  public static final ProcessName SOLVEMATCHSHIFT = new ProcessName(
      solvematchshift);
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
  public static final ProcessName SOLVEMATCH = new ProcessName(solvematch);
  public static final ProcessName STARTJOIN = new ProcessName(startjoin);
  public static final ProcessName PROCESSCHUNKS = new ProcessName(processchunks);
  public static final ProcessName TOMOSNAPSHOT = new ProcessName(tomosnapshot);
  public static final ProcessName TRANSFERFID = new ProcessName(transferfid);
  public static final ProcessName CLIPFLIPYZ = new ProcessName(clipflipyz);
  public static final ProcessName FINISHJOIN = new ProcessName(finishjoin);
  public static final ProcessName MAKEJOINCOM = new ProcessName(makejoincom);
  public static final ProcessName TRIMVOL = new ProcessName(trimvol);
  public static final ProcessName SQUEEZEVOL = new ProcessName(squeezevol);
  public static final ProcessName ARCHIVEORIG = new ProcessName(archiveorig);
  public static final ProcessName SPLITTILT = new ProcessName(splittilt);
  public static final ProcessName SPLITCOMBINE = new ProcessName(splitcombine);
  public static final ProcessName EXTRACTMAGRAD = new ProcessName(extractmagrad);
  public static final ProcessName EXTRACTTILTS = new ProcessName(extracttilts);
  public static final ProcessName EXTRACTPIECES = new ProcessName(extractpieces);
  public static final ProcessName XFALIGN = new ProcessName(xfalign);
  public static final ProcessName XFJOINTOMO = new ProcessName(xfjointomo);
  public static final ProcessName XFTOXG = new ProcessName(xftoxg);
  public static final ProcessName XFMODEL = new ProcessName(xfmodel);
  public static final ProcessName REMAPMODEL = new ProcessName(remapmodel);
  public static final ProcessName PEET_PARSER = new ProcessName(peetParser);
  public static final ProcessName ANISOTROPIC_DIFFUSION = new ProcessName(
      anisotropicDiffusion);
  public static final ProcessName CHUNKSETUP = new ProcessName(chunksetup);
  public static final ProcessName CTF_PLOTTER = new ProcessName(ctfPlotter);
  public static final ProcessName CTF_CORRECTION = new ProcessName(ctfCorrection);
  public static final ProcessName SPLIT_CORRECTION = new ProcessName(splitCorrection);
  public static final ProcessName CCD_ERASER = new ProcessName(ccderaser);
  public static final ProcessName CLIP = new ProcessName(clip);
  public static final ProcessName RUNRAPTOR = new ProcessName(runraptor);

  /**
   * Returns a string representation of the object.
   */
  public String toString() {
    return name;
  }

  public final boolean equals(String name) {
    if (getInstance(name) == this) {
      return true;
    }
    return false;
  }

  /**
   * Turn name into an instance of ProcessName.
   * Possibilities are checked in this order:
   * 1. Name is a process name.
   * 2. Name is a process name plus the axis extension.
   * 3. Name is a file name where everything but the extension (starts with last
   *    '.') is a process name.
   * 4. Name is a file name where everything but the extension (starts with last
   *    '.') is a process name plus the axis extension.
   * @param name
   * @param axisID
   * @return
   */
  public static ProcessName getInstance(String name, AxisID axisID) {
    //check if name equals process name
    ProcessName processName = getInstance(name);
    if (processName != null) {
      return processName;
    }
    //check if name is process name plus axis extension
    if (!axisID.getExtension().equals("")
        && name.endsWith(axisID.getExtension())) {
      processName = getInstance(name.substring(0, name.length()
          - axisID.getExtension().length()));
      if (processName != null) {
        return processName;
      }
    }
    int extIndex = name.lastIndexOf('.');
    if (extIndex == -1) {
      return null;
    }
    name = name.substring(0, extIndex);
    //check if file name equals process name plus file extension
     processName = getInstance(name);
    if (processName != null) {
      return processName;
    }
    //check if file name is process name plus axis extension plus file extension
    if (!axisID.getExtension().equals("")
        && name.endsWith(axisID.getExtension())) {
      processName = getInstance(name.substring(0, name.length()
          - axisID.getExtension().length()));
      if (processName != null) {
        return processName;
      }
    }
    return null;
  }
  
  /**
   * If extension matches the end of name, strip off extension at the end of
   * name before returning a call to getInstance(name, axisID).
   * @param name
   * @param axisID
   * @param extension
   * @return
   */
  public static ProcessName getInstance(String name, AxisID axisID,
      String extension) {
    if (extension==null||extension.equals("")) {
      return getInstance(name,axisID);
    }
    int extIndex = name.lastIndexOf(extension);
    if (extIndex==-1) {
      return getInstance(name,axisID);
    }
    return getInstance(name.substring(0,extIndex),axisID);
  }

  /**
   * Takes a string representation of an ProcessName type and returns the 
   * correct
   * static object.  The string is case insensitive.  Null is returned if the
   * string is not one of the known process names.
   */
  private static ProcessName getInstance(String name) {
    if (name == null) {
      return null;
    }
    if (name.endsWith(DatasetFiles.COMSCRIPT_EXT)) {
      int extIndex = name.lastIndexOf(DatasetFiles.COMSCRIPT_EXT);
      name = name.substring(0, extIndex);
    }
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
    if (name.compareToIgnoreCase(solvematch) == 0) {
      return SOLVEMATCH;
    }
    if (name.compareToIgnoreCase(startjoin) == 0) {
      return STARTJOIN;
    }
    if (name.compareToIgnoreCase(processchunks) == 0) {
      return PROCESSCHUNKS;
    }
    if (name.compareToIgnoreCase(tomosnapshot) == 0) {
      return TOMOSNAPSHOT;
    }
    if (name.compareToIgnoreCase(transferfid) == 0) {
      return TRANSFERFID;
    }
    if (name.compareToIgnoreCase(clipflipyz) == 0) {
      return CLIPFLIPYZ;
    }
    if (name.compareToIgnoreCase(finishjoin) == 0) {
      return FINISHJOIN;
    }
    if (name.compareToIgnoreCase(makejoincom) == 0) {
      return MAKEJOINCOM;
    }
    if (name.compareToIgnoreCase(trimvol) == 0) {
      return TRIMVOL;
    }
    if (name.compareToIgnoreCase(squeezevol) == 0) {
      return SQUEEZEVOL;
    }
    if (name.compareToIgnoreCase(archiveorig) == 0) {
      return ARCHIVEORIG;
    }
    if (name.compareToIgnoreCase(splittilt) == 0) {
      return SPLITTILT;
    }
    if (name.compareToIgnoreCase(splitcombine) == 0) {
      return SPLITCOMBINE;
    }
    if (name.compareToIgnoreCase(extractmagrad) == 0) {
      return EXTRACTMAGRAD;
    }
    if (name.compareToIgnoreCase(extracttilts) == 0) {
      return EXTRACTTILTS;
    }
    if (name.compareToIgnoreCase(extractpieces) == 0) {
      return EXTRACTPIECES;
    }
    if (name.compareToIgnoreCase(xfalign) == 0) {
      return XFALIGN;
    }
    if (name.compareToIgnoreCase(xfjointomo) == 0) {
      return XFJOINTOMO;
    }
    if (name.compareToIgnoreCase(xftoxg) == 0) {
      return XFTOXG;
    }
    if (name.compareToIgnoreCase(xfmodel) == 0) {
      return XFMODEL;
    }
    if (name.compareToIgnoreCase(remapmodel) == 0) {
      return REMAPMODEL;
    }
    if (name.compareToIgnoreCase(peetParser) == 0) {
      return PEET_PARSER;
    }
    if (name.compareToIgnoreCase(anisotropicDiffusion) == 0) {
      return ANISOTROPIC_DIFFUSION;
    }
    if (name.compareToIgnoreCase(chunksetup) == 0) {
      return CHUNKSETUP;
    }
    if (name.compareToIgnoreCase(ctfPlotter) == 0) {
      return CTF_PLOTTER;
    }
    if (name.compareToIgnoreCase(ctfCorrection) == 0) {
      return CTF_CORRECTION;
    }
    if (name.compareToIgnoreCase(splitCorrection) == 0) {
      return SPLIT_CORRECTION;
    }
    if (name.compareToIgnoreCase(ccderaser) == 0) {
      return CCD_ERASER;
    }
    if (name.compareToIgnoreCase(clip) == 0) {
      return CLIP;
    }
    return null;
  }

  public static ProcessName getInstance(File file, String excludeString) {
    if (file == null) {
      return null;
    }
    String fileName = file.getName();
    StringBuffer processStringBuffer = new StringBuffer(fileName.substring(0,
        fileName.lastIndexOf(excludeString)));
    ProcessName processName;
    if ((processName = ProcessName.getInstance(processStringBuffer.toString())) != null) {
      return processName;
    }
    if (processStringBuffer.toString().endsWith(AxisID.FIRST.getExtension())) {
      return ProcessName.getInstance(processStringBuffer
          .substring(processStringBuffer.lastIndexOf(AxisID.FIRST
              .getExtension())));
    }
    if (processStringBuffer.toString().endsWith(AxisID.SECOND.getExtension())) {
      return ProcessName.getInstance(processStringBuffer
          .substring(processStringBuffer.lastIndexOf(AxisID.SECOND
              .getExtension())));
    }
    return null;
  }

  public String getComscript(AxisID axisID) {
    return name + axisID.getExtension() + DatasetFiles.COMSCRIPT_EXT;
  }

  public String[] getComscriptArray(AxisID axisID) {
    return new String[] { name + axisID.getExtension()
        + DatasetFiles.COMSCRIPT_EXT };
  }
}
