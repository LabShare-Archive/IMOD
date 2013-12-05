package etomo.storage.autodoc;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.HashMap;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.storage.AutodocFilter;
import etomo.storage.LogFile;
import etomo.type.AxisID;

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
 * <p> Revision 1.19  2011/02/18 22:51:44  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.18  2010/03/05 03:58:58  sueh
 * <p> bug# 1319 Added the tilt autodoc.
 * <p>
 * <p> Revision 1.17  2010/02/17 04:49:43  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.16  2009/09/01 03:18:16  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.15  2009/06/05 02:01:14  sueh
 * <p> bug# 1219 Added FLATTEN_WARP, FLATTEN_WARP_INSTANCE,
 * <p> WARP_VOL, and WARP_VOL_INSTANCE.
 * <p>
 * <p> Revision 1.14  2009/03/17 00:45:53  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.13  2009/03/09 17:27:54  sueh
 * <p> bug# 1199 Added a getInstance function that takes a File and doesn't
 * <p> require a version.
 * <p>
 * <p> Revision 1.12  2009/02/04 23:30:00  sueh
 * <p> bug# 1158 Changed id and exceptions classes in LogFile.
 * <p>
 * <p> Revision 1.11  2009/01/20 19:33:30  sueh
 * <p> bug# 1102 In getInstance(String,AxisID) added else to if name equals CPU.
 * <p>
 * <p> Revision 1.10  2008/10/27 18:35:26  sueh
 * <p> bug# 1141 Added ctfplotter and ctfcorrection
 * <p>
 * <p> Revision 1.9  2008/05/30 21:23:42  sueh
 * <p> bug# 1102 Added writable.  Will be used to limit functionality of non-
 * <p> matlab autodocs to original autodoc definition.
 * <p>
 * <p> Revision 1.8  2008/01/31 20:24:49  sueh
 * <p> bug# 1055 throwing a FileException when LogFile.getInstance fails.
 * <p>
 * <p> Revision 1.7  2007/08/16 16:31:55  sueh
 * <p> bug# 1035 Added NEWSTACK.
 * <p>
 * <p> Revision 1.6  2007/06/07 21:31:52  sueh
 * <p> bug# 1012 Added function getMatlabDebugInstance.
 * <p>
 * <p> Revision 1.5  2007/04/13 18:42:59  sueh
 * <p> bug# 964 Added getDebugInstance(String, AxisID).
 * <p>
 * <p> Revision 1.4  2007/03/26 23:33:52  sueh
 * <p> bug# 964 Added getInstance(String name) which opens an n'ton autodoc with
 * <p> AxisID.ONLY.
 * <p>
 * <p> Revision 1.3  2007/03/26 18:36:41  sueh
 * <p> bug# 964 Made Version optional so that it is not necessary in matlab param files.
 * <p>
 * <p> Revision 1.2  2007/03/23 20:32:07  sueh
 * <p> bug# 964 Added PEET_PRM - an autodoc which contains Feld sections that
 * <p> represent the fields that may be used in the PEET .prm file.
 * <p>
 * <p> Revision 1.1  2007/03/21 18:14:50  sueh
 * <p> bug# 964 Limiting access to autodoc classes by using ReadOnly interfaces.
 * <p> Added AutodocFactory to create Autodoc instances.
 * <p> </p>
 */
public final class AutodocFactory {
  public static final String rcsid = "$Id$";

  public static final String EXTENSION = ".adoc";

  public static final String VERSION = "1.2";
  public static final String TILTXCORR = "tiltxcorr";
  public static final String MTF_FILTER = "mtffilter";
  public static final String COMBINE_FFT = "combinefft";
  public static final String TILTALIGN = "tiltalign";
  public static final String CCDERASER = "ccderaser";
  public static final String SOLVEMATCH = "solvematch";
  public static final String BEADTRACK = "beadtrack";
  public static final String DENS_MATCH = "densmatch";
  public static final String CORR_SEARCH_3D = "corrsearch3d";
  public static final String XFJOINTOMO = "xfjointomo";
  public static final String CPU = "cpu";
  public static final String UITEST = "uitest";
  public static final String PEET_PRM = "peetprm";
  public static final String NEWSTACK = "newstack";
  public static final String CTF_PLOTTER = "ctfplotter";
  public static final String CTF_PHASE_FLIP = "ctfphaseflip";
  public static final String FLATTEN_WARP = "flattenwarp";
  public static final String WARP_VOL = "warpvol";
  public static final String FIND_BEADS_3D = "findbeads3d";
  public static final String TILT = "tilt";
  public static final String SIRTSETUP = "sirtsetup";
  public static final String BLENDMONT = "blendmont";
  public static final String XFTOXG = "xftoxg";
  public static final String XFALIGN = "xfalign";
  public static final String AUTOFIDSEED = "autofidseed";
  public static final String ETOMO = "etomo";

  private static final String TEST = "test";
  private static final String UITEST_AXIS = "uitest_axis";

  private static Autodoc TILTXCORR_INSTANCE = null;
  private static Autodoc TEST_INSTANCE = null;
  private static Autodoc UITEST_INSTANCE = null;
  private static Autodoc MTF_FILTER_INSTANCE = null;
  private static Autodoc COMBINE_FFT_INSTANCE = null;
  private static Autodoc TILTALIGN_INSTANCE = null;
  private static Autodoc CCDERASER_INSTANCE = null;
  private static Autodoc SOLVEMATCH_INSTANCE = null;
  private static Autodoc BEADTRACK_INSTANCE = null;
  private static Autodoc CPU_INSTANCE = null;
  private static Autodoc DENS_MATCH_INSTANCE = null;
  private static Autodoc CORR_SEARCH_3D_INSTANCE = null;
  private static Autodoc XFJOINTOMO_INSTANCE = null;
  private static Autodoc PEET_PRM_INSTANCE = null;
  private static Autodoc NEWSTACK_INSTANCE = null;
  private static Autodoc CTF_PLOTTER_INSTANCE = null;
  private static Autodoc CTF_PHASE_FLIP_INSTANCE = null;
  private static Autodoc FLATTEN_WARP_INSTANCE = null;
  private static Autodoc WARP_VOL_INSTANCE = null;
  private static Autodoc FIND_BEADS_3D_INSTANCE = null;
  private static Autodoc TILT_INSTANCE = null;
  private static Autodoc SIRTSETUP_INSTANCE = null;
  private static Autodoc BLENDMONT_INSTANCE = null;
  private static Autodoc XFTOXG_INSTANCE = null;
  private static Autodoc XFALIGN_INSTANCE = null;
  private static Autodoc AUTOFIDSEED_INSTANCE = null;
  private static Autodoc ETOMO_INSTANCE = null;

  private static final HashMap UITEST_AXIS_MAP = new HashMap();

  private AutodocFactory() {
  }

  public static ReadOnlyAutodoc getInstance(BaseManager manager, String name)
      throws FileNotFoundException, IOException, LogFile.LockException {
    return getInstance(manager, name, AxisID.ONLY);
  }

  public static ReadOnlyAutodoc getInstance(BaseManager manager, String name,
      AxisID axisID) throws FileNotFoundException, IOException, LogFile.LockException {
    if (name == null) {
      throw new IllegalStateException("name is null");
    }
    Autodoc autodoc = getExistingAutodoc(name);
    if (autodoc != null) {
      return autodoc;
    }
    autodoc = new Autodoc(name);
    if (name.equals(UITEST)) {
      autodoc.initializeUITest(manager, name, axisID);
    }
    else if (name.equals(CPU)) {
      autodoc.initializeCpu(manager, name, axisID);
    }
    else {
      autodoc.initialize(manager, name, axisID);
    }
    return autodoc;
  }

  public static ReadOnlyAutodoc getDebugInstance(BaseManager manager, String name,
      AxisID axisID) throws FileNotFoundException, IOException, LogFile.LockException {
    if (name == null) {
      throw new IllegalStateException("name is null");
    }
    Autodoc autodoc = getExistingAutodoc(name);
    if (autodoc != null) {
      return autodoc;
    }
    autodoc = new Autodoc(name);
    autodoc.setDebug(true);
    if (name.equals(UITEST)) {
      autodoc.initializeUITest(manager, name, axisID);
    }
    if (name.equals(CPU)) {
      autodoc.initializeCpu(manager, name, axisID);
    }
    else {
      autodoc.initialize(manager, name, axisID);
    }
    return autodoc;
  }

  public static WritableAutodoc getMatlabDebugInstance(BaseManager manager, File file)
      throws IOException, LogFile.LockException {
    if (file == null) {
      throw new IllegalStateException("file is null");
    }
    Autodoc autodoc = new Autodoc(true, stripFileExtension(file));
    autodoc.setDebug(true);
    try {
      autodoc.initialize(manager, file, true, false, true, true);
      return autodoc;
    }
    catch (FileNotFoundException e) {
      return null;
    }
  }

  public static WritableAutodoc getMatlabInstance(BaseManager manager, File file)
      throws IOException, LogFile.LockException {
    if (file == null) {
      throw new IllegalStateException("file is null");
    }
    Autodoc autodoc = new Autodoc(true, stripFileExtension(file));
    try {
      autodoc.initialize(manager, file, true, false, true, true);
      return autodoc;
    }
    catch (FileNotFoundException e) {
      return null;
    }
  }

  private static String stripFileExtension(File file) {
    return stripFileExtension(file.getName());
  }

  private static String stripFileExtension(String fileName) {
    int extensionIndex = fileName.lastIndexOf('.');
    if (extensionIndex == -1) {
      return fileName;
    }
    return fileName.substring(0, extensionIndex);
  }

  public static WritableAutodoc getEmptyMatlabInstance(BaseManager manager, File file)
      throws IOException, LogFile.LockException {
    if (file == null) {
      throw new IllegalStateException("file is null");
    }
    Autodoc autodoc = new Autodoc(true, stripFileExtension(file));
    try {
      autodoc.initialize(manager, file, false, false, true, true);
      return autodoc;
    }
    catch (FileNotFoundException e) {
      return null;
    }
  }

  public static ReadOnlyAutodoc getInstance(BaseManager manager, File file)
      throws IOException, LogFile.LockException {
    return getInstance(manager, file, true);
  }

  public static ReadOnlyAutodoc getInstance(BaseManager manager, File file,
      boolean versionRequired) throws IOException, LogFile.LockException {
    if (file == null) {
      throw new IllegalStateException("file is null");
    }
    Autodoc autodoc = new Autodoc(stripFileExtension(file));
    try {
      autodoc.initialize(manager, file, true, versionRequired, false, false);
      return autodoc;
    }
    catch (FileNotFoundException e) {
      return null;
    }
  }

  /**
   * For testing initializes but doesn't parse and store data.  Call
   * runInternalTest on the resulting instance.
   * @param file
   * @param storeData
   * @return
   * @throws IOException
   * @throws LogFile.ReadException
   */
  public static ReadOnlyAutodoc getTestInstance(BaseManager manager, File file)
      throws IOException, LogFile.LockException {
    if (file == null) {
      throw new IllegalStateException("file is null");
    }
    Autodoc autodoc = new Autodoc(stripFileExtension(file));
    try {
      autodoc.initialize(manager, file, false, true, false, false);
      return autodoc;
    }
    catch (FileNotFoundException e) {
      return null;
    }
  }

  /**
   * open and preserve an autodoc without a type for testing
   * @param autodocFile
   * @param axisID
   * @return
   * @throws FileNotFoundException
   * @throws IOException
   */
  public static Autodoc getTestInstance(BaseManager manager, File directory,
      String autodocFileName, AxisID axisID) throws FileNotFoundException, IOException,
      LogFile.LockException {
    if (autodocFileName == null) {
      return null;
    }
    File autodocFile = new File(directory, autodocFileName);
    AutodocFilter filter = new AutodocFilter();
    if (!filter.accept(autodocFile)) {
      throw new IllegalArgumentException(autodocFile + " is not an autodoc.");
    }
    if (EtomoDirector.INSTANCE.getArguments().isTest()) {
      System.err.println("autodoc file:" + autodocFile.getAbsolutePath());
    }
    Autodoc autodoc = getExistingUITestAxisAutodoc(autodocFile);
    if (autodoc != null) {
      return autodoc;
    }
    autodoc = new Autodoc(stripFileExtension(autodocFileName));
    UITEST_AXIS_MAP.put(autodocFile, autodoc);
    autodoc.initializeUITestAxis(manager, LogFile.getInstance(autodocFile), axisID);
    return autodoc;
  }

  /**
   * open and return an autodoc without a type.
   * @param autodocFile
   * @param axisID
   * @return
   * @throws FileNotFoundException
   * @throws IOException
   */
  public static Autodoc getInstance(final BaseManager manager, final File autodocFile,
      final AxisID axisID) throws FileNotFoundException, IOException,
      LogFile.LockException {
    if (autodocFile == null) {
      return null;
    }
    AutodocFilter filter = new AutodocFilter();
    if (!filter.accept(autodocFile)) {
      throw new IllegalArgumentException(autodocFile + " is not an autodoc.");
    }
    if (EtomoDirector.INSTANCE.getArguments().isTest()) {
      System.err.println("autodoc file:" + autodocFile.getAbsolutePath());
    }
    Autodoc autodoc = new Autodoc(stripFileExtension(autodocFile.getName()));
    autodoc.initialize(manager, LogFile.getInstance(autodocFile), axisID);
    return autodoc;
  }

  public static void setAbsoluteDir(String absoluteDir) {
    Autodoc.setAbsoluteDir(absoluteDir);
  }

  private static Autodoc getExistingUITestAxisAutodoc(File autodocFile) {
    if (UITEST_AXIS_MAP == null) {
      return null;
    }
    Autodoc autodoc = (Autodoc) UITEST_AXIS_MAP.get(autodocFile);
    return autodoc;
  }

  private static Autodoc getExistingAutodoc(String fileName, String name) {
    if (name.equals(UITEST_AXIS)) {
      if (UITEST_AXIS_MAP == null) {
        return null;
      }
      return (Autodoc) UITEST_AXIS_MAP.get(fileName);
    }
    throw new IllegalArgumentException("Illegal autodoc name: " + name + ".");
  }

  private static Autodoc getExistingAutodoc(String name) {
    if (name.equals(TILTXCORR)) {
      return TILTXCORR_INSTANCE;
    }
    if (name.equals(TEST)) {
      return TEST_INSTANCE;
    }
    if (name.equals(UITEST)) {
      return UITEST_INSTANCE;
    }
    if (name.equals(MTF_FILTER)) {
      return MTF_FILTER_INSTANCE;
    }
    if (name.equals(NEWSTACK)) {
      return NEWSTACK_INSTANCE;
    }
    if (name.equals(CTF_PLOTTER)) {
      return CTF_PLOTTER_INSTANCE;
    }
    if (name.equals(CTF_PHASE_FLIP)) {
      return CTF_PHASE_FLIP_INSTANCE;
    }
    if (name.equals(FLATTEN_WARP)) {
      return FLATTEN_WARP_INSTANCE;
    }
    if (name.equals(WARP_VOL)) {
      return WARP_VOL_INSTANCE;
    }
    if (name.equals(FIND_BEADS_3D)) {
      return FIND_BEADS_3D_INSTANCE;
    }
    if (name.equals(COMBINE_FFT)) {
      return COMBINE_FFT_INSTANCE;
    }
    if (name.equals(TILTALIGN)) {
      return TILTALIGN_INSTANCE;
    }
    if (name.equals(CCDERASER)) {
      return CCDERASER_INSTANCE;
    }
    if (name.equals(SOLVEMATCH)) {
      return SOLVEMATCH_INSTANCE;
    }
    if (name.equals(BEADTRACK)) {
      return BEADTRACK_INSTANCE;
    }
    if (name.equals(CPU)) {
      return CPU_INSTANCE;
    }
    if (name.equals(DENS_MATCH)) {
      return DENS_MATCH_INSTANCE;
    }
    if (name.equals(CORR_SEARCH_3D)) {
      return CORR_SEARCH_3D_INSTANCE;
    }
    if (name.equals(XFJOINTOMO)) {
      return XFJOINTOMO_INSTANCE;
    }
    if (name.equals(PEET_PRM)) {
      return PEET_PRM_INSTANCE;
    }
    if (name.equals(TILT)) {
      return TILT_INSTANCE;
    }
    if (name.equals(SIRTSETUP)) {
      return SIRTSETUP_INSTANCE;
    }
    if (name.equals(BLENDMONT)) {
      return BLENDMONT_INSTANCE;
    }
    if (name.equals(XFTOXG)) {
      return XFTOXG_INSTANCE;
    }
    if (name.equals(XFALIGN)) {
      return XFALIGN_INSTANCE;
    }
    if (name.equals(AUTOFIDSEED)) {
      return AUTOFIDSEED_INSTANCE;
    }
    if (name.equals(ETOMO)) {
      return ETOMO_INSTANCE;
    }
    throw new IllegalArgumentException("Illegal autodoc name: " + name + ".");
  }

  /**
   * for testing
   * @param name
   */
  public static void resetInstance(String name) {
    if (name.equals(TILTXCORR)) {
      TILTXCORR_INSTANCE = null;
    }
    else if (name.equals(TEST)) {
      TEST_INSTANCE = null;
    }
    else if (name.equals(UITEST)) {
      UITEST_INSTANCE = null;
    }
    else if (name.equals(MTF_FILTER)) {
      MTF_FILTER_INSTANCE = null;
    }
    else if (name.equals(NEWSTACK)) {
      NEWSTACK_INSTANCE = null;
    }
    else if (name.equals(CTF_PLOTTER)) {
      CTF_PLOTTER_INSTANCE = null;
    }
    else if (name.equals(CTF_PHASE_FLIP)) {
      CTF_PHASE_FLIP_INSTANCE = null;
    }
    else if (name.equals(FLATTEN_WARP)) {
      FLATTEN_WARP_INSTANCE = null;
    }
    else if (name.equals(WARP_VOL)) {
      WARP_VOL_INSTANCE = null;
    }
    else if (name.equals(FIND_BEADS_3D)) {
      FIND_BEADS_3D_INSTANCE = null;
    }
    else if (name.equals(COMBINE_FFT)) {
      COMBINE_FFT_INSTANCE = null;
    }
    else if (name.equals(TILTALIGN)) {
      TILTALIGN_INSTANCE = null;
    }
    else if (name.equals(CCDERASER)) {
      CCDERASER_INSTANCE = null;
    }
    else if (name.equals(SOLVEMATCH)) {
      SOLVEMATCH_INSTANCE = null;
    }
    else if (name.equals(BEADTRACK)) {
      BEADTRACK_INSTANCE = null;
    }
    else if (name.equals(CPU)) {
      CPU_INSTANCE = null;
    }
    else if (name.equals(DENS_MATCH)) {
      DENS_MATCH_INSTANCE = null;
    }
    else if (name.equals(CORR_SEARCH_3D)) {
      CORR_SEARCH_3D_INSTANCE = null;
    }
    else if (name.equals(XFJOINTOMO)) {
      XFJOINTOMO_INSTANCE = null;
    }
    else if (name.equals(PEET_PRM)) {
      PEET_PRM_INSTANCE = null;
    }
    else if (name.equals(TILT)) {
      TILT_INSTANCE = null;
    }
    else if (name.equals(SIRTSETUP)) {
      SIRTSETUP_INSTANCE = null;
    }
    else if (name.equals(BLENDMONT)) {
      BLENDMONT_INSTANCE = null;
    }
    else if (name.equals(XFTOXG)) {
      XFTOXG_INSTANCE = null;
    }
    else if (name.equals(XFALIGN)) {
      XFALIGN_INSTANCE = null;
    }
    else if (name.equals(AUTOFIDSEED)) {
      AUTOFIDSEED_INSTANCE = null;
    }
    else if (name.equals(ETOMO)) {
      ETOMO_INSTANCE = null;
    }
    else {
      throw new IllegalArgumentException("Illegal autodoc name: " + name + ".");
    }
  }
}
