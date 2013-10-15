package etomo.storage;

import java.io.File;
import java.util.regex.Pattern;

import javax.swing.filechooser.FileFilter;

import etomo.type.FileType;

/*
 * <p>Description: </p>
 *
 * <p>Copyright: Copyright (c) 2002</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 * <p> $Log$
 * <p> Revision 3.8  2011/04/04 16:58:01  sueh
 * <p> bug# 1416 Added .alisub and .alilog10 to accept.
 * <p>
 * <p> Revision 3.7  2011/02/22 04:34:39  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 3.6  2006/07/24 14:07:42  sueh
 * <p> bug# 878 Added .dcst to the files accepted by this filter.
 * <p>
 * <p> Revision 3.5  2005/12/13 00:29:49  sueh
 * <p> bug# 618 Made the file selection for files with dashes more selective.
 * <p>
 * <p> Revision 3.4  2005/12/07 17:43:07  sueh
 * <p> bug# 618 Was not handling dashes.  Made the code that recognizes
 * <p> parallel processing file more specific.
 * <p>
 * <p> Revision 3.3  2005/10/17 23:52:54  sueh
 * <p> bug# 532 Handling intermediate files created by splittilt, splitcombine, and
 * <p> processchunks in accept().
 * <p>
 * <p> Revision 3.2  2005/04/06 21:26:34  sueh
 * <p> bug# 533 Added the .bl file to the clean up panel.
 * <p>
 * <p> Revision 3.1  2005/03/29 23:50:39  sueh
 * <p> bug# 618 Added acceptPretrimmedTomograms boolean to configure
 * <p> whether the class will show sum.rec or _full.rec.
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 1.4  2003/11/06 18:30:40  sueh
 * <p> bug321 accept(File f): added matchcheck.rec
 * <p>
 * <p> Revision 1.3  2003/04/24 17:46:54  rickg
 * <p> Changed fileset name to dataset name
 * <p>
 * <p> Revision 1.2  2003/04/17 17:19:11  rickg
 * <p> Reformat
 * <p>
 * <p> Revision 1.1  2003/04/17 05:06:06  rickg
 * <p> Initial revision
 * <p>
 */
public class IntermediateFileFilter extends FileFilter {
  public static final String rcsid = "$Id$";
  private String datasetName;
  boolean acceptPretrimmedTomograms = false;

  public IntermediateFileFilter(String datasetName) {
    this.datasetName = datasetName;
  }

  /* (non-Javadoc)
   * @see javax.swing.filechooser.FileFilter#accept(java.io.File) */
  public boolean accept(File f) {
    String[] endsWith = { "~", "matchcheck.rec", ".mat", ".ali", ".preali", "bot.rec",
        "bota.rec", "botb.rec", "mid.rec", "mida.rec", "midb.rec", "top.rec", "topa.rec",
        "topb.rec", "volcombine.log", ".bl", ".dcst", ".alilog10", ".vsr" };
    String[] pretrimmedTomograms = { "sum.rec", "full.rec" };
    if (f.isFile()) {
      // .rec.mat1659344 and .rec.wrp0905524
      String name = f.getName();
      if (name.matches("\\S+" + Pattern.quote(".rec.mat") + "\\S+")
          || name.matches("\\S+" + Pattern.quote(".rec.wrp") + "\\S+")) {
        return true;
      }
      String path = f.getAbsolutePath();
      for (int i = 0; i < endsWith.length; i++) {
        if (path.endsWith(endsWith[i])) {
          return true;
        }
      }
      if (acceptPretrimmedTomograms) {
        for (int i = 0; i < pretrimmedTomograms.length; i++) {
          if (path.endsWith(pretrimmedTomograms[i])) {
            return true;
          }
        }
      }
      if (path.endsWith(datasetName + "a.rec")) {
        return true;
      }
      if (path.endsWith(datasetName + "b.rec")) {
        return true;
      }
      // handle split... and processchunks files
      if (name.matches(datasetName + "[ab]?-\\d\\d\\d\\.rec")) {
        return true;
      }
      if (name.matches("tilt[ab]?-\\d\\d\\d\\.log")) {
        return true;
      }
      if (name.matches("tilt[ab]?-\\d\\d\\d\\.com")) {
        return true;
      }
      if (name.matches("volcombine[ab]?-\\d\\d\\d\\.log")) {
        return true;
      }
      if (name.matches("volcombine[ab]?-\\d\\d\\d\\.com")) {
        return true;
      }
      if (accept(name, FileType.ERASED_BEADS_STACK)) {
        return true;
      }
      if (accept(name, FileType.FIXED_XRAYS_STACK)) {
        return true;
      }
      if (accept(name, FileType.CTF_CORRECTED_STACK)) {
        return true;
      }
      if (accept(name, FileType.MTF_FILTERED_STACK)) {
        return true;
      }
      if (name.matches(datasetName + "[ab]?_full\\.vsr\\d\\d")) {
        return true;
      }
      if (name.matches(datasetName + "[ab]?_sub\\.vsr\\d\\d")) {
        return true;
      }
    }
    return false;
  }

  /**
   * Returns true if fileName matches file type.  Only works for file types that use the
   * dataset and the axisID.
   * dataset and axisID
   * @param fileName
   * @param fileType
   * @return
   */
  private boolean accept(final String fileName, final FileType fileType) {
    if (!fileType.usesDataset() || !fileType.usesAxisID()) {
      return false;
    }
    if (fileName.endsWith(fileType.getTypeString() + fileType.getExtension())) {
      return true;
    }
    return false;
  }

  /* (non-Javadoc)
   * @see javax.swing.filechooser.FileFilter#getDescription() */
  public String getDescription() {
    return "Intermediate files";
  }

  public void setAcceptPretrimmedTomograms(boolean acceptPretrimmedTomograms) {
    this.acceptPretrimmedTomograms = acceptPretrimmedTomograms;
  }
}
