package etomo.storage;

import java.io.File;

import javax.swing.filechooser.FileFilter;

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
  public static final String rcsid =
    "$Id$";
  private String datasetName;
  
  public IntermediateFileFilter(String datasetName) {
    this.datasetName = datasetName;
  }
  /* (non-Javadoc)
   * @see javax.swing.filechooser.FileFilter#accept(java.io.File)
   */
  public boolean accept(File f) {
    String[] endsWith =
      {
        "~",
        "sum.rec",
        "full.rec",
        "matchcheck.rec",
        ".mat",
        ".ali",
        ".preali",
        "bot.rec",
        "bota.rec",
        "botb.rec",
        "mid.rec",
        "mida.rec",
        "midb.rec",
        "top.rec",
        "topa.rec",
        "topb.rec",
        "volcombine.log" };
    if (f.isFile()) {
      for (int i = 0; i < endsWith.length; i++) {
        if (f.getAbsolutePath().endsWith(endsWith[i])) {
          return true;
        }
      }
      if (f.getAbsolutePath().endsWith(datasetName + "a.rec")) {
        return true;
      }
      if (f.getAbsolutePath().endsWith(datasetName + "b.rec")) {
        return true;
      }
    }
    return false;
  }

  /* (non-Javadoc)
   * @see javax.swing.filechooser.FileFilter#getDescription()
   */
  public String getDescription() {
    return "Intermediate files";
  }

}
