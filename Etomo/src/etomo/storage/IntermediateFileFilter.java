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
 */
public class IntermediateFileFilter extends FileFilter {
  public static final String rcsid = "$Id$";
  private String fileset;
  public IntermediateFileFilter(String filesetName) {
    fileset = filesetName;
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
      if(f.getAbsolutePath().endsWith(fileset + "a.rec")) {
        return true;
      }
      if(f.getAbsolutePath().endsWith(fileset + "b.rec")) {
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
