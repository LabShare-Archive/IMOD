package etomo.storage;

import java.io.File;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.Vector;

import javax.swing.filechooser.FileFilter;
import javax.swing.filechooser.FileView;

/**
* <p>Description: A file filter that is a container for multiple file filters.</p>
* 
* <p>Copyright: Copyright 2011</p>
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
public final class FileFilterCollection extends FileFilter {
  public static final String rcsid = "$Id$";

  private final List fileFilterList = new Vector<FileFilter>();
  //Using a Set to avoid duplicate descriptions
  private final Set descriptionSet = new LinkedHashSet<String>();

  /**
   * Adds a file filter.
   * @param fileFilter
   */
  public void addFileFilter(final FileFilter fileFilter) {
    if (fileFilter == null) {
      return;
    }
    fileFilterList.add(fileFilter);
    descriptionSet.add(fileFilter.getDescription());
  }

  /**
   * Whether the given file is accepted by any of these filters.
   */
  public boolean accept(File file) {
    Iterator<FileFilter> i = fileFilterList.iterator();
    while (i.hasNext()) {
      if (i.next().accept(file)) {
        return true;
      }
    }
    return false;
  }

  /**
   * The description of these filters. For example:
   * "JPG and GIF Images, Nethack Save Files"
   * @see FileView#getName
   */
  public String getDescription() {
    String description = descriptionSet.toString();
    //Remove []
    return description.substring(1, description.length() - 1);
  }
}
