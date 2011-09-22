package etomo.storage;

import java.io.File;
import java.io.FilenameFilter;

import javax.swing.filechooser.FileFilter;

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.type.FileType;

/**
* <p>Description: Gets files of two types:</p>
* <p>{dataset}{axis letter}.srec{int}</p>
* <p>If includeScaledOutput is true:  {dataset}{axis letter}.sint{int}</p>
* <p>The {int} must be a valid integer for a file name to be accepted.</p>
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
* <p> $Log$
* <p> Revision 1.2  2011/04/25 23:18:36  sueh
* <p> bug# 1416 Remove unnecessary interface java.io.FileFilter.
* <p>
* <p> Revision 1.1  2011/04/04 17:00:13  sueh
* <p> bug# 1416 File filter for sirt output files (.srecdd and .sintdd).
* <p> </p>
*/
public final class SirtOutputFileFilter extends FileFilter implements FilenameFilter {
  public static final String rcsid = "$Id$";

  private final BaseManager manager;
  private final AxisID axisID;
  private final boolean includeScaledOutput;
  private final boolean subarea;
  private final boolean full;

  public SirtOutputFileFilter(final BaseManager manager, final AxisID axisID,
      final boolean includeScaledOutput, final boolean subarea, final boolean full) {
    this.manager = manager;
    this.axisID = axisID;
    this.includeScaledOutput = includeScaledOutput;
    this.subarea = subarea;
    this.full = full;
  }

  public static SirtOutputFileFilter getSubareaInstance(final BaseManager manager,
      final AxisID axisID, final boolean includeScaledOutput) {
    return new SirtOutputFileFilter(manager, axisID, includeScaledOutput, true, false);
  }

  public static SirtOutputFileFilter getFullInstance(final BaseManager manager,
      final AxisID axisID, final boolean includeScaledOutput) {
    return new SirtOutputFileFilter(manager, axisID, includeScaledOutput, false, true);
  }

  @Override
  /**
   * Return true if the file name contains a SIRT output template followed by a valid
   * integer.
   */
  public boolean accept(final File file) {
    return accept(null, file.getName());
  }

  public boolean accept(final File dir, final String fileName) {
    if (fileName.endsWith("~")) {
      return false;
    }
    String template;
    if (full) {
      template = FileType.SIRT_OUTPUT_TEMPLATE.getTemplate(manager, axisID);
      if (fileName.startsWith(template)) {
        return acceptTemplate(fileName, template);
      }
    }
    if (subarea) {
      template = FileType.SIRT_SUBAREA_OUTPUT_TEMPLATE.getTemplate(manager, axisID);
      if (fileName.startsWith(template)) {
        return acceptTemplate(fileName, template);
      }
    }
    if (includeScaledOutput) {
      if (full) {
        template = FileType.SIRT_SCALED_OUTPUT_TEMPLATE.getTemplate(manager, axisID);
        if (fileName.startsWith(template)) {
          return acceptTemplate(fileName, template);
        }
      }
      if (subarea) {
        template = FileType.SIRT_SUBAREA_SCALED_OUTPUT_TEMPLATE.getTemplate(manager,
            axisID);
        if (fileName.startsWith(template)) {
          return acceptTemplate(fileName, template);
        }
      }
    }
    return false;
  }

  /**
   * The file name should be shorter then the template.  The part of the file name that
   * extends beyond the template should be a valid integer. 
   * @param fileName
   * @param template
   * @return
   */
  private boolean acceptTemplate(final String fileName, final String template) {
    int templateLength = template.length();
    if (fileName.length() <= templateLength) {
      return false;
    }
    try {
      Integer.getInteger(fileName.substring(templateLength));
    }
    catch (NumberFormatException e) {
      return false;
    }
    return true;
  }

  @Override
  public String getDescription() {
    return "SIRT Iteration Files";
  }
}
