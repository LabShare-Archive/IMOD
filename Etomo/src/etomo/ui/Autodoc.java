package etomo.ui;

import etomo.util.Utilities;

import java.io.File;
import java.io.FileNotFoundException;
import java.lang.IllegalArgumentException;
import java.io.IOException;

/**
* <p>Description:</p>
*
* <p>Copyright: Copyright © 2002, 2003</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
*
* @author $$Author$$
*
* @version $$Revision$$
*
* <p> $$Log$
* <p> $Revision 1.1  2003/12/22 23:47:45  sueh
* <p> $bug# 372 Autodoc contains informatio from the autodoc file.
* <p> $It instantiates at most one per type of autodoc file.
* <p> $$ </p>
*/

public class Autodoc {
  public static final String rcsid =
    "$$Id$$";

  public static final String PATH_ENVIRONMENT_VARIABLE =
    new String("AUTODOC_PATH");
  public static final String TILTXCORR = new String("tiltxcorr");

  private static final String fileExt = new String(".adoc");

  private static Autodoc tiltxcorr = null;

  private String dirName = null;
  private String fileName = null;
  private File file = null;
  private AutodocParser parser = null;
  private boolean test = false;

  public static Autodoc get(String name)
    throws FileNotFoundException, IOException {
    if (name == null) {
      throw new IllegalArgumentException("name is null.");
    }
    if (name.equals(TILTXCORR)) {
      tiltxcorr = getAutodoc(tiltxcorr, name);
      return tiltxcorr;
    }
    throw new IllegalArgumentException("Illegal autodoc name: " + name + ".");
  }

  public String getName() {
    return fileName;
  }

  private static Autodoc getAutodoc(Autodoc autodoc, String name)
    throws FileNotFoundException, IOException {
    if (autodoc == null) {
      autodoc = new Autodoc(name);
      autodoc.initialize();
    }
    return autodoc;
  }

  private Autodoc(String name) {
    fileName = new String(name + fileExt);
    dirName =
      new String(Utilities.getEnvironmentVariable(PATH_ENVIRONMENT_VARIABLE));
    file = new File(dirName, fileName);
  }

  private void initialize() throws FileNotFoundException, IOException {
    String errorMessage = null;
    if (file == null) {
      errorMessage =
        "Unable to open autodoc file, " + fileName + " , in " + dirName + ".";
    }
    if (!file.exists()) {
      errorMessage =
        "The autodoc file, "
          + fileName
          + " , in "
          + dirName
          + " does not exist.";
    }
    if (!file.canRead()) {
      errorMessage =
        "The autodoc file, "
          + fileName
          + " , in "
          + dirName
          + " is not readable.";
    }
    if (errorMessage != null) {
      throw new FileNotFoundException(errorMessage);
    }
    parser = new AutodocParser(this, file);
    if (test) {
      //parser.testStreamTokenizer(false);
      //parser.testStreamTokenizer(true);
      //parser.testPrimativeTokenizer(false);
      //parser.testPrimativeTokenizer(true);
      //parser.testAutodocTokenizer(false);
      //parser.testAutodocTokenizer(true);
      parser.testPreprocessor(false);
      //parser.testPreprocessor(true);
    }
    else {
      parser.initialize();
      parser.parse();
    }
  }

}
