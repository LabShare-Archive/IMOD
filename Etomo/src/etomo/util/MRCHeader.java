package etomo.util;

import java.io.IOException;

import etomo.process.SystemProgram;

/**
 * <p>Description: An interface to the header information in a MRC Image 
 * file.</p>
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
public class MRCHeader {
  private String filename;
  private int nColumns = -1;
  private int nRows = -1;
  private int nSections = -1;

  public MRCHeader(String name) {
    filename = new String(name);
  }

  public void read() throws IOException {

    if (filename == null || filename == "") {
      throw new IOException("No filename specified");
    }

    // Run the header command on the filename
    SystemProgram header = new SystemProgram("header " + filename);
    header.run();

    // Throw an exception if the file can not be read
    String[] stdError = header.getStdError();
    if (stdError.length > 0) {
      String message = "";
      for (int i = 0; i < stdError.length; i++) {
        message = message + stdError[i] + "\n";
      }
      throw new IOException(message);
    }

    // Parse the output
    String[] stdOutput = header.getStdOutput();
    if (stdOutput.length < 1) {
      throw new IOException("header returned no data");
    }

    for (int i = 0; i < stdOutput.length; i++) {
      if (stdOutput[i].startsWith("Number of columns, rows, section")) {
        String[] tokens = stdOutput[i].split("\\s+");
        if (tokens.length < 4) {
          throw new IOException(
            "Header returned less than three parameters for image size");
        }
        nRows = Integer.parseInt(tokens[1]);
        nColumns = Integer.parseInt(tokens[2]);
        nSections = Integer.parseInt(tokens[3]);
      }
    }
  }
  /**
   * Returns the nColumns.
   * @return int
   */
  public int getNColumns() {
    return nColumns;
  }

  /**
   * Returns the nRows.
   * @return int
   */
  public int getNRows() {
    return nRows;
  }

  /**
   * Returns the nSections.
   * @return int
   */
  public int getNSections() {
    return nSections;
  }

  /**
   * Returns the filename.
   * @return String
   */
  public String getFilename() {
    return filename;
  }

  /**
   * Sets the filename.
   * @param filename The filename to set
   */
  public void setFilename(String filename) {
    this.filename = filename;
  }

}
