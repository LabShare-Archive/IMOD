package etomo.storage.autodoc;

import etomo.storage.LogFile;
import etomo.ui.Token;

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
 * <p> $Log$ </p>
 */
public interface WritableAutodoc extends ReadOnlyAutodoc {
  public static final String rcsid = "$Id$";

  public void addAttributeAndNameValuePair(String name,
      String value);
  public WritableAttribute getWritableAttribute(String name);
  public void write() throws LogFile.FileException, LogFile.WriteException;
  public void addComment(Token comment);
  public void addEmptyLine();
  public void addComment(String comment);
}
