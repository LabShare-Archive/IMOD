package etomo.ui.swing;

import javax.swing.text.BadLocationException;

import etomo.storage.Loggable;
import etomo.type.AxisID;

/**
 * <p>Description: An interface for anything that can act as a log display.
 * Used by classes that have messages to log.  Also used by EtomoLogger which is
 * a utility for LogInterface classes.
 * 
 * <p>Copyright: Copyright 2010</p>
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
 * <p> Revision 1.1  2010/02/17 04:56:35  sueh
 * <p> bug# 1301 An interface for classes that receive messages to log.
 * <p> </p>
 */
public interface LogInterface {
  public static final String rcsid = "$Id$";

  public void logMessage(String title, AxisID axisID, String[] message);

  public void logMessage(Loggable loggable, AxisID axisID);

  public void save();
  
  //Functions used by EtomoLogger

  public void append(String line);

  public void msgChanged();

  public int getLineEndOffset() throws BadLocationException;
}
