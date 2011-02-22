package etomo.ui.swing;

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Font;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.WindowConstants;

/**
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
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 3.5  2010/02/18 22:34:22  sueh
 * <p> bug# 1315 Made it possible to pass a File to setFile.  This allow for a shorter
 * <p> title by using getName.  The FileReader requests an absolute file path.
 * <p>
 * <p> Revision 3.4  2008/03/22 00:19:00  sueh
 * <p> bug# 1099 Not using StyledEditorKit anymore because it takes up a lot
 * <p> of memory and mostly runs from the event loop, meaning the resulting
 * <p> memory errors cannot be caught and cause Etomo to lock up.  Setting the
 * <p> font in editorPane instead.
 * <p>
 * <p> Revision 3.3  2004/04/08 19:07:53  rickg
 * <p> Bug #422 added setDefaultCloseOperation call to constructor
 * <p>
 * <p> Revision 3.2  2003/11/27 00:04:53  rickg
 * <p> Bug# 366 Close file reader when done
 * <p>
 * <p> Revision 3.1  2003/11/10 07:46:32  rickg
 * <p> Task tags moved to bugzilla
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.2  2003/03/20 17:44:32  rickg
 * <p> setFile now returns a boolean if it was successful
 * <p>
 * <p> Revision 2.1  2003/03/06 05:53:28  rickg
 * <p> Combine interface in progress
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.1.2.1  2003/01/24 18:43:37  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
public class TextPageWindow extends JFrame {
  public static final String rcsid = "$Id$";

  Container mainPanel;
  String filename;
  JEditorPane editorPane = new JEditorPane();
  JScrollPane scrollPane = new JScrollPane(editorPane);
  FileReader reader;

  public TextPageWindow() {
    editorPane.setFont(new Font("monospaced", Font.PLAIN, 12));
    //editorPane.setEditorKit(new StyledEditorKit());
    mainPanel = getContentPane();
    mainPanel.add(scrollPane, BorderLayout.CENTER);
    setSize(625, 800);
    setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
  }

  public boolean setFile(File file) {
    this.filename = file.getAbsolutePath();
    setTitle(file.getName());
    return setFile();
  }

  public boolean setFile(String fileName) {
    this.filename = fileName;
    setTitle(fileName);
    return setFile();
  }

  public boolean setFile() {
    try {
      reader = new FileReader(filename);
      editorPane.read(reader, filename);
      editorPane.setEditable(false);
      reader.close();
    }
    catch (FileNotFoundException except) {
      String[] messages = new String[2];
      messages[0] = except.getMessage();
      messages[1] = "Make sure that " + filename + " is available";

      JOptionPane.showMessageDialog(null, messages, filename + " not found",
          JOptionPane.ERROR_MESSAGE);
      return false;
    }
    catch (IOException except) {
      JOptionPane.showMessageDialog(null, except.getMessage(),
          filename + " IO Exception", JOptionPane.ERROR_MESSAGE);
      return false;
    }
    return true;
  }
}
