package etomo.ui;

import java.awt.BorderLayout;
import java.awt.Container;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.text.StyledEditorKit;

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
  public static final String rcsid =
    "$Id$";

  Container mainPanel;
  String filename;
  JEditorPane editorPane = new JEditorPane();
  JScrollPane scrollPane = new JScrollPane(editorPane);
  FileReader reader;

  public TextPageWindow() {
    editorPane.setEditorKit(new StyledEditorKit());
    mainPanel = getContentPane();
    mainPanel.add(scrollPane, BorderLayout.CENTER);
    //  TODO: make the window size setable in properties
    setSize(625, 800);
  }

  public boolean setFile(String filename) {
    this.filename = filename;
    setTitle(filename);

    try {
      reader = new FileReader(filename);
      editorPane.read(reader, filename);
      editorPane.setEditable(false);
    }
    catch (FileNotFoundException except) {
      String[] messages = new String[2];
      messages[0] = except.getMessage();
      messages[1] = "Make sure that " + filename + " is available";

      JOptionPane.showMessageDialog(
        null,
        messages,
        filename + " not found",
        JOptionPane.ERROR_MESSAGE);
      return false;
    }
    catch (IOException except) {
      JOptionPane.showMessageDialog(
        null,
        except.getMessage(),
        filename + " IO Exception",
        JOptionPane.ERROR_MESSAGE);
      return false;
    }
    return true;
  }
}
