package etomo.ui;

import java.io.*;
import java.awt.*;
import javax.swing.*;
import javax.swing.text.html.*;

//FIXME the scrollbar does not work in this window when it is opened in a modal
//dialog box

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
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
public class HTMLPageWindow extends JFrame {
  public static final String rcsid =
    "$Id$";

  Container mainPanel;
  String filename;
  JEditorPane editorPane = new JEditorPane();
  JScrollPane scrollPane = new JScrollPane(editorPane);
  FileReader reader;

  public HTMLPageWindow() {
    editorPane.setEditorKit(new HTMLEditorKit());
    mainPanel = getContentPane();
    mainPanel.add(scrollPane, BorderLayout.CENTER);
    //  FIXME setable it properties
    setSize(625, 800);
  }

  public void setFile(String filename) {
    this.filename = filename;
    setTitle(filename);

    try {
      reader = new FileReader(filename);
      editorPane.read(reader, filename);
      editorPane.setEditable(false);
    }
    catch (Exception excep) {
      excep.printStackTrace();
    }
  }
}
