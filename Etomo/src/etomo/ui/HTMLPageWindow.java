package etomo.ui;

import java.io.*;
import java.awt.*;
import javax.swing.*;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;
import javax.swing.text.html.*;
import java.net.URL;

//TODO the scrollbar does not work in this window when it is opened in a modal
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
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.3.2.1  2003/01/24 18:43:37  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.3  2002/11/14 04:22:51  rickg
 * <p> HTMLPage and ContextPopup now work with URLS
 * <p>
 * <p> Revision 1.2  2002/10/07 22:31:18  rickg
 * <p> removed unused imports
 * <p> reformat after emacs trashed it
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
public class HTMLPageWindow extends JFrame implements HyperlinkListener {
  public static final String rcsid =
    "$Id$";

  Container mainPanel;
  String url;
  JEditorPane editorPane = new JEditorPane();
  JScrollPane scrollPane = new JScrollPane(editorPane);
  FileReader reader;

  public HTMLPageWindow() {
    editorPane.setEditorKit(new HTMLEditorKit());
    mainPanel = getContentPane();
    mainPanel.add(scrollPane, BorderLayout.CENTER);
    //  TODO should be setable in properties, need to get from app manager some
    // how
    setSize(625, 800);
    editorPane.addHyperlinkListener(this);
  }

  public void openURL(String newURL) {
    url = newURL;

    try {
      editorPane.setPage(newURL);
      setTitle(newURL);
      editorPane.setEditable(false);
    }
    catch (Exception except) {
      except.printStackTrace();
    }
  }

  public void hyperlinkUpdate(HyperlinkEvent event) {
    if (event.getEventType() == HyperlinkEvent.EventType.ACTIVATED) {
      try {
        URL url = event.getURL();
        editorPane.setPage(url);
        setTitle(url.getPath());
      }
      catch (Exception except) {
        except.printStackTrace();
      }
    }
  }
}
