package etomo.ui.swing;

import java.awt.BorderLayout;
import java.awt.Container;
import java.net.URL;

import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.WindowConstants;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;
import javax.swing.text.html.HTMLEditorKit;

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
 * <p> Revision 3.2  2004/04/08 19:07:53  rickg
 * <p> Bug #422 added setDefaultCloseOperation call to constructor
 * <p>
 * <p> Revision 3.1  2003/11/27 00:04:06  rickg
 * <p> Fixed imports
 * <p> Removed unused member reader
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.2  2003/05/10 19:12:20  rickg
 * <p> OS independent path implementation
 * <p>
 * <p> Revision 2.1  2003/03/20 17:41:54  rickg
 * <p> Comment update
 * <p>
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
  public static final String rcsid = "$Id$";

  Container mainPanel;
  String url;
  JEditorPane editorPane = new JEditorPane();
  JScrollPane scrollPane = new JScrollPane(editorPane);

  public HTMLPageWindow() {
    editorPane.setEditorKit(new HTMLEditorKit());
    mainPanel = getContentPane();
    mainPanel.add(scrollPane, BorderLayout.CENTER);
    //  TODO should be setable in properties, need to get from app manager some
    // how
    setSize(625, 800);
    editorPane.addHyperlinkListener(this);
    setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
  }

  public void openURL(String newURL) {
    url = newURL;

    try {
      editorPane.setPage(newURL);
      setTitle(newURL);
      editorPane.setEditable(false);
    }
    catch (Exception except) {
      System.err.println("Cannot open URL:");
      System.err.println(newURL);
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
