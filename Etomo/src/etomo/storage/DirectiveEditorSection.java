package etomo.storage;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2013</p>
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
public final class DirectiveEditorSection {
  public static final String rcsid = "$Id:$";
  private final List<String> nameArray = new ArrayList<String>();

  private final String header;

  private DirectiveType directiveType = null;

  public DirectiveEditorSection(final String header) {
    this.header = header;
  }

  public void add(final Directive directive) {
    if (directive == null) {
      return;
    }
    if (directiveType == null) {
      // Save the directive type of the first directive added to get an idea of what
      // kind of directives will be stored in this section
      directiveType = directive.getType();
    }
    nameArray.add(directive.getName());
  }

  public void add(final String directiveName) {
    nameArray.add(directiveName);
  }

  public String toString() {
    return header;
  }

  public Iterator<String> nameIterator() {
    return nameArray.iterator();
  }
  
  public DirectiveType getDirectiveType() {
    return directiveType;
  }
}
