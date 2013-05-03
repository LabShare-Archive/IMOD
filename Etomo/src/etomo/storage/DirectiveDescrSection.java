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
public final class DirectiveDescrSection {
  public static final String rcsid = "$Id:$";

  private final List<String> nameArray = new ArrayList<String>();

  private final String header;

  private boolean containsEditableDirectives = true;

  public DirectiveDescrSection(final String header) {
    this.header = header;
  }

  public void add(final Directive directive) {
    if (directive == null || !directive.isValid()) {
      return;
    }
    nameArray.add(directive.getKey());
  }

  public void add(final String directiveName) {
    if (directiveName != null && !directiveName.matches("\\s*")) {
      nameArray.add(directiveName);
    }
  }

  public int size() {
    return nameArray.size();
  }

  public void setContainsEditableDirectives(final boolean input) {
    containsEditableDirectives = input;
  }

  public boolean isContainsEditableDirectives() {
    return containsEditableDirectives;
  }

  public String toString() {
    return header;
  }

  public Iterator<String> nameIterator() {
    return nameArray.iterator();
  }
}