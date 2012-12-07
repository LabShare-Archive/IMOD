package etomo.storage.autodoc;

import java.util.Iterator;
import java.util.List;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2012</p>
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
public final class ReadOnlyAttributeIterator {
  public static final String rcsid = "$Id:$";

  private final Iterator<Attribute> iterator;

  ReadOnlyAttributeIterator(final List<Attribute> list) {
    iterator = list.iterator();
  }

  public ReadOnlyAttribute next() {
    return iterator.next();
  }

  public boolean hasNext() {
    return iterator.hasNext();
  }
}
