package etomo.util;

import java.util.Vector;

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
 * <p> Revision 3.1  2004/11/20 00:09:16  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 3.0.6.1  2004/10/11 02:28:24  sueh
 * <p> bug# 520 Added toString() function for debugging purposes.
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.1  2003/11/04 21:32:59  sueh
 * <p> bug329 fixing null pointer exception
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.1.2.1  2003/01/24 18:45:05  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
public final class CircularBuffer {
  public static final String rcsid = "$Id$";

  Vector buffer;

  int iHead = 0;

  public CircularBuffer(int nElements) {
    buffer = new Vector(nElements);
    //
    //  Need to also set the size, even though the space is allocated for
    //  nElements, the buffer thinks it's length is zero.
    //
    buffer.setSize(nElements);
    iHead = nElements - 1;
  }

  public String toString() {
    return getClass().getName() + "[" + paramString() + "]";
  }

  protected String paramString() {
    StringBuffer stringBuffer = new StringBuffer("\niHead" + "=" + iHead);
    int index = iHead;
    while (index < buffer.size()) {
      stringBuffer.append(",\nbuffer[" + index + "]=" + buffer.get(index++));
    }
    index = 0;
    while (index < iHead && index < buffer.size()) {
      stringBuffer.append(",\nbuffer[" + index + "]=" + buffer.get(index++));
    }
    return stringBuffer.toString();
  }

  /**
   * Return the number of elements available in the circular buffer.
   */
  public int size() {
    return buffer.size();
  }

  /**
   * Place an object into the next position on circular buffer.
   */
  public void put(final Object obj) {
    if (iHead == buffer.size() - 1) {
      iHead = 0;
    }
    else {
      iHead = iHead + 1;
    }
    buffer.set(iHead, obj);
  }

  /**
   * Get an object from current position in the buffer and move the position
   * down one element.
   */
  public Object get() {
    int iCurrent = iHead;
    if (iHead == 0) {
      iHead = buffer.size() - 1;
    }
    else {
      iHead = iHead - 1;
    }
    return buffer.get(iCurrent);
  }

  /**
   * Search the list to see if the specified object is on it
   */
  public int search(Object obj) {
    Object bufferObj = null;
    for (int i = 0; i < buffer.size(); i++) {
      bufferObj = buffer.get(i);
      if (bufferObj != null && bufferObj.equals(obj)) {
        return i;
      }
    }
    return -1;
  }
}
