package etomo.type;
/**
* <p>Description: Enum class identify the different types of frames.</p>
* 
* <p>Copyright: Copyright 2011</p>
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
public final class FrameType {
  public static final String rcsid = "$Id$";
  
  public static final FrameType Main = new FrameType();
  public static final FrameType Sub = new FrameType();
  
  public String toString() {
    if (this==Main) {
      return "Main";
    }
    if (this==Sub) {
      return "Sub";
    }
    return "Unknown";
  }
}
