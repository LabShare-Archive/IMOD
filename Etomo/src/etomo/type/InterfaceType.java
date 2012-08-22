package etomo.type;
/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2006</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
* 
* @threadsafe
* @immutable
* @n'ton
* 
* <p> $Log$
* <p> Revision 1.2  2007/05/22 21:14:32  sueh
* <p> bug# 999 Added a comment.
* <p>
* <p> Revision 1.1  2007/05/21 22:29:55  sueh
* <p> bug# 1000 Class that enumerates the four different interfaces.
* <p> </p>
*/
public final class InterfaceType {
  public static  final String  rcsid =  "$Id$";
  
  public static final InterfaceType RECON = new InterfaceType("recon");
  public static final InterfaceType JOIN = new InterfaceType("join");
  public static final InterfaceType PP = new InterfaceType("pp");
  public static final InterfaceType PEET = new InterfaceType("peet");
  public static final InterfaceType TOOLS = new InterfaceType("tools");
  public static final InterfaceType FRONT_PAGE = new InterfaceType("frontPage");
  public static final InterfaceType SERIAL_SECTIONS = new InterfaceType("serialSections");
  
  private final String name;
  
  private InterfaceType(String name) {
    this.name = name;
  }
  
  public static InterfaceType getInstance(String name) {
    if (name == null) {
      return null;
    }
    if (name.equals(RECON.name)) {
      return RECON;
    }
    if (name.equals(JOIN.name)) {
      return JOIN;
    }
    if (name.equals(PP.name)) {
      return PP;
    }
    if (name.equals(PEET.name)) {
      return PEET;
    }
    return null;
  }
  
  public boolean equals(InterfaceType interfaceType) {
    return this==interfaceType;
  }
}
