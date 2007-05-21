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
* <p> $Log$ </p>
*/
public final class InterfaceType {
  public static  final String  rcsid =  "$Id$";
  
  public static final InterfaceType RECON = new InterfaceType("recon");
  public static final InterfaceType JOIN = new InterfaceType("join");
  public static final InterfaceType PP = new InterfaceType("pp");
  public static final InterfaceType PEET = new InterfaceType("peet");
  
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
