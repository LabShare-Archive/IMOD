package etomo.type;

import java.util.Properties;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2002, 2003, 2004</p>
*
*<p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
* 
* <p> $Log$
* <p> Revision 1.2  2004/12/08 21:32:04  sueh
* <p> bug# 564 Added access to flipped.
* <p>
* <p> Revision 1.1  2004/12/07 22:54:07  sueh
* <p> bug# 564 Contains state variables to be saved in the .edf file.
* <p> </p>
*/
public class TomogramState implements BaseState {
  public static  final String  rcsid =  "$Id$";
  
  private static final String groupString = "ReconstructionState";
  EtomoBoolean flipped = new EtomoBoolean("Flipped");
 
  
  public TomogramState() {
    reset();
  }
  
  void reset() {
    flipped.reset();
  }
  
  public void store(Properties props) {
    store(props, "");
  }
  
  public void store(Properties props, String prepend) {
    prepend = createPrepend(prepend);
    String group = prepend + ".";
    flipped.store(props, prepend);
  }

  
  public boolean equals(TomogramState that) {
    if (!flipped.equals(that.flipped)) {
      return false;
    }
    return true;
  }
  
  protected static String createPrepend(String prepend) {
    if (prepend == "") {
      return groupString;
    }
    return prepend + "." + groupString;
  }

  public void load(Properties props) {
    load(props, "");
  }

  public void load(Properties props, String prepend) {
    reset();
    prepend = createPrepend(prepend);
    String group = prepend + ".";
    flipped.load(props, prepend);
  }
  
  public ConstEtomoBoolean setFlipped(boolean flipped) {
    return this.flipped.set(flipped);
  }
}
