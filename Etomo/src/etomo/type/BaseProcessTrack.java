package etomo.type;

import java.util.Properties;

import etomo.storage.Storable;

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
* <p> Revision 1.1.2.1  2004/09/29 19:16:18  sueh
* <p> bug# 520 Base class for ProcessTrack and JoinProcessTrack.  Implements
* <p> Storable with generic functions and abstract functions.
* <p> </p>
*/
public abstract class BaseProcessTrack implements Storable {
  public static  final String  rcsid =  "$Id$";
  
  protected String revisionNumber;
  protected boolean isModified = false;
  
  public abstract void store(Properties props, String prepend);
  public abstract void load(Properties props, String prepend);
  
  public void store(Properties props) {
    store(props, "");
  }
  
  public void load(Properties props) {
    load(props, "");
  }
  
  public String getRevisionNumber() {
    return revisionNumber;
  }

  public boolean isModified() {
    return isModified;
  }

  public void resetModified() {
    isModified = false;
  }
}
