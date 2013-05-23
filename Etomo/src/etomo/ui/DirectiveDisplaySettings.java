package etomo.ui;

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
public interface DirectiveDisplaySettings {
  public static final String rcsid = "$Id:$";

  public boolean isInclude(int index);

  public boolean isExclude(int index);

  public boolean isShowUnchanged();

  public boolean isShowHidden();

  public boolean isShowOnlyIncluded();
}
