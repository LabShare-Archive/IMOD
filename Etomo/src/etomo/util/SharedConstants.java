package etomo.util;

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
public final class SharedConstants {
  public static final String DISTORTION_FIELD_TOOLTIP = "OPTIONAL: If you wish to correct "
      + "for image distortion, enter the name of the appropriate image distortion file in "
      + "this field and the CCD camera binning in the following spin control.";
  public static final String IMAGES_ARE_BINNED_TOOLTIP = "Binning at which images were "
      + "acquired on " + "CCD camera.";
  public static final String VIEW_TYPE_TOOLTIP = "This radio button selector will choose whether the "
      + "data consists of a single frame per view or multiple frames per "
      + "view (montaged).";
  public static final String MIDAS_BINNING_TOOLTIP = "Bin images when reading then into Midas.";
}
