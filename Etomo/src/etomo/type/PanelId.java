package etomo.type;

/**
 * <p>Description: Unique IDs for panel that can be used to identify meta data
 * and state information.</p>
 * 
 * <p>Copyright: Copyright 2010</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 3.3  2010/03/27 04:51:56  sueh
 * <p> bug# 1333 Added panel IDs for AbstractTiltPanel.
 * <p>
 * <p> Revision 3.2  2010/03/03 04:58:31  sueh
 * <p> bug# 1311 Added panel IDs for the tiltxcorr panel.
 * <p>
 * <p> Revision 3.1  2010/02/17 04:52:36  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p> </p>
 */
public final class PanelId {
  public static final String rcsid = "$Id$";

  public static final PanelId POST_FLATTEN_VOLUME = new PanelId();
  public static final PanelId TOOLS_FLATTEN_VOLUME = new PanelId();
  public static final PanelId CROSS_CORRELATION = new PanelId();
  public static final PanelId PATCH_TRACKING = new PanelId();
  public static final PanelId TILT_3D_FIND = new PanelId();
  public static final PanelId TILT = new PanelId();
  public static final PanelId TILT_SIRT = new PanelId();
}
