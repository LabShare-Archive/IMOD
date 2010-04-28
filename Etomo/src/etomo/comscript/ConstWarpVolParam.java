package etomo.comscript;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2008</p>
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
 * <p> Revision 1.1  2009/06/05 01:46:19  sueh
 * <p> bug# 1219 Constant interface for WarpVolParam.
 * <p> </p>
 */
public interface ConstWarpVolParam extends Command {
  public static final String rcsid = "$Id$";

  public String getTemporaryDirectory();

  public String getOutputSizeZ();

  public boolean isInterpolationOrderLinear();
}
