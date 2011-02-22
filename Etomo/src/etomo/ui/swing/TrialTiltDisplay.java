package etomo.ui.swing;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2009</p>
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
* <p> Revision 1.1  2010/11/13 16:07:34  sueh
* <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
* <p>
* <p> Revision 3.1  2009/09/01 03:18:25  sueh
* <p> bug# 1222
* <p> </p>
*/
public interface TrialTiltDisplay extends TiltDisplay {
  public static final String rcsid = "$Id$";

  public String getTrialTomogramName();

  public boolean containsTrialTomogramName(String trialTomogramName);

  public void addTrialTomogramName(String trialTomogramName);
}
