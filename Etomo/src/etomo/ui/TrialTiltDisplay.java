package etomo.ui;
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
* <p> $Log$ </p>
*/
public interface TrialTiltDisplay extends TiltDisplay {
  public static  final String  rcsid =  "$Id$";
  
  public String getTrialTomogramName();
  public boolean containsTrialTomogramName(String trialTomogramName);
  public void addTrialTomogramName(String trialTomogramName);
}
