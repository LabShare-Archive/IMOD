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
* <p> $Log$
* <p> Revision 1.2  2007/04/19 21:38:09  sueh
* <p> bug# 964 Saving header state for RunParameters and Run.
* <p>
* <p> Revision 1.1  2007/02/21 04:19:34  sueh
* <p> bug# 964 Const inteface for PeetScreenState.
* <p> </p>
*/
public interface ConstPeetScreenState {
  public static  final String  rcsid =  "$Id$";
  
  PanelHeaderState getPeetSetupHeaderState();
  PanelHeaderState getPeetRunHeaderState();
}
