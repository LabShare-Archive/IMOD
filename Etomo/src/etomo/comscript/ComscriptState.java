package etomo.comscript;

/**
* <p>Description: </p>
*
* <p>Copyright: Copyright 2004 </p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* Univeristy of Colorado</p>
*
* @author $$Author$$
*
* @version $$Revision$$
*
* <p> $$Log$
* <p> $Revision 1.1  2004/08/19 01:29:34  sueh
* <p> $Interface to CombineComscriptState used by
* <p> $BackgroundComscriptProcess and BackgroundSystemProgram.
* <p> $It only interfaces one object, but an object specific to one comscript
* <p> $does not belong in BackgroundComscriptProcess or
* <p> $BackgroundSystemProgram.
* <p> $$ </p>
*/
public interface ComscriptState {
  public static final String rcsid = "$$Id$$";
  
  public int getStartCommand();
  public int getEndCommand();
  public String getCommand(int commandIndex);
  public String getWatchedFile(int commandIndex);
  public String getComscriptName();
  public String getComscriptWatchedFile();
}
