package etomo.comscript;

import java.io.File;
import java.util.ArrayList;

import etomo.type.ProcessName;

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
* <p> $Log$ </p>
*/
public final class PrmParserParam {
  public static  final String  rcsid =  "$Id$";
  
  private final File prmFile;
  public PrmParserParam(File prmFile) {
    this.prmFile=prmFile;
  }
  
  public ArrayList getCommand() {
    ArrayList command = new ArrayList();
    command.add(ProcessName.PRMPARSER.toString());
    command.add(prmFile.getName());
    return command;
  }
}
