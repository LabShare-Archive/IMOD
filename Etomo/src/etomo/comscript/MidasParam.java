package etomo.comscript;

import java.io.File;
import java.util.ArrayList;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.process.SystemProgram;
import etomo.type.ConstJoinMetaData;
import etomo.type.ConstSectionTableRowData;
import etomo.type.SectionTableRowData;

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
* <p> Revision 1.3  2004/12/08 21:21:40  sueh
* <p> bug# 564 Added getBooleanValue() to get a misc boolean value.
* <p>
* <p> Revision 1.2  2004/11/19 23:05:04  sueh
* <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
* <p>
* <p> Revision 1.1.2.8  2004/11/16 02:21:23  sueh
* <p> bug# 520 EtomoNumber uses toString() instead of getString().
* <p>
* <p> Revision 1.1.2.7  2004/11/12 22:48:45  sueh
* <p> bug# 520 Added empty getIntegerValue and getBinning.
* <p>
* <p> Revision 1.1.2.6  2004/11/08 22:12:00  sueh
* <p> bug# 520 Add getMode to conform to Command.  Returns 0, since there
* <p> is no more for this Param.
* <p>
* <p> Revision 1.1.2.5  2004/10/29 01:17:33  sueh
* <p> bug# 520 Removed working directory from meta data.  Getting working
* <p> directory from propertyUserDir.
* <p>
* <p> Revision 1.1.2.4  2004/10/28 16:55:07  sueh
* <p> bug# 520 Specifying output file: -o rootname_midas.xf.
* <p>
* <p> Revision 1.1.2.3  2004/10/25 22:59:49  sueh
* <p> bug# 520 Fix chunk size by passing the number of rows to
* <p> ConstJoinMetaData.getChunkSize.
* <p>
* <p> Revision 1.1.2.2  2004/10/22 20:58:58  sueh
* <p> bug# 520 Getting chunk size from ConstSectionTableRowData.
* <p>
* <p> Revision 1.1.2.1  2004/10/21 02:35:21  sueh
* <p> bug# 520 Param for running Midas.
* <p> </p>
*/
public class MidasParam implements Command {
  public static  final String  rcsid =  "$Id$";
  
  private static final int commandSize = 1;
  private static final String commandName = "midas";
  private static final String outputFileExtension = "_midas.xf";
  
  private ConstJoinMetaData metaData;
  private String[] commandArray;
  private SystemProgram program;
  private String workingDir;
  private File outputFile = null;
  private String rootName = null;
  private String outputFileName = null;
  
  public MidasParam(ConstJoinMetaData metaData) {
    this.metaData = metaData;
    workingDir = EtomoDirector.getInstance().getCurrentPropertyUserDir();
    rootName = metaData.getRootName();
    outputFileName = rootName + outputFileExtension;
    outputFile = new File(workingDir, outputFileName);
    ArrayList options = genOptions();
    commandArray = new String[options.size() + commandSize];
    commandArray[0] = BaseManager.getIMODBinPath() + commandName;    
    for (int i = 0; i < options.size(); i++) {
      commandArray[i + commandSize] = (String) options.get(i);
    }
    program = new SystemProgram(commandArray);
    program.setWorkingDirectory(new File(workingDir));
  }
  
  public String[] getCommandArray() {
    return commandArray;
  }
  
  public String getCommandLine() {
    StringBuffer buffer = new StringBuffer();
    for (int i = 0; i < commandArray.length; i++) {
      buffer.append(commandArray[i] + " ");
    }
    return buffer.toString();
  }
  
  public String getCommandName() {
    return commandName;
  }
  
  private ArrayList genOptions() {
    ArrayList options = new ArrayList();
    ArrayList sectionData = metaData.getSectionTableData();
    int sectionDataSize = sectionData.size();
    StringBuffer chunkSize = new StringBuffer();
    options.add("-c");
    for (int i = 0; i < sectionDataSize; i++) {
      ConstSectionTableRowData data = (SectionTableRowData) sectionData.get(i);
      chunkSize.append(data.getChunkSize(sectionDataSize).toString());
      if (i < sectionDataSize - 1) {
        chunkSize.append(",");
      }
    }
    options.add(chunkSize.toString());
    options.add("-b");
    options.add("0");
    options.add("-D");
    options.add("-o");
    options.add(outputFileName);
    options.add(rootName + ".sample");
    options.add(rootName + ".xf");
    return options;
  }
  
  public File getCommandOutputFile() {
    return outputFile;
  }
  
  public int getIntegerValue(int name) {
    return Integer.MIN_VALUE;
  }
  
  public boolean getBooleanValue(int name) {
    return false;
  }
  
  public int getCommandMode() {
    return 0;
  }
  
  public static String getName() {
    return commandName;
  }
  
  public static String getOutputFileExtension() {
    return outputFileExtension;
  }

}
