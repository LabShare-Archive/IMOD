package etomo.comscript;

import java.io.File;
import java.util.ArrayList;

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.EnumeratedType;
import etomo.type.EtomoNumber;
import etomo.type.FileType;
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
 * <p> $Log$
 * <p> Revision 1.6  2010/04/28 16:13:46  sueh
 * <p> bug# 1344 Added getOutputImageFileType functions.
 * <p>
 * <p> Revision 1.5  2010/01/11 23:49:01  sueh
 * <p> bug# 1299 Added isMessageReporter.
 * <p>
 * <p> Revision 1.4  2009/12/11 17:26:22  sueh
 * <p> bug# 1291 Added getCommandInputFile to implement Command.
 * <p>
 * <p> Revision 1.3  2009/09/01 03:17:46  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.2  2007/11/06 19:18:38  sueh
 * <p> bug# 1047 Added getSubcommandDetails.
 * <p>
 * <p> Revision 1.1  2007/02/05 22:51:16  sueh
 * <p> bug# 962 Xftoxg parameter object.
 * <p> </p>
 */
public final class XftoxgParam implements Command {
  public static final String rcsid = "$Id$";

  public static final ProcessName PROCESS_NAME = ProcessName.XFTOXG;
  public static final String COMMAND_NAME = PROCESS_NAME.toString();
  public static final String NUMBER_TO_FIT_KEY = "NumberToFit";
  public static final String HYBRID_FITS_KEY = "HybridFits";
  public static final String REFERENCE_SECTION = "ReferenceSection";
  private static final boolean debug = true;
  private static final int COMMAND_SIZE = 1;

  private final EtomoNumber referenceSection = new EtomoNumber();
  private final EtomoNumber hybridFits = new EtomoNumber();
  private final EtomoNumber numberToFit = new EtomoNumber();

  private final BaseManager manager;

  private String xfFileName = "";
  private String xgFileName = "";
  private String[] commandArray = null;

  public XftoxgParam(BaseManager manager) {
    this.manager = manager;
  }

  private ArrayList genOptions() {
    ArrayList options = new ArrayList();
    if (!numberToFit.isNull()) {
      options.add("-" + NUMBER_TO_FIT_KEY);
      options.add(numberToFit.toString());
    }
    if (!referenceSection.isNull()) {
      options.add("-" + REFERENCE_SECTION);
      options.add(referenceSection.toString());
    }
    if (!hybridFits.isNull()) {
      options.add("-" + HYBRID_FITS_KEY);
      options.add(hybridFits.toString());
    }
    options.add(xfFileName);
    options.add(xgFileName);
    return options;
  }

  public void setReferenceSection(final ConstEtomoNumber input) {
    referenceSection.set(input);
  }

  public void setReferenceSection(final Number input) {
    referenceSection.set(input);
  }

  public void resetReferenceSection() {
    referenceSection.reset();
  }

  public void setXfFileName(final String input) {
    xfFileName = input;
  }

  public void setXgFileName(final String input) {
    xgFileName = input;
  }

  public void setHybridFits(final EnumeratedType enumType) {
    hybridFits.set(enumType.getValue());
  }

  public void resetHybridFits() {
    hybridFits.reset();
  }

  public void setNumberToFit(final int input) {
    numberToFit.set(input);
  }

  public void setNumberToFit(final EnumeratedType enumType) {
    numberToFit.set(enumType.getValue());
  }

  public void resetNumberToFit() {
    numberToFit.reset();
  }

  public int getHybridFits() {
    return hybridFits.getInt();
  }

  public boolean isHybridFitsEmpty() {
    return hybridFits.isNull();
  }

  public int getReferenceSection() {
    return referenceSection.getInt();
  }

  public boolean isReferenceSectionEmpty() {
    return referenceSection.isNull();
  }

  public int getNumberToFit() {
    return numberToFit.getInt();
  }

  public boolean isNumberToFitEmpty() {
    return numberToFit.isNull();
  }

  public AxisID getAxisID() {
    return AxisID.ONLY;
  }

  public String getCommand() {
    return COMMAND_NAME;
  }

  public String[] getCommandArray() {
    if (commandArray == null) {
      ArrayList options = genOptions();
      commandArray = new String[options.size() + COMMAND_SIZE];
      commandArray[0] = BaseManager.getIMODBinPath() + COMMAND_NAME;
      for (int i = 0; i < options.size(); i++) {
        commandArray[i + COMMAND_SIZE] = (String) options.get(i);
      }
      if (debug) {
        StringBuffer buffer = new StringBuffer();
        for (int i = 0; i < commandArray.length; i++) {
          buffer.append(commandArray[i]);
          if (i < commandArray.length - 1) {
            buffer.append(' ');
          }
        }
        System.err.println(buffer.toString());
      }
    }
    return commandArray;
  }

  public String getCommandLine() {
    if (commandArray.length == 0) {
      return "";
    }
    StringBuffer buffer = new StringBuffer(commandArray[0]);
    for (int i = 0; i < commandArray.length; i++) {
      buffer.append(' ' + commandArray[i]);
    }
    return buffer.toString();
  }

  public CommandDetails getSubcommandDetails() {
    return null;
  }

  public String getSubcommandProcessName() {
    return null;
  }

  public CommandMode getCommandMode() {
    return null;
  }

  public boolean isMessageReporter() {
    return false;
  }

  public String getCommandName() {
    return COMMAND_NAME;
  }

  public ProcessName getProcessName() {
    return PROCESS_NAME;
  }

  public File getCommandOutputFile() {
    if (xgFileName.equals("")) {
      return null;
    }
    return new File(manager.getPropertyUserDir(), xgFileName);
  }

  public FileType getOutputImageFileType() {
    return FileType.TRANSFORMED_REFINING_MODEL;
  }

  public FileType getOutputImageFileType2() {
    return null;
  }

  public File getCommandInputFile() {
    return null;
  }

  public static final class HybridFits implements EnumeratedType {
    public static final HybridFits TRANSLATIONS = new HybridFits(2);
    public static final HybridFits TRANSLATIONS_ROTATIONS = new HybridFits(3);

    private final EtomoNumber value = new EtomoNumber();

    private HybridFits(final int value) {
      this.value.set(value);
    }

    public ConstEtomoNumber getValue() {
      return value;
    }

    public boolean isDefault() {
      return false;
    }

    public String getLabel() {
      return null;
    }

    public String toString() {
      return value.toString();
    }

    public boolean equals(final int input) {
      return value.equals(input);
    }
  }

  public static final class NumberToFit implements EnumeratedType {
    public static final NumberToFit GLOBAL_ALIGNMENT = new NumberToFit(0);

    private final EtomoNumber value = new EtomoNumber();

    private NumberToFit(final int value) {
      this.value.set(value);
    }

    public ConstEtomoNumber getValue() {
      return value;
    }

    public boolean isDefault() {
      return false;
    }
    
    public String getLabel() {
      return null;
    }

    public String toString() {
      return value.toString();
    }

    public boolean equals(final int input) {
      return value.equals(input);
    }
  }
}
