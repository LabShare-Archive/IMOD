package etomo.storage;

import java.io.File;
import java.io.IOException;

import etomo.Arguments.DebugLevel;
import etomo.EtomoDirector;
import etomo.comscript.FortranInputString;
import etomo.comscript.FortranInputSyntaxException;
import etomo.storage.autodoc.AutodocTokenizer;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstStringParameter;
import etomo.type.DirectiveFileType;
import etomo.type.EtomoNumber;

/**
* <p>Description: This class represents information about a directive required by the
* Directive File Editor.  Directives which can have multiple axes (other then
* setupset.copyarg directives), are handled with a single Directive instance.
* </p>
* 
* <p>Copyright: Copyright 2013</p>
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
public class Directive {
  public static final String rcsid = "$Id:$";

  private final DirectiveName directiveName = new DirectiveName();
  private final boolean batch;
  private final String description;
  private final DirectiveDescrEtomoColumn etomoColumn;
  private final boolean template;
  private final DirectiveValues values;
  private final DirectiveValueType valueType;
  private final String label;
  private final DirectiveDescrFile.ChoiceList choiceList;

  private boolean[] inDirectiveFile = null;
  private boolean include = false;
  private DebugLevel debug = EtomoDirector.INSTANCE.getArguments().getDebugLevel();

  public Directive(final DirectiveDescr descr) {
    directiveName.setKey(descr);
    valueType = descr.getValueType();
    values = new DirectiveValues(valueType);
    description = descr.getDescription();
    batch = descr.isBatch();
    template = descr.isTemplate();
    etomoColumn = descr.getEtomoColumn();
    label = descr.getLabel();
    choiceList = descr.getChoiceList(debug);
  }

  /**
   * Constructor for undefined directives.
   * @param directiveName - constructor does a deep copy of this parameter
   */
  public Directive(final DirectiveName directiveName) {
    this.directiveName.deepCopy(directiveName);
    valueType = DirectiveValueType.UNKNOWN;
    values = new DirectiveValues(valueType);
    description = null;
    // No description of this directive, so allow it to exist in any type of directive
    // file.
    batch = true;
    template = true;
    etomoColumn = null;
    label = null;
    choiceList = null;
  }

  void write(final LogFile logFile, LogFile.WriterId id) throws LogFile.LockException,
      IOException {
    Value value = values.getValue();
    String valueString = "";
    if (value != null) {
      valueString = value.toString();
    }
    logFile.write(directiveName.getName() + AutodocTokenizer.DEFAULT_DELIMITER
        + valueString, id);
    logFile.newLine(id);
  }

  public void setDebug(final DebugLevel input) {
    values.setDebug(input);
    if (choiceList != null) {
      choiceList.setDebug(input);
    }
  }

  public void resetDebug() {
    values.resetDebug();
  }

  public boolean equals(final DirectiveType input) {
    return directiveName.equals(input);
  }

  public DirectiveDescrEtomoColumn getEtomoColumn() {
    return etomoColumn;
  }

  public String getInDirectiveFileDebugString() {
    if (inDirectiveFile != null) {
      StringBuffer buffer = new StringBuffer();
      for (int i = 0; i < inDirectiveFile.length; i++) {
        if (inDirectiveFile[i]) {
          buffer.append((buffer.length() > 0 ? ", " : "") + "in "
              + DirectiveFileType.getInstance(i) + " directive file");
        }
      }
      if (buffer.length() > 0) {
        return buffer.toString();
      }
    }
    return null;
  }

  public DirectiveDescrFile.ChoiceList getChoiceList() {
    return choiceList;
  }

  public String getDescription() {
    return description;
  }

  public String getKey() {
    return directiveName.getKey();
  }

  public String getKeyDescription() {
    return directiveName.getKeyDescription();
  }

  public String getName() {
    return directiveName.getName();
  }

  public String getTitle() {
    if (label != null) {
      return label;
    }
    return directiveName.getTitle();
  }

  public DirectiveType getType() {
    return directiveName.getType();
  }

  public DirectiveValues getValues() {
    return values;
  }

  public DirectiveValueType getValueType() {
    return valueType;
  }

  public boolean isBatch() {
    return batch;
  }

  public boolean isChoiceList() {
    return choiceList != null && !choiceList.isEmpty();
  }

  public boolean isInclude() {
    return include;
  }

  public boolean isCopyArg() {
    return directiveName.isCopyArg();
  }

  public boolean isValid() {
    return directiveName.isValid();
  }

  /**
   * Returns true if inDirectiveFile is true for the index and the axis (or for any axes).
   * @param index
   * @param axisID
   * @return
   */
  public boolean isInDirectiveFile(final int index) {
    if (inDirectiveFile == null || index < 0 || index >= DirectiveFileType.NUM) {
      return false;
    }
    return inDirectiveFile[index];
  }

  public boolean isTemplate() {
    return template;
  }

  public void setDefaultValue(final boolean input) {
    values.setDefaultValue(input);
  }

  public void setDefaultValue(final ConstEtomoNumber input) {
    values.setDefaultValue(input);
  }

  public void setDefaultValue(final String input) {
    values.setDefaultValue(input);
  }

  public void setDefaultValue(final int input) {
    values.setDefaultValue(input);
  }

  public void setInclude(final boolean input) {
    include = input;
  }

  public void setInDirectiveFile(final DirectiveFileType type, final boolean input) {
    if (type == null) {
      return;
    }
    int index = type.getIndex();
    if (inDirectiveFile == null) {
      // Initialize inDirectiveFile, and add input value.
      inDirectiveFile = new boolean[DirectiveFileType.NUM];
      for (int i = 0; i < inDirectiveFile.length; i++) {
        if (i == index) {
          inDirectiveFile[i] = input;
        }
        else {
          inDirectiveFile[i] = false;
        }
      }
    }
    else if (index >= 0 && index < inDirectiveFile.length) {
      // Add input value.
      inDirectiveFile[index] = input;
    }
  }

  public void setValue(final File input) {
    if (input != null) {
      values.setValue(input.getAbsolutePath());
    }
    else {
      values.setValue("");
    }
  }

  public void setValue(final boolean input) {
    values.setValue(input);
  }

  public void setValue(final ConstEtomoNumber input) {
    values.setValue(input);
  }

  public void setValue(final String input) {
    values.setValue(input);
  }

  public void setValue(final ConstStringParameter input) {
    values.setValue(input);
  }

  public void setValue(final double input) {
    values.setValue(input);
  }

  public void setValue(final double[] input) {
    values.setValue(input);
  }

  public void setValue(final FortranInputString input) {
    values.setValue(input);
  }

  public void setValue(final int input) {
    values.setValue(input);
  }

  public String toString() {
    StringBuffer buffer = new StringBuffer();
    buffer.append("[directiveName:" + directiveName + ",batch:" + batch + ",description:"
        + description + ",etomoColumn:" + etomoColumn + ",template:" + template
        + ",valueType:" + valueType + ",values:" + values + "]");
    return buffer.toString();
  }

  private static final class AxisLevelData {
    private boolean[] inDirectiveFile = new boolean[DirectiveFileType.NUM];

    private AxisLevelData() {
      for (int i = 0; i < inDirectiveFile.length; i++) {
        inDirectiveFile[i] = false;
      }
    }

    public String toString() {
      StringBuffer buffer = new StringBuffer();
      for (int i = 0; i < inDirectiveFile.length; i++) {
        if (inDirectiveFile[i]) {
          buffer.append((buffer.length() > 0 ? ", " : "") + "in "
              + DirectiveFileType.getInstance(i) + " directive file");
        }
      }
      if (buffer.length() > 0) {
        return buffer.toString();
      }
      return null;
    }
  }

  public static abstract class Value {
    DebugLevel debug = EtomoDirector.INSTANCE.getArguments().getDebugLevel();

    abstract void set(boolean input);

    abstract void set(ConstEtomoNumber input);

    abstract void set(ConstStringParameter input);

    abstract void set(double input);

    abstract void set(double[] input);

    abstract void set(FortranInputString input);

    abstract void set(int input);

    abstract void set(String input);

    public abstract boolean toBoolean();

    public abstract String toString();

    public abstract boolean isEmpty();

    void setDebug(final DebugLevel input) {
      debug = input;
    }

  }

  static final class ValueFactory {
    static Value getValue(final DirectiveValueType valueType) {
      if (valueType == DirectiveValueType.BOOLEAN) {
        return new BooleanValue();
      }
      else if (valueType == DirectiveValueType.FLOATING_POINT
          || valueType == DirectiveValueType.INTEGER) {
        return new NumericValue(valueType);
      }
      else if (valueType == DirectiveValueType.FLOATING_POINT_PAIR
          || valueType == DirectiveValueType.INTEGER_PAIR) {
        return new NumericPairValue(valueType);
      }
      return new StringValue();
    }
  }

  static final class BooleanValue extends Value {
    private boolean value = false;

    private BooleanValue() {
    }

    public boolean isEmpty() {
      return false;
    }

    boolean equals(final BooleanValue input) {
      if (debug.isExtra()) {
        System.err.println("equals:value:" + value + ",input:" + input);
      }
      if (input == null) {
        // the default is false
        return !value;
      }
      return value == input.value;
    }

    public void set(final boolean input) {
      value = input;
    }

    public void set(final ConstEtomoNumber input) {
      if (input == null) {
        value = false;
      }
      else {
        value = input.is();
      }
    }

    public void set(final ConstStringParameter input) {
      if (input == null) {
        value = false;
      }
      else {
        set(input.toString());
      }
    }

    public void set(double input) {
      if (input == 1) {
        value = true;
      }
      else if (input == 0 || input == EtomoNumber.DOUBLE_NULL_VALUE) {
        value = false;
      }
    }

    public void set(double[] input) {
      if (input == null || input.length == 0) {
        value = false;
      }
      else {
        set(input[0]);
      }
    }

    public void set(FortranInputString input) {
      if (input == null || input.size() == 0) {
        value = false;
      }
      else {
        if (input.isIntegerType(0)) {
          set(input.getInt(0));
        }
        else {
          set(input.getDouble(0));
        }
      }
    }

    public void set(int input) {
      if (input == 1) {
        value = true;
      }
      else if (input == 0 || input == EtomoNumber.INTEGER_NULL_VALUE) {
        value = false;
      }
    }

    public void set(String input) {
      if (input == null) {
        value = false;
      }
      else {
        input = input.trim();
        if (input.equals("1")) {
          value = true;
        }
        else if (input.equals("0") || input.equals("")) {
          value = false;
        }
      }
    }

    public boolean toBoolean() {
      return value;
    }

    public String toString() {
      if (value) {
        return "1";
      }
      return "0";
    }
  }

  static final class NumericValue extends Value {
    private final EtomoNumber value;

    private NumericValue(final DirectiveValueType valueType) {
      if (valueType == DirectiveValueType.FLOATING_POINT) {
        value = new EtomoNumber(EtomoNumber.Type.DOUBLE);
      }
      else {
        value = new EtomoNumber();
      }
    }

    public boolean isEmpty() {
      return value == null || value.isNull();
    }

    boolean equals(final NumericValue input) {
      if (input == null) {
        return value.isNull();
      }
      return value.equals(input.value);
    }

    public void set(final boolean input) {
      value.set(input);
    }

    public void set(final ConstEtomoNumber input) {
      value.set(input);
    }

    public void set(final ConstStringParameter input) {
      if (input == null) {
        value.reset();
      }
      else {
        value.set(input.toString());
      }
    }

    public void set(double input) {
      value.set(input);
    }

    public void set(double[] input) {
      if (input == null || input.length == 0) {
        value.reset();
      }
      else {
        value.set(input[0]);
      }
    }

    public void set(FortranInputString input) {
      if (input == null || input.size() == 0) {
        value.reset();
      }
      else {
        if (input.isIntegerType(0)) {
          set(input.getInt(0));
        }
        else {
          set(input.getDouble(0));
        }
      }
    }

    public void set(int input) {
      value.set(input);
    }

    public void set(final String input) {
      value.set(input);
    }

    public boolean toBoolean() {
      return value.is();
    }

    public String toString() {
      return value.toString();
    }
  }

  static final class NumericPairValue extends Value {
    private final FortranInputString value = new FortranInputString(2);

    private NumericPairValue(final DirectiveValueType valueType) {
      if (valueType != DirectiveValueType.FLOATING_POINT_PAIR) {
        value.setIntegerType(true);
      }
    }

    public boolean isEmpty() {
      return value == null || value.isNull();
    }

    boolean equals(final NumericPairValue input) {
      if (input == null) {
        return value.isNull();
      }
      return value.equals(input.value);
    }

    public void set(final boolean input) {
      value.set(0, input ? 1 : 0);
    }

    public void set(final ConstEtomoNumber input) {
      value.set(0, input);
    }

    public void set(final ConstStringParameter input) {
      if (input == null) {
        value.setDefault();
      }
      else {
        try {
          value.validateAndSet(input.toString());
        }
        catch (FortranInputSyntaxException e) {
          e.printStackTrace();
        }
      }
    }

    public void set(double input) {
      value.set(0, input);
    }

    public void set(double[] input) {
      value.setDefault();
      if (input != null) {
        for (int i = 0; i < input.length; i++) {
          value.set(i, input[i]);
        }
      }
    }

    public void set(FortranInputString input) {
      value.setDefault();
      if (input != null) {
        for (int i = 0; i < input.size(); i++) {
          value.set(i, input.toString(i, true));
        }
      }
    }

    public void set(int input) {
      value.set(0, input);
    }

    public void set(final String input) {
      try {
        value.validateAndSet(input);
      }
      catch (FortranInputSyntaxException e) {
        e.printStackTrace();
      }
    }

    public boolean toBoolean() {
      if (value.isNull(0)) {
        return false;
      }
      int element = value.getInt(0);
      if (element == 0) {
        return false;
      }
      return true;
    }

    public String toString() {
      if (value.isNull()) {
        return "";
      }
      return value.toString();
    }
  }

  static final class StringValue extends Value {
    private String value = null;

    private StringValue() {
    }

    public boolean isEmpty() {
      return value == null || value.matches("\\s*");
    }

    boolean equals(final StringValue input) {
      if (input == null || input.value == null || input.value.matches("\\s*")) {
        return value == null || value.matches("\\s*");
      }
      return value.trim().equals(input.value.trim());
    }

    public void set(final boolean input) {
      value = input ? "1" : "0";
    }

    public void set(final ConstEtomoNumber input) {
      if (input == null || input.isNull()) {
        value = null;
      }
      else {
        value = input.toString();
      }
    }

    public void set(final ConstStringParameter input) {
      if (input == null || input.isEmpty()) {
        value = null;
      }
      else {
        value = input.toString();
      }
    }

    public void set(double input) {
      if (input == EtomoNumber.DOUBLE_NULL_VALUE) {
        value = null;
      }
      else {
        value = String.valueOf(input);
      }
    }

    public void set(double[] input) {
      if (input == null) {
        value = null;
      }
      else {
        StringBuffer buffer = new StringBuffer();
        for (int i = 0; i < input.length; i++) {
          buffer.append((i > 0 ? "," : "")
              + (input[i] == EtomoNumber.INTEGER_NULL_VALUE ? " " : String
                  .valueOf(input[i])));
        }
        if (buffer.length() > 0) {
          value = buffer.toString();
        }
      }
    }

    public void set(FortranInputString input) {
      if (input == null || input.isNull()) {
        value = null;
      }
      else {
        value = input.toString();
      }
    }

    public void set(int input) {
      if (input == EtomoNumber.INTEGER_NULL_VALUE) {
        value = null;
      }
      else {
        value = String.valueOf(input);
      }
    }

    public void set(final String input) {
      value = input;
    }

    public String toString() {
      return value == null ? "" : value;
    }

    public boolean toBoolean() {
      if (value != null && value.equals("1")) {
        return true;
      }
      return false;
    }
  }
}
