package etomo.storage;

import etomo.comscript.FortranInputString;
import etomo.type.AxisID;
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

  private final String description;
  private final boolean batch;
  private final boolean template;
  private final DirectivesDescriptionFile.EtomoColumn etomoColumn;
  private final DirectiveValueType valueType;

  private boolean matchingTemplate = false;
  private String value = null;
  private String valueA = null;
  private String valueB = null;
  private String defaultValue = null;
  private String defaultValueA = null;
  private String defaultValueB = null;

  public Directive(final DirectivesDescriptionFile.DirectiveDescription descr) {
    directiveName.set(descr);
    description = descr.getDescription();
    batch = descr.isBatch();
    template = descr.isTemplate();
    etomoColumn = descr.getEtomoColumn();
    valueType = descr.getValueType();
  }

  public Directive(final String name) {
    directiveName.set(name);
    description = null;
    batch = false;
    template = false;
    etomoColumn = null;
    // The default directive value type is a string
    valueType = DirectiveValueType.STRING;
  }

  public boolean equals(final DirectiveType input) {
    return directiveName.equals(input);
  }

  public String getName() {
    return directiveName.get();
  }

  public DirectiveType getType() {
    return directiveName.getType();
  }
  
  public void setDefaultValue(final AxisID axisID, final boolean input) {
    if (axisID == null) {
      defaultValue = processValue(input);
    }
    else if (axisID == AxisID.SECOND) {
      defaultValueB = processValue(input);
    }
    else {
      defaultValueA = processValue(input);
    }
  }

  public void setDefaultValue(final AxisID axisID, final String input) {
    if (axisID == null) {
      defaultValue = input;
    }
    else if (axisID == AxisID.SECOND) {
      defaultValueB = input;
    }
    else {
      defaultValueA = input;
    }
  }

  public void setDefaultValue(final int input) {
    if (input == EtomoNumber.INTEGER_NULL_VALUE) {
      defaultValue = null;
    }
    else {
      defaultValue = String.valueOf(input);
    }
  }

  public void setValue(final AxisID axisID, final boolean input) {
    if (axisID == null) {
      value = processValue(input);
    }
    else if (axisID == AxisID.SECOND) {
      valueB = processValue(input);
    }
    else {
      valueA = processValue(input);
    }
  }

  public void setValue(final AxisID axisID, final ConstEtomoNumber input) {
    if (axisID == null) {
      value = input.toString();
    }
    else if (axisID == AxisID.SECOND) {
      valueB = input.toString();
    }
    else {
      valueA = input.toString();
    }
  }

  public void setValue(final AxisID axisID, final String input) {
    if (axisID == null) {
      value = input;
    }
    else if (axisID == AxisID.SECOND) {
      valueB = input;
    }
    else {
      valueA = input;
    }
  }

  public void setValue(final boolean input) {
    value = processValue(input);
  }

  public void setValue(final ConstEtomoNumber input) {
    if (input == null) {
      value = null;
    }
    else {
      value = input.toString();
    }
  }

  public void setValue(final ConstStringParameter input) {
    if (input == null) {
      value = null;
    }
    else {
      value = input.toString();
    }
  }

  public void setValue(final double input) {
    if (input == EtomoNumber.DOUBLE_NULL_VALUE) {
      value = null;
    }
    else {
      value = String.valueOf(input);
    }
  }

  public void setValue(final double[] input) {
    if (input == null) {
      value = null;
    }
    else {
      FortranInputString inputString = new FortranInputString(input.length);
      for (int i = 0; i < input.length; i++) {
        inputString.set(i, input[i]);
      }
      value = inputString.toString();
    }
  }

  public void setValue(final int input) {
    if (input == EtomoNumber.INTEGER_NULL_VALUE) {
      value = null;
    }
    else {
      value = String.valueOf(input);
    }
  }

  public void setValue(final String input) {
    value = input;
  }

  private String processValue(final boolean input) {
    if (input) {
      return "1";
    }
    return "0";
  }

  public boolean isValid(DirectiveFileType directiveFileType) {
    if (directiveFileType == null || matchingTemplate || (batch && template)) {
      return true;
    }
    if (!batch && !template) {
      return false;
    }
    if (batch && directiveFileType != DirectiveFileType.BATCH) {
      return false;
    }
    if (template && directiveFileType == DirectiveFileType.BATCH) {
      return false;
    }
    return true;
  }

  public void setMatchingTemplate(final boolean input) {
    matchingTemplate = input;
  }
}
