package etomo.type;

import java.util.Properties;

import etomo.comscript.ComScriptCommand;
import etomo.comscript.InvalidParameterException;

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
 * <p> Revision 1.3  2004/12/29 00:06:47  sueh
 * <p> bug# 567 Added set(ComScriptCommand) to get the value in
 * <p> ComScriptCommand value where thekeyword in ComScriptCommand
 * <p> equals name.
 * <p>
 * <p> Revision 1.2  2004/12/16 02:29:15  sueh
 * <p> bug# 564 Added backwardCompatibleValue, which overrides resetValue
 * <p> as the default when loading.
 * <p>
 * <p> Revision 1.1  2004/12/14 21:45:16  sueh
 * <p> bug# 564 A three state boolean (null, true, false).
 * <p> </p>
 */
public class EtomoBoolean extends ConstEtomoBoolean {
  public static final String rcsid = "$Id$";

  public EtomoBoolean() {
    super();
  }

  public EtomoBoolean(String name) {
    super(name);
  }

  public void load(Properties props) {
    if (useBackwardCompatibleValue) {
      set(props.getProperty(name, toString(backwardCompatibleValue)));
    }
    else {
      set(props.getProperty(name, toString(resetValue)));
    }
  }

  public void load(Properties props, String prepend) {
    if (useBackwardCompatibleValue) {
      set(props.getProperty(prepend + "." + name,
          toString(backwardCompatibleValue)));
    }
    else {
      set(props.getProperty(prepend + "." + name, toString(resetValue)));
    }
  }

  public ConstEtomoBoolean set(String value) {
    this.value = toInteger(value);
    return this;
  }

  public ConstEtomoBoolean set(ComScriptCommand scriptCommand)
      throws InvalidParameterException {
    value = toInteger(scriptCommand.getValue(name));
    return this;
  }

  public ConstEtomoBoolean set(boolean value) {
    this.value = toInteger(value);
    return this;
  }

  public ConstEtomoBoolean reset() {
    value = resetValue;
    return this;
  }

}