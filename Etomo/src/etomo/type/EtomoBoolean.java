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
 * <p> Revision 1.7  2005/01/10 23:27:20  sueh
 * <p> bug# 578 Removed backwardCompatibleValue since it is not being used.
 * <p>
 * <p> Revision 1.6  2005/01/05 19:58:15  sueh
 * <p> bug# 567 Fixed bug in set(ComScriptCommand).  Function was not
 * <p> handling a comscript where including a boolean parameter without a value
 * <p>  means that it is true.  Caught the InvalidParameterException from
 * <p> ComScriptCommand.getValue() and set the value to 1.
 * <p>
 * <p> Revision 1.5  2004/12/30 17:59:29  sueh
 * <p> bug# 567 Fixed set(ComScriptCommand): handle a missing keyword by
 * <p> setting value to 0.
 * <p>
 * <p> Revision 1.4  2004/12/29 23:47:59  sueh
 * <p> bug# 567 fixing set(ComScriptCommand):  Pass the string retrieved from
 * <p> scriptCommand to toInteger instead of checking whether the keyword is
 * <p> there.  Boolean may be stored in different ways in a comscript.
 * <p>
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

  public EtomoBoolean(String name) {
    super(name);
  }

  public void load(Properties props) {
    set(props.getProperty(name, toString(resetValue)));
  }

  public void load(Properties props, String prepend) {
    set(props.getProperty(prepend + "." + name, toString(resetValue)));
  }

  public ConstEtomoBoolean set(String value) {
    this.value = toInteger(value);
    return this;
  }

  public ConstEtomoBoolean set(ComScriptCommand scriptCommand)
      throws InvalidParameterException {
    //If the keyword is missing, set value to false
    //Since a missing keyword can mean false, it value shouldn't be set to null
    if (!scriptCommand.hasKeyword(name)) {
      value = 0;
    }
    else {
      try {
        value = toInteger(scriptCommand.getValue(name));
      }
      catch (InvalidParameterException e) {
        value = 1;
      }
    }
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