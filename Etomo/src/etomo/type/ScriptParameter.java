package etomo.type;

import etomo.comscript.ComScriptCommand;
import etomo.comscript.InvalidParameterException;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2005</p>
*
*<p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
* 
* <p> $Log$ </p>
*/
public class ScriptParameter extends EtomoNumber {
  public static  final String  rcsid =  "$Id$";

  protected Number defaultValue;
  
   public ScriptParameter(int type) {
     super(type);
     defaultValue = newNumber();
   }
   
   public ScriptParameter(int type, String name) {
     super(type, name);
     defaultValue = newNumber();
   }
   
   public ScriptParameter(ScriptParameter that) {
     super(that);
     defaultValue = newNumber(that.defaultValue);
   }
   
   public ScriptParameter(ConstEtomoNumber that) {
     super(that);
     defaultValue = newNumber();
   }
   
   public ScriptParameter(int type, String name, boolean preventNullValue, Number resetValue) {
     super(type, name, preventNullValue, resetValue);
     defaultValue = newNumber();
   }
   
   protected String paramString() {
     return super.paramString() + ",\ndefaultValue=" + defaultValue;
   }
   
   /**
    * Set the default value.
    * isUpdateCommand() returns false when getValueForCommand() is equal to a
    * non-null default value.
    * Don't use default value or don't call isUpdateCommand() if the parameter is
    * should appear in the command when it is defaulted.
    * Set the default and reset value the same if you want an unset parameter
    * to appear with its default value in the command.
    * @param defaultValue
    * @return
    */
   public ScriptParameter setDefault(int defaultValue) {
     this.defaultValue = newNumber(defaultValue);
     return this;
   }
   
   public ScriptParameter setDefault(boolean defaultValue) {
     if (defaultValue) {
       return setDefault(1);
     }
     return setDefault(0);
   }
   
   public ConstEtomoNumber setDefault(String defaultValueString) {
     StringBuffer invalidBuffer = new StringBuffer();
     Number defaultValue = newNumber(defaultValueString, invalidBuffer);
     if (invalidBuffer.length() == 0 && !isNull(defaultValue)) {
       this.defaultValue = defaultValue;
     }
     return this;
   }

   /**
    * Returns true if defaultValue is not null and getValue() is equal to
    * defaultValue.
    * @return
    */
   protected boolean isDefault(Number value) {
     if (isNull(value) || isNull(defaultValue)) {
       return false;
     }
     return equals(currentValue, defaultValue);
   }
   
   public ConstEtomoNumber addToScript(StringBuffer optionBuffer, boolean force) {
     if (!force && !isUseInScript()) {
       return this;
     }
     optionBuffer.append(toString(getValueForScript()));
     return this;
   }
   
   public ConstEtomoNumber setInScript(ComScriptCommand scriptCommand) {
     if (isUseInScript()) {
       scriptCommand.setValue(name, toString(getValueForScript()));
     }
     else {
       scriptCommand.deleteKey(name);
     }
     return this;
   }
   
   protected Number getValueForScript() {
     if (!isNull(currentValue)) {
       return currentValue;
     }
     if (!isNull(displayValue)) {
       return displayValue;
     }
     return currentValue;
   }
   
   public ConstEtomoNumber setUseScreenDisplayValue(boolean useDisplayValue) {
     super.useDisplayValue = useDisplayValue;
     return this;
   }
   
   /**
    * Returns true if value could be placed in a script (not null, not blank, and
    * not default).
    * @return
    */
   public boolean isUseInScript() {
     Number value = getValueForScript();
     if (!isNull(value) && !isDefault(value)) {
       return true;
     }
     return false;
   }
   
   /**
    * Parse scriptCommand for keyword.  If keyword is not found, call reset().
    * If keyword is found, call set with the string value found in scriptCommand.
    * @param scriptCommand
    * @param keyword
    * @return
    * @throws InvalidParameterException
    */
   public ConstEtomoNumber parse(ComScriptCommand scriptCommand, String keyword)
       throws InvalidParameterException {
     if (!scriptCommand.hasKeyword(keyword)) {
       return reset();
     }
     return set(scriptCommand.getValue(keyword));
   }
   
   public ConstEtomoNumber parse(ComScriptCommand scriptCommand)
      throws InvalidParameterException {
    return parse(scriptCommand, name);
  }

}
