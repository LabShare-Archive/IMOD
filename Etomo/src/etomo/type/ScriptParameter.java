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
* <p> $Log$
* <p> Revision 1.2  2005/02/11 22:32:34  sueh
* <p> Fixed problem in ScriptParameter:  it was letting defaulted parameters
* <p> into scripts.
* <p>
* <p> Revision 1.1  2005/01/25 23:59:34  sueh
* <p> An EtomoNumber which can read from and write to scripts.  Has a
* <p> defaultValue member variable which is compared to currentValue in
* <p> order to prevent unnecessary script entries.  Has it own getValue function
* <p> called getValueForScript() which ignores useDisplayValue.
* <p> Can set useDisplayValue to prevent the screen from showing the display
* <p> value, but still allow the script to show it.  To force entries to appear in
* <p> a script when they are defaulted, set the force parameter to true.  Can
* <p> also use EtomoNumber.get functions, but these do not ignore
* <p> useDisplayValue.
* <p> </p>
*/
public class ScriptParameter extends EtomoNumber {
  public static  final String  rcsid =  "$Id$";

  protected Number defaultValue;
  private String shortName = null;
  
   public ScriptParameter(int type) {
     super(type);
     defaultValue = newNumber();
   }
   
   public ScriptParameter(int type, String name) {
     super(type, name);
     defaultValue = newNumber();
   }
   
   public ScriptParameter(int type, String name, String shortName) {
     super(type, name);
     this.shortName = shortName;
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

   public ConstEtomoNumber useDefaultAsDisplayValue() {
     return setDisplayValue(defaultValue);
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
     return equals(value, defaultValue);
   }
   
   public ConstEtomoNumber updateComScript(ComScriptCommand scriptCommand) {
     if (isUseInScript()) {
       scriptCommand.setValue(name, toString(getValue()));
     }
     else {
       scriptCommand.deleteKey(name);
     }
     return this;
   }
   
   /**
    * Returns true if value could be placed in a script (not null, not blank, and
    * not default).
    * @return
    */
   public boolean isUseInScript() {
     Number value = getValue();
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
     if (!scriptCommand.hasKeyword(name)) {
       if (shortName == null || !scriptCommand.hasKeyword(shortName)) {
         return reset();
       }
       return set(scriptCommand.getValue(shortName));
     }
     return set(scriptCommand.getValue(name));
  }

}
