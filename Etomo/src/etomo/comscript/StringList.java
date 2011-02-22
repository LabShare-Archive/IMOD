package etomo.comscript;

import java.util.ArrayList;

/**
 * <p>Description: </p>
 *
 * <p>Copyright: Copyright (c) 2002</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 * <p> $Log$
 * <p> Revision 3.5  2010/09/24 00:54:43  sueh
 * <p> bug# 1404 Added mConvertToSingleEntry.
 * <p>
 * <p> Revision 3.4  2010/09/23 21:06:49  sueh
 * <p> bug# 1404 Added boolean successiveEntriesAccumulate.  Added parse
 * <p> ComScriptCommand).  Removed successiveEntriesAccumulate
 * <p> parameter from updateComScript.
 * <p>
 * <p> Revision 3.3  2006/05/16 21:30:23  sueh
 * <p> bug# 856 Changed parseString() to handle a null newList.
 * <p>
 * <p> Revision 3.2  2005/05/09 23:13:46  sueh
 * <p> bug# 658 Added a key for saving to a comscript.  Added
 * <p> StringList(String key).  Added a reset() function to remove data without
 * <p> removing the key.  Added setKey() and getKey().  Added
 * <p> parseString(StringList) to get data from another StringList without
 * <p> changing the key.
 * <p>
 * <p> Revision 3.1  2004/12/28 23:46:57  sueh
 * <p> bug# 567 Add default constructor.  Add constructor for String[].  Add
 * <p> parseString() for String[].
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.1.2.1  2003/01/24 18:33:42  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 * <p>Title: </p>
 */

public class StringList {
  public static final String rcsid = "$Id$";
  private String[] mElements = null;
  private String mKey;
  private boolean mSuccessiveEntriesAccumulate = false;
  private boolean mConvertToSingleEntry = false;

  public StringList() {
    mElements = new String[0];
  }

  public StringList(String key) {
    mElements = new String[0];
    this.mKey = key;
  }

  public StringList(int nElements) {
    mElements = new String[nElements];
  }

  void reset() {
    if (mElements == null) {
      mElements = new String[0];
    }
    else {
      mElements = new String[mElements.length];
    }
  }

  /**
   * MSuccessiveEntriesAccumulate defaults to false.  Turning on
   * mSuccessiveEntriesAccumulate means that all entries matching mKey will be
   * loaded.
   */
  void setSuccessiveEntriesAccumulate() {
    mSuccessiveEntriesAccumulate = true;
  }

  /**
   * MConvertToSingleEntry defaults to false.  Turning on mConvertToSingleEntry
   * means that all entries matching mKey will be deleted and then mElement
   * will be saved into one entry.  If mSuccessiveEntriesAccumulate is false,
   * mConvertToSingleEntry will be ignored.
   */
  void setConvertToSingleEntry() {
    mConvertToSingleEntry = true;
  }

  /**
   * Copy constructor
   */
  public StringList(StringList src) {
    mElements = new String[src.getNElements()];
    for (int i = 0; i < mElements.length; i++) {
      mElements[i] = src.get(i);
    }
    mKey = src.mKey;
  }

  public StringList(String[] stringArray) {
    parseString(stringArray);
  }

  public void setKey(String key) {
    this.mKey = key;
  }

  public String getKey() {
    return mKey;
  }

  public void setNElements(int nElements) {
    //  Allocate a new string array
    mElements = new String[nElements];
  }

  public void set(int index, String value) {
    mElements[index] = value;
  }

  public String get(int index) {
    return mElements[index];
  }

  public int getNElements() {
    return mElements.length;
  }

  public String toString() {
    if (mElements == null || mElements.length == 0) {
      return "";
    }
    StringBuffer buffer = new StringBuffer();
    for (int i = 0; i < mElements.length; i++) {
      buffer.append(mElements[i]);
      buffer.append(" ");
    }
    return buffer.toString();
  }

  /**
   * Parse a space delimited string into the StringList
   */
  public void parseString(String newList) {
    //  If the string is only white space set the StringList to the null set
    if (newList == null || newList.matches("\\s*")) {
      mElements = new String[0];
      return;
    }
    mElements = newList.split(" +");
  }

  /**
   * Parse a space delimited string into the StringList
   */
  public void parseString(String[] newList) {
    //  If the string is only white space set the StringList to the null set
    if (newList == null || newList.length == 0) {
      mElements = new String[0];
      return;
    }
    ArrayList elementArray = new ArrayList();
    for (int i = 0; i < newList.length; i++) {
      if (!newList[i].matches("\\s*")) {
        String[] stringArray = newList[i].split(" +");
        for (int stringIndex = 0; stringIndex < stringArray.length; stringIndex++) {
          elementArray.add(stringArray[stringIndex]);
        }
      }
    }
    if (elementArray.size() == 0) {
      mElements = new String[0];
    }
    else if (elementArray.size() == 1) {
      mElements = new String[1];
      mElements[0] = (String) elementArray.get(0);
    }
    else {
      mElements = (String[]) elementArray.toArray(new String[elementArray.size()]);
    }
  }

  public void parseString(StringList stringList) {
    if (stringList != null) {
      int nElements = stringList.mElements.length;
      if (nElements < 0) {
        mElements = new String[0];
      }
      else {
        mElements = new String[nElements];
        for (int i = 0; i < nElements; i++) {
          mElements[i] = stringList.mElements[i];
        }
      }
    }
  }

  public void parse(ComScriptCommand scriptCommand) throws InvalidParameterException {
    if (mSuccessiveEntriesAccumulate) {
      parseString(scriptCommand.getValues(mKey));
    }
    else {
      parseString(scriptCommand.getValue(mKey));
    }
  }

  public void updateComScript(ComScriptCommand scriptCommand)
      throws BadComScriptException {
    if (mKey == null || mKey.matches("\\s*")) {
      throw new IllegalArgumentException();
    }
    if (mSuccessiveEntriesAccumulate && mConvertToSingleEntry) {
      //Remove all entries before saving
      while (scriptCommand.deleteKey(mKey)) {
      }
    }
    if (mSuccessiveEntriesAccumulate && !mConvertToSingleEntry && getNElements() > 1) {
      //output separate entries
      scriptCommand.setValues(mKey, mElements);
    }
    else if (getNElements() > 0) {
      //output one entry
      scriptCommand.setValue(mKey, toString());
    }
    else {
      scriptCommand.deleteKey(mKey);
    }
  }
}
