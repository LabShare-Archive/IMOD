package etomo.type;

import java.util.Properties;

import etomo.storage.Storable;

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
* <p> $Log$ </p>
*/
public class EtomoVersion implements Storable {
  public static  final String  rcsid =  "$Id$";
  
  private static final String defaultName = "Version";
  String sections[] = null;
  String name;
  
  public EtomoVersion() {
    name = defaultName;
  }
  
  public EtomoVersion(String version) {
    set(version);
    name = defaultName;
  }
  
  EtomoVersion set(String version) {
    if (version == null || version.matches("\\s*")) {
      sections = null;
      return this;
    }
    String[] strings = version.trim().split("\\.");
    if (strings == null || strings.length == 0) {
      sections = null;
      return this;
    }
    sections = new String[strings.length];
    for (int i = 0; i < sections.length; i++) {
      sections[i] = strings[i].trim();
    }
    return this;
  }
  
  public EtomoVersion set(EtomoVersion that) {
    if (that.isNull()) {
      sections = null;
    }
    else {
      sections = new String[that.sections.length];
      for (int i = 0; i < sections.length; i++) {
        sections[i] = that.sections[i];
      }
    }
    return this;
  }
  
  public boolean earlierOrEqualTo(EtomoVersion that) {
    if (isNull() || that.isNull()) {
      return false;
    }
    for (int i = 0; i < Math.min(sections.length, that.sections.length); i++) {
      //this instance is later
      if (sections[i].compareToIgnoreCase(that.sections[i]) > 0) {
        return false;
      }
      //this instance is earlier
      if (sections[i].compareToIgnoreCase(that.sections[i]) < 0) {
        return true;
      }
    }
    //So far they are equal
    //If this instance is shorter then it must be earlier or equal to the other
    if (sections.length < that.sections.length) {
      return true;
    }
    //This instance must be longer
    //It must be all blanks or zeros to be equal to the other
    for (int i = that.sections.length; i < sections.length; i++) {
      if (that.sections[i] != null || !that.sections[i].matches("\\s*")) {
        //test for zero
        try {
          if (Integer.parseInt(that.sections[i]) != 0) {
            return false;
          }
        }
        catch (NumberFormatException e) {
          return false;
        }
        return false;
      }
    }
    return true;
  }
  
  public String toString() {
    if (isNull()) {
      return "";
    }
    StringBuffer buffer = new StringBuffer(sections[0]);
    for (int i = 1; i < sections.length; i++) {
      buffer.append("." + sections[i]);
    }
    return buffer.toString();
  }
  
  public void store(Properties props) {
    props.setProperty(name, toString());
  }

  public void store(Properties props, String prepend) {
    props.setProperty(prepend + "." + name, toString());
  }
  
  
  public void load(Properties props) {
    set(props.getProperty(name));
  }
  public void load(Properties props, String prepend) {
    set(props.getProperty(prepend + "." + name));
  }
  

  
  boolean isNull() {
    if (sections == null || sections.length == 0) {
      return true;
    }
    return false;
  }


}
