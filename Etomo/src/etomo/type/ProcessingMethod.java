package etomo.type;

import java.util.Properties;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2010</p>
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
public final class ProcessingMethod {
  public static final String rcsid = "$Id$";

  public static ProcessingMethod LOCAL_CPU = new ProcessingMethod("LOCAL_CPU");
  public static ProcessingMethod LOCAL_GPU = new ProcessingMethod("LOCAL_GPU");
  public static ProcessingMethod PP_CPU = new ProcessingMethod("PP_CPU");
  public static ProcessingMethod PP_GPU = new ProcessingMethod("PP_GPU");
  public static ProcessingMethod QUEUE = new ProcessingMethod("QUEUE");

  public static ProcessingMethod DEFAULT = LOCAL_CPU;

  private static final String key = "ProcessingMethod";

  private final String value;

  private ProcessingMethod(final String value) {
    this.value = value;
  }

  public boolean isLocal() {
    return this == LOCAL_CPU || this == LOCAL_GPU;
  }
  
  public String toString() {
    return value;
  }

  private static ProcessingMethod getInstance(String string) {
    if (LOCAL_CPU.value.equals(string)) {
      return LOCAL_CPU;
    }
    if (LOCAL_GPU.value.equals(string)) {
      return LOCAL_GPU;
    }
    if (PP_CPU.value.equals(string)) {
      return PP_CPU;
    }
    if (PP_GPU.value.equals(string)) {
      return PP_GPU;
    }
    if (QUEUE.value.equals(string)) {
      return QUEUE;
    }
    return null;
  }

  public static void remove(Properties props, String prepend) {
    if (props == null) {
      return;
    }
    props.remove(createKey(prepend));
  }

  public void store(Properties props, String prepend) {
    if (props == null) {
      return;
    }
    props.setProperty(createKey(prepend), value);
  }

  public static ProcessingMethod load(Properties props, String prepend) {
    if (props == null) {
      return null;
    }
    return getInstance(props.getProperty(createKey(prepend)));
  }

  private static String createKey(String prepend) {
    if (prepend == null || prepend.matches("\\s*")) {
      return key;
    }
    return prepend + "." + key;
  }
}
