package etomo.comscript;

/**
 * <p>Description: </p>
 *
 * <p>Copyright: Copyright (c) 2004</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $$Author$$
 *
 * @version $$Revision$$
 *
 * <p> $$Log$$ </p>
 */

public class ConstGotoParam {
  public static final String rcsid =
    "$$Id$$";
  
  public static final char DELIMITER = ':';
  public static final String COMMAND_NAME = "goto";
  
  protected String label = "";
  
  public ConstGotoParam() {
    reset();
  }

  protected void reset() {
    label = "";
  }

  /**
   * @return String
   */
  public String getLabel() {
    return label;
  }
  
}

