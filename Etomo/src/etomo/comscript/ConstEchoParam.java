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

public class ConstEchoParam {
  public static final String rcsid =
    "$$Id$$";
  
  public static final String COMMAND_NAME = "echo";
  
  protected StringBuffer string;
  
  public ConstEchoParam() {
    reset();
  }

  protected void reset() {
    string = new StringBuffer();
  }
  
  /**
   * @return String
   */
  public String getString() {
    return string.toString();
  }
  
}

