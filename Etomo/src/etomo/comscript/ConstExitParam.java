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
 * <p> $$Log$
 * <p> $Revision 1.1  2004/08/19 01:31:23  sueh
 * <p> $Constant object for the exit command
 * <p> $$ </p>
 */

public class ConstExitParam {
  public static final String rcsid = "$$Id$$";

  public static final String COMMAND_NAME = "exit";

  protected int resultValue;

  public ConstExitParam() {
    reset();
  }

  protected void reset() {
    resultValue = 0;
  }

  /**
   * @return String
   */
  public int getResultValue() {
    return resultValue;
  }

}
