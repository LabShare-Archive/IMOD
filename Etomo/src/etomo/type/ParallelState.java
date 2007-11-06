package etomo.type;

import java.util.Properties;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2006</p>
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
public final class ParallelState extends BaseState {
  public static final String rcsid = "$Id$";

  private static final String GROUP_KEY = "State";

  private final ParsedArray kValueList = ParsedArray.getInstance(
      EtomoNumber.Type.FLOAT, "KValueList");
  private final EtomoNumber iteration = new EtomoNumber("Iteration");
  /**
   * IterationList may contain array descriptors in the form start-end.
   * Example: "2,4 - 9,10".
   */
  private final ParsedArray iterationList = ParsedArray
      .getIteratorInstance("IterationList");
  private final EtomoNumber kValue = new EtomoNumber(EtomoNumber.Type.FLOAT,
      "KValue");

  public void store(final Properties props) {
    store(props, "");
  }

  public void store(final Properties props, String prepend) {
    super.store(props, prepend);
    prepend = createPrepend(prepend);
    kValueList.store(props, prepend);
    iteration.store(props, prepend);
    iterationList.store(props, prepend);
    kValue.store(props, prepend);
  }

  public void load(final Properties props) {
    load(props, "");
  }

  public void load(final Properties props, String prepend) {
    super.load(props, prepend);
    prepend = createPrepend(prepend);
    kValueList.load(props, prepend);
    iteration.load(props, prepend);
    iterationList.load(props, prepend);
    kValue.load(props, prepend);
  }

  String createPrepend(final String prepend) {
    if (prepend.equals("")) {
      return GROUP_KEY + "." + ParallelMetaData.ANISOTROPIC_DIFFUSION_GROUP_KEY;
    }
    return prepend + "." + GROUP_KEY + "."
        + ParallelMetaData.ANISOTROPIC_DIFFUSION_GROUP_KEY;
  }

  public void setKValueList(String input) {
    kValueList.setRawString(input);
  }

  public ParsedArray getKValueList() {
    return kValueList;
  }

  public void setIteration(int input) {
    iteration.set(input);
  }

  public ConstEtomoNumber getIteration() {
    return iteration;
  }

  public void setIterationList(String input) {
    iterationList.setRawString(input);
  }

  public ParsedArray getIterationList() {
    return iterationList;
  }

  public void setKValue(float input) {
    kValue.set(input);
  }

  public ConstEtomoNumber getKValue() {
    return kValue;
  }
}
