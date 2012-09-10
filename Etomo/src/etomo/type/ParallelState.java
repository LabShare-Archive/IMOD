package etomo.type;

import java.util.Properties;

import etomo.BaseManager;

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
 * <p> $Log$
 * <p> Revision 1.5  2009/09/05 00:31:05  sueh
 * <p> bug# 1256 Changed testIterationList to IteratorElementList type.
 * <p>
 * <p> Revision 1.4  2008/06/20 18:57:42  sueh
 * <p> bug# 1119 ParsedArrayDescriptor can be either Matlab or non-Matlab now, so I need to explicitly choose an iterator array when I need one.
 * <p>
 * <p> Revision 1.3  2008/04/02 02:01:20  sueh
 * <p> bug# 1097 Made non-matlab syntax the default in the ParsedElements
 * <p> classes.  This is because matlab uses "NaN", which is unhealthy for
 * <p> Etomo and IMOD.
 * <p>
 * <p> Revision 1.2  2007/11/09 17:46:04  sueh
 * <p> bug# 1047 Changed the names of NAD fields for clarity.
 * <p>
 * <p> Revision 1.1  2007/11/06 19:38:00  sueh
 * <p> bug# 1047 Added process state for anisotropic diffusion.
 * <p> </p>
 */
public final class ParallelState extends BaseState {
  public static final String rcsid = "$Id$";

  private static final String GROUP_KEY = "State";

  private final ParsedArray testKValueList = ParsedArray.getInstance(
      EtomoNumber.Type.DOUBLE, "TestKValueList");
  private final EtomoNumber testIteration = new EtomoNumber("TestIteration");
  private final EtomoNumber testKValue = new EtomoNumber(EtomoNumber.Type.DOUBLE,
      "TestKValue");

  /**
   * IterationList may contain array descriptors in the form start-end.
   * Example: "2,4 - 9,10".
   */
  private final IteratorElementList testIterationList;

  public ParallelState(BaseManager manager, AxisID axisID) {
    testIterationList = new IteratorElementList(manager, axisID, "TestIterationList");
  }

  public void store(final Properties props) {
    store(props, "");
  }

  public void store(final Properties props, String prepend) {
    super.store(props, prepend);
    prepend = createPrepend(prepend);
    testKValueList.store(props, prepend);
    testIteration.store(props, prepend);
    testIterationList.store(props, prepend);
    testKValue.store(props, prepend);
  }

  public void load(final Properties props) {
    load(props, "");
  }

  public void load(final Properties props, String prepend) {
    super.load(props, prepend);
    prepend = createPrepend(prepend);
    testKValueList.load(props, prepend);
    testIteration.load(props, prepend);
    testIterationList.load(props, prepend);
    testKValue.load(props, prepend);
  }

  String createPrepend(final String prepend) {
    if (prepend.equals("")) {
      return GROUP_KEY + "." + ParallelMetaData.ANISOTROPIC_DIFFUSION_GROUP_KEY;
    }
    return prepend + "." + GROUP_KEY + "."
        + ParallelMetaData.ANISOTROPIC_DIFFUSION_GROUP_KEY;
  }

  public void setTestKValueList(String input) {
    testKValueList.setRawString(input);
  }

  public ParsedArray getTestKValueList() {
    return testKValueList;
  }

  public void setTestIteration(int input) {
    testIteration.set(input);
  }

  public ConstEtomoNumber getTestIteration() {
    return testIteration;
  }

  public void setTestIterationList(IteratorElementList input) {
    testIterationList.setList(input);
  }

  public IteratorElementList getTestIterationList() {
    return testIterationList;
  }

  public void setTestKValue(double input) {
    testKValue.set(input);
  }

  public ConstEtomoNumber getTestKValue() {
    return testKValue;
  }
}
