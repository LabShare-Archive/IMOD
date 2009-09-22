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
 * <p> Revision 3.6  2005/01/08 01:41:07  sueh
 * <p> bug# 578 Added AxisID to NewstParam constructor.
 * <p>
 * <p> Revision 3.5  2004/06/28 20:28:07  sueh
 * <p> bug# 451
 * <p>
 * <p> Revision 3.4  2004/03/12 17:04:07  rickg
 * <p> Some of the ComScriptCommand objects didn't have their
 * <p> command string specified
 * <p>
 * <p> Revision 3.3  2004/02/18 00:52:42  rickg
 * <p> Restructured testing to match newstack PIP
 * <p> Remove == from string comparisons
 * <p>
 */

package etomo.comscript;

import etomo.type.AxisID;
import junit.framework.TestCase;

public class NewstParamTest extends TestCase {
  private String inputFile = "inputfile";
  private String outputFile = "outputfile";
  private String xformFile = "xformfile";
  private String size = "0,1";
  private String offset = "3,4";

  private String inputFileOption = "-input";
  private String outputFileOption = "-output";
  private String sizeOption = "-size";
  private String offsetOption = "-offset";
  private String xformOption = "-xform";
  private String linearOption = "-linear";

  /*
   * @see TestCase#setUp()
   */
  protected void setUp() throws Exception {
    super.setUp();
  }

  /*
   * @see TestCase#tearDown()
   */
  protected void tearDown() throws Exception {
    super.tearDown();
  }

  /**
   * Constructor for NewstParamTest.
   * 
   * @param arg0
   */
  public NewstParamTest(String arg0) {
    super(arg0);
  }

  /**
   * Test the old syntx newst command
   */
  public void testParseOldSyntax(boolean caseInsensitive,
      boolean separateWithASpace) {
    String oldSizeOption = "-si";
    String oldOffsetOption = "-offset";
    String oldXformOption = "-xform";
    String oldLinearOption = "-linear";
    int i = 9;
    String[] s = new String[i];
    s[0] = sizeOption;
    s[1] = size;
    s[2] = offsetOption;
    s[3] = offset;
    s[4] = xformOption;
    s[5] = xformFile;
    s[6] = linearOption;
    s[i - 2] = inputFile;
    s[i - 1] = outputFile;
    ComScriptCommand scriptCommand = new ComScriptCommand(caseInsensitive,
        separateWithASpace);
    scriptCommand.setCommand("newst");
    scriptCommand.setCommandLineArgs(s);
    NewstParam newstParam = new NewstParam(AxisID.ONLY);
    try {
      newstParam.parseComScriptCommand(scriptCommand);
    }
    catch (Exception e) {
      fail("Unexpected exception: " + e.getClass().getName() + ": "
          + e.getMessage());
    }

    //  Test to see if the old old syntax was parsed correctly
    assertEquals("Input filename mismatch", inputFile, newstParam
        .getInputFile());
    assertEquals("Output filename mismatch", outputFile, newstParam
        .getOutputFile());
    assertEquals("Transform filename mismatch", xformFile, newstParam
        .getTransformFile());
    assertEquals("Size parameter mismatch", size, newstParam
        .getSizeToOutputInXandY());
    assertEquals("Offset parameter mismatch", offset, newstParam
        .getOffsetsInXandY());
    assertTrue("Linear interpolation mismatch", newstParam
        .isLinearInterpolation());
  }

  public void testParsingAndUpdatingComScriptCommand()
      throws BadComScriptException {
    //ParseComScriptCommand() set values in NewstParam from a
    // ComScriptCommand.
    //UpdateComScriptCommand() creates a ComScriptCommand from the values
    //in NewstParam. They should be compatible.

    //Case: all options

    ComScriptCommand csc = getAllOptionsComScriptCommand();
    NewstParam np = new NewstParam(AxisID.ONLY);
    //test Parse
    testParseAllOptions(np, csc);

    //test Update
    String[] commandLine = testUpdate(np);

    //test compatibility of Update and Parse
    csc = new ComScriptCommand(false,false);
    csc.setCommandLineArgs(commandLine);
    np = new NewstParam(AxisID.ONLY);
    testParseAllOptions(np, csc);
  }

  /**
   * Test the unitialized values
   */
  public void testNoOptions() {
    //Case: no options

    //test Parse - should reset
    ComScriptCommand csc = getNoOptionsComScriptCommand();
    NewstParam np = new NewstParam(AxisID.ONLY);
    try {
      np.parseComScriptCommand(csc);
    }
    catch (Exception e) {
      fail("Unexpected exception: " + e.getClass().getName() + ": "
          + e.getMessage());
    }
    assertEquals(np.getInputFile(), "");
    assertEquals(np.getOutputFile(), "");
    assertEquals(np.getTransformFile(), "");
    String unitFISPair = String.valueOf(Integer.MIN_VALUE) + ","
        + String.valueOf(Integer.MIN_VALUE);
    assertEquals(np.getSizeToOutputInXandY(), unitFISPair);
    assertEquals(np.getOffsetsInXandY(), "");
    assertFalse(np.isLinearInterpolation());
  }

  private void testParseAllOptions(NewstParam np, ComScriptCommand csc) {
    try {
      np.parseComScriptCommand(csc);
    }
    catch (Exception e) {
      fail("Unexpected exception: " + e.getClass().getName() + ": "
          + e.getMessage());
    }
    assertEquals("Input filename mismatch", inputFile, np.getInputFile());
    assertEquals("Output filename mismatch", outputFile, np.getOutputFile());
    assertEquals("Transform filename mismatch", xformFile, np
        .getTransformFile());
    assertEquals("Size parameter mismatch", size, np.getSizeToOutputInXandY());
    assertEquals("Size parameter mismatch", offset, np.getOffsetsInXandY());
    assertTrue("Linear interpolation mismatch", np.isLinearInterpolation());
  }

  private String[] testUpdate(NewstParam np) throws BadComScriptException {
    //  Create a new ComScriptCommand with a newstack command
    ComScriptCommand csc = new ComScriptCommand(false,false);
    csc.setCommand("newstack");

    //  Update the command with suppplied parameters
    np.updateComScriptCommand(csc);

    //  Exctract the command line from the ComScriptCommand
    String[] commandLine = csc.getCommandLineArgs();

    //  Analysze the command line
    assertTrue("Testing -SizeToOutputInXandY ", findStringPair(commandLine,
        sizeOption, size) >= 0);
    assertTrue("Testing -OffsetsInXandY ", findStringPair(commandLine,
        offsetOption, offset) >= 0);
    assertTrue("Testing -TransformFile ", findStringPair(commandLine,
        xformOption, xformFile) >= 0);
    assertTrue("Testing -SizeToOutputInXandY ", findString(commandLine,
        linearOption) >= 0);
    assertTrue("Testing input file ", findStringPair(commandLine,
        inputFileOption, inputFile) >= 0);

    assertTrue("Testing output file ", findStringPair(commandLine,
        outputFileOption, outputFile) >= 0);
    return commandLine;
  }

  private ComScriptCommand getAllOptionsComScriptCommand() {
    int i = 9;
    String[] s = new String[i];
    s[0] = sizeOption;
    s[1] = size;
    s[2] = offsetOption;
    s[3] = offset;
    s[4] = xformOption;
    s[5] = xformFile;
    s[6] = linearOption;
    s[i - 2] = inputFile;
    s[i - 1] = outputFile;
    ComScriptCommand csc = new ComScriptCommand(false,false);
    csc.setCommandLineArgs(s);
    csc.setCommand("newstack");
    return csc;
  }

  private ComScriptCommand getNoOptionsComScriptCommand() {
    int i = 4;
    String[] s = new String[i];
    s[0] = "";
    s[1] = "";
    s[2] = "";
    s[3] = "";
    ComScriptCommand csc = new ComScriptCommand(false,false);
    csc.setCommandLineArgs(s);
    csc.setCommand("newstack");
    return csc;
  }

  /*
   * Return the index of the test string within the array or -1 if the array
   * does not contain the string.
   */
  private int findString(String[] sa, String match) {
    for (int i = 0; i < sa.length; i++) {
      if (sa[i].equals(match)) {
        return i;
      }
    }
    return -1;
  }

  /*
   * Return the index of the firt string in a matching pair or -1 if the array
   * does not contain the pair string in order.
   */
  private int findStringPair(String[] sa, String match1, String match2) {
    for (int i = 0; i < sa.length - 1; i++) {
      if (sa[i].equals(match1) && sa[i + 1].equals(match2)) {
        return i;
      }
    }
    return -1;
  }
}
