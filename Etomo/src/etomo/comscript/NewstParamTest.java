/*
 * Created on Sep 30, 2003
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package etomo.comscript;

import junit.framework.TestCase;

/**
 * @author sueh
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class NewstParamTest extends TestCase {
  private String inputFile = "inputfile";
  private String outputFile = "outputfile";
  private String xformFile = "xformfile";
  private String size = "0,1";
  private String offset = "3,4";
  private String oldSizeOption = "-si";
  private String sizeOption = "-SizeToOutputInXandY";
  private String offsetOption = "-OffsetsInXandY";
  private String xformOption = "-TransformFile";
  private String linearOption = "-LinearInterpolation";

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
   * @param arg0
   */
  public NewstParamTest(String arg0) {
    super(arg0);
  }

  public void testParsingAndUpdatingComScriptCommand()
  throws BadComScriptException {
    //ParseComScriptCommand() set values in NewstParam from a ComScriptCommand.
    //UpdateComScriptCommand() creates a ComScriptCommand from the values
    //in NewstParam.  They should be compatible.

    //Case: all options

    ComScriptCommand csc = getAllOptionsComScriptCommand();
    NewstParam np = new NewstParam();
    //test Parse
    testParseAllOptions(np, csc);
    //test Update
    String[] commandLine = testUpdate(np);
    //test compatibility of Update and Parse
    csc = new ComScriptCommand();
    csc.setCommandLineArgs(commandLine);
    np = new NewstParam();
    testParseAllOptions(np, csc);

    //Case: no options

    //test Parse - should reset
    csc = getNoOptionsComScriptCommand();
    testParseNoOptions(np, csc);
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
    ComScriptCommand csc = new ComScriptCommand();
    np.updateComScriptCommand(csc);
    String[] commandLine = csc.getCommandLineArgs();
    assertTrue("Testing -SizeToOutputInXandY ", testFor(commandLine, 
    sizeOption, size) >= 0);
    assertTrue("Testing -OffsetsInXandY ", testFor(commandLine, offsetOption, 
    offset) >= 0);
    assertTrue("Testing -TransformFile ", testFor(commandLine, xformOption, 
    xformFile) >= 0);
    assertTrue("Testing -SizeToOutputInXandY ", testFor(commandLine, 
    linearOption) >= 0);
    assertEquals("Testing input file ", commandLine.length - 2, testFor(
      commandLine, inputFile)
    );
    assertEquals("Testing output file ", commandLine.length - 1, testFor(
      commandLine, outputFile)
    );
    return commandLine;
  }

  private void testParseNoOptions(NewstParam np, ComScriptCommand csc) {
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
    assertEquals(np.getSizeToOutputInXandY(), "");
    assertEquals(np.getOffsetsInXandY(), "");
    assertFalse(np.isLinearInterpolation());
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
    ComScriptCommand csc = new ComScriptCommand();
    csc.setCommandLineArgs(s);
    return csc;
  }

  private ComScriptCommand getNoOptionsComScriptCommand() {
    int i = 4;
    String[] s = new String[i];
    s[0] = "";
    s[1] = "";
    s[2] = "";
    s[3] = "";
    ComScriptCommand csc = new ComScriptCommand();
    csc.setCommandLineArgs(s);
    return csc;
  }

  private int testFor(String[] sa, String match) {
    for (int i = 0; i < sa.length; i++) {
      System.err.println(sa[i]);
      if (sa[i] == match) {
        
        return i;
      }
    }
    return -1;
  }

  private int testFor(String[] sa, String match1, String match2) {
    //System.err.println("'" + match1 + "' '" + match2 + "'");
    for (int i = 0; i < sa.length; i++) {
      //System.err.println(sa[i]);
      //if (sa[i] == match1 && sa[i + 1] == match2) {
      if (sa[i] == match1) {
        return i;
      }
    }
    return -1;
  }
}
