package etomo.process;

import java.io.File;
import java.io.FileNotFoundException;

import etomo.util.TestUtilites;
import junit.framework.TestCase;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2012</p>
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
public class ProcessMessagesTest extends TestCase {
  public static final String rcsid = "$Id:$";

  public void testWarnings() {
    ProcessMessages messages = ProcessMessages.getInstance(null);
    File file = new File(TestUtilites.INSTANCE.getUnitTestData(), "aligna.log");
    try {
      messages.addProcessOutput(file);
    }
    catch (FileNotFoundException e) {
      fail(e.getMessage());
    }
    messages.printWarning();
    assertTrue("2 messages should be found", messages.warningListSize() == 2);
    assertTrue("first message shouldn't contain extra lines", messages.getWarning(0)
        .endsWith("Minimization error #3 - Iteration limit exceeded"));
  }

  public void testErrors() {
    ProcessMessages messages = ProcessMessages.getInstance(null);
    File file = new File(TestUtilites.INSTANCE.getUnitTestData(), "testErrors.log");
    try {
      messages.addProcessOutput(file);
    }
    catch (FileNotFoundException e) {
      fail(e.getMessage());
    }
    messages.printWarning();
    assertTrue(
        "3 errors should be found - prnstr('ERROR: & log.write('ERROR: should be ignored",
        messages.errorListSize() == 3);
    assertTrue(
        "ERROR: message should not contain extra lines when MultiLineMessages is off",
        messages.getError(0).endsWith("A. error line"));
    assertTrue(
        "Errno: message should not contain extra lines when MultiLineMessages is off",
        messages.getError(1).endsWith("C. error line"));
    assertTrue("Traceback message should always contain extra lines", messages
        .getError(2).indexOf("F. second error line") != -1);
  }
}
