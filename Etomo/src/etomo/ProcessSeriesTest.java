package etomo;

import etomo.type.AxisID;
import etomo.type.DialogType;
import etomo.type.Run3dmodMenuOptions;
import etomo.ui.swing.Deferred3dmodButton;
import junit.framework.TestCase;

/**
* <p>Description: Unit tests for ProcessSeries.  Does not test code involving ProcessData,
* which has a privacy level that prevents the creation of test data.  For the sake of
* convenience, only testing processes set up using TaskInterface.</p>
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
public final class ProcessSeriesTest extends TestCase {
  public static final String rcsid = "$Id:$";

  private ProcessSeries testInstance = null;

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    EtomoDirector.main(JUnitTests.ETOMO_ARGUMENTS);
    testInstance = new ProcessSeries(EtomoDirector.INSTANCE.getCurrentManagerForTest(),
        DialogType.FIDUCIAL_MODEL);
    testInstance.setNextProcess(Task.NEXT_TASK);
    testInstance.addProcess(Task.TASK1);
    testInstance.addProcess(Task.TASK2);
    testInstance.addProcess(Task.TASK3);
    testInstance.setLastProcess(Task.LAST_TASK);
    testInstance.setRun3dmodDeferred(new DummyDeferred3dmodButton(), null);
  }

  public void testStartNextProcess() {
    assertTrue("starts next process", testInstance.startNextProcess(AxisID.FIRST, null));
    assertEquals("process deleted after start", Task.TASK1.toString(),
        testInstance.peekNextProcess());
    assertTrue("starts first list process",
        testInstance.startNextProcess(AxisID.FIRST, null));
    assertEquals("process deleted after start", Task.TASK2.toString(),
        testInstance.peekNextProcess());
    assertTrue("starts second list process",
        testInstance.startNextProcess(AxisID.FIRST, null));
    assertEquals("process deleted after start", Task.TASK3.toString(),
        testInstance.peekNextProcess());
    assertTrue("starts third list process",
        testInstance.startNextProcess(AxisID.FIRST, null));
    assertEquals("process deleted after start", Task.LAST_TASK.toString(),
        testInstance.peekNextProcess());
    assertTrue("starts last process", testInstance.startNextProcess(AxisID.FIRST, null));
    assertEquals("process deleted after start", "3dmod", testInstance.peekNextProcess());
    assertTrue("starts 3dmod", testInstance.startNextProcess(AxisID.FIRST, null));
    assertFalse("no processes left", testInstance.startNextProcess(AxisID.FIRST, null));
    // test dropping fail
    testInstance.setNextProcess(Task.NEXT_TASK);
    testInstance.setFailProcess(Task.FAIL_TASK);
    testInstance.startNextProcess(AxisID.FIRST, null);
    testInstance.startNextProcess(AxisID.FIRST, null);
    assertFalse("all processes completed and fail process dropped",
        testInstance.willProcessBeDropped(null));
  }

  public void testStartFailProcess() {
    // test null fail
    testInstance.startFailProcess(AxisID.FIRST, null);
    assertFalse("null fail process still causes the process series to be cleaned up",
        testInstance.startNextProcess(AxisID.FIRST, null));
    // test non-null fail
    testInstance.setNextProcess(Task.NEXT_TASK);
    testInstance.setFailProcess(Task.FAIL_TASK);
    testInstance.startFailProcess(AxisID.FIRST, null);
    assertFalse("starting fail process caused all processes to be dropped",
        testInstance.startNextProcess(AxisID.FIRST, null));
    // test force next process
    testInstance.addProcess(Task.TASK1, true);
    testInstance.addProcess(Task.TASK2);
    testInstance.addProcess(Task.TASK3);
    testInstance.setFailProcess(Task.FAIL_TASK);
    testInstance.startNextProcess(AxisID.FIRST, null);
    testInstance.startFailProcess(AxisID.FIRST, null);
    assertEquals(
        "force next process caused startNextProcess to be run instead of fail functionality",
        Task.TASK3.toString(), testInstance.peekNextProcess());
    testInstance.startFailProcess(AxisID.FIRST, null);
    assertFalse("fail process was not deleted, because force next process was on",
        testInstance.startNextProcess(AxisID.FIRST, null));
  }

  public void testGetDialogType() {
    assertEquals("dialog type is saved", DialogType.FIDUCIAL_MODEL,
        testInstance.getDialogType());
  }

  public void testGetLastProcess() {
    assertEquals("returns last process", Task.LAST_TASK.toString(),
        testInstance.getLastProcess());
    // test null last process
    testInstance.clearProcesses();
    assertNull("return null when there is no last process", testInstance.getLastProcess());
  }

  public void testSetNextProcess() {
    assertEquals("next process is set.", Task.NEXT_TASK.toString(),
        testInstance.peekNextProcess());
    testInstance.setNextProcess(Task.ANOTHER_NEXT_TASK);
    assertEquals("Only one next process can be set.", Task.ANOTHER_NEXT_TASK.toString(),
        testInstance.peekNextProcess());
  }

  public void testAddProcess() {
    testInstance.startNextProcess(AxisID.FIRST, null);
    assertEquals("processes are added to process list in order.", Task.TASK1.toString(),
        testInstance.peekNextProcess());
    testInstance.startNextProcess(AxisID.FIRST, null);
    assertEquals("processes are added to process list in order.", Task.TASK2.toString(),
        testInstance.peekNextProcess());
    testInstance.startNextProcess(AxisID.FIRST, null);
    assertEquals("processes are added to process list in order.", Task.TASK3.toString(),
        testInstance.peekNextProcess());
  }

  public void testClearProcesses() {
    testInstance.setFailProcess(Task.FAIL_TASK);
    testInstance.clearProcesses();
    assertFalse("processes dropped", testInstance.willProcessBeDropped(null));
    // test that fail was dropped
    testInstance.setNextProcess(Task.NEXT_TASK);
    testInstance.startFailProcess(AxisID.FIRST, null);
    assertFalse("null fail process still causes the process series to be cleaned up",
        testInstance.startNextProcess(AxisID.FIRST, null));
  }

  public void testWillProcessBeDropped() {
    assertTrue("checks next process", testInstance.willProcessBeDropped(null));
    testInstance.startNextProcess(AxisID.FIRST, null);
    assertTrue("checks process list", testInstance.willProcessBeDropped(null));
    testInstance.startNextProcess(AxisID.FIRST, null);
    assertTrue("checks process list", testInstance.willProcessBeDropped(null));
    testInstance.startNextProcess(AxisID.FIRST, null);
    assertTrue("checks process list", testInstance.willProcessBeDropped(null));
    testInstance.startNextProcess(AxisID.FIRST, null);
    assertTrue("checks last process", testInstance.willProcessBeDropped(null));
    testInstance.startNextProcess(AxisID.FIRST, null);
    assertFalse("doesn't check run 3dmod", testInstance.willProcessBeDropped(null));
    // test fail process
    testInstance.clearProcesses();
    testInstance.setFailProcess(Task.FAIL_TASK);
    assertFalse("doesn't check fail process", testInstance.willProcessBeDropped(null));
    // test droppable processes
    testInstance.clearProcesses();
    testInstance.setNextProcess(Task.DROPPABLE_NEXT_TASK);
    testInstance.addProcess(Task.DROPPABLE_TASK1);
    assertFalse("droppable next process - return false",
        testInstance.willProcessBeDropped(null));
    testInstance.startNextProcess(AxisID.FIRST, null);
    assertFalse("droppable process list process - return false",
        testInstance.willProcessBeDropped(null));
  }

  public void testWillProcessListBeDropped() {
    testInstance.clearProcesses();
    testInstance.setNextProcess(Task.NEXT_TASK);
    assertFalse("ignores next process", testInstance.willProcessListBeDropped());
    testInstance.clearProcesses();
    testInstance.setNextProcess(Task.TASK1);
    assertTrue("checks process list", testInstance.willProcessBeDropped(null));
  }

  public void testPeekNextProcess() {
    assertEquals("next process", Task.NEXT_TASK.toString(),
        testInstance.peekNextProcess());
    testInstance.startNextProcess(AxisID.FIRST, null);
    assertEquals("first process on process list", Task.TASK1.toString(),
        testInstance.peekNextProcess());
    testInstance.startNextProcess(AxisID.FIRST, null);
    assertEquals("second process on process list", Task.TASK2.toString(),
        testInstance.peekNextProcess());
    testInstance.startNextProcess(AxisID.FIRST, null);
    assertEquals("third process on process list", Task.TASK3.toString(),
        testInstance.peekNextProcess());
    testInstance.startNextProcess(AxisID.FIRST, null);
    assertEquals("last process", Task.LAST_TASK.toString(),
        testInstance.peekNextProcess());
    testInstance.startNextProcess(AxisID.FIRST, null);
    assertEquals("run 3dmod", "3dmod", testInstance.peekNextProcess());
    testInstance.startNextProcess(AxisID.FIRST, null);
    assertNull("no processes left", testInstance.peekNextProcess());
    // test fail process
    testInstance.setFailProcess(Task.FAIL_TASK);
    assertNull("ignores fail process", testInstance.peekNextProcess());
  }

  public void testSetLastProcess() {
    testInstance.clearProcesses();
    testInstance.setLastProcess(Task.LAST_TASK);
    assertEquals("last process is set.", Task.LAST_TASK.toString(),
        testInstance.peekNextProcess());
    testInstance.setLastProcess(Task.ANOTHER_LAST_TASK);
    assertEquals("Only one last process can be set.", Task.ANOTHER_LAST_TASK.toString(),
        testInstance.peekNextProcess());
  }

  public void testSetFailProcess() {
    testInstance.setFailProcess(Task.FAIL_TASK);
    testInstance.startFailProcess(AxisID.FIRST, null);
    assertFalse("fail completed and all processes dropped",
        testInstance.willProcessBeDropped(null));
  }

  public void testSetRun3dmodDeferred() {
    testInstance.clearProcesses();
    testInstance.setRun3dmodDeferred(new DummyDeferred3dmodButton(), null);
    assertEquals("run 3dmod set", "3dmod", testInstance.peekNextProcess());
  }

  private static final class Task implements TaskInterface {
    private static final Task NEXT_TASK = new Task(false);
    private static final Task ANOTHER_NEXT_TASK = new Task(false);
    private static final Task DROPPABLE_NEXT_TASK = new Task(true);
    private static final Task TASK1 = new Task(false);
    private static final Task DROPPABLE_TASK1 = new Task(true);
    private static final Task TASK2 = new Task(false);
    private static final Task TASK3 = new Task(false);
    private static final Task LAST_TASK = new Task(false);
    private static final Task ANOTHER_LAST_TASK = new Task(false);
    private static final Task FAIL_TASK = new Task(false);

    private final boolean droppable;

    private Task(final boolean droppable) {
      this.droppable = droppable;
    }

    public boolean okToDrop() {
      return droppable;
    }
  }

  private static final class DummyDeferred3dmodButton implements Deferred3dmodButton {
    public void action(final Run3dmodMenuOptions options) {
    }
  }
}
