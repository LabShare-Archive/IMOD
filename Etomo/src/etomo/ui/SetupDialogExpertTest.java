package etomo.ui;

import etomo.ApplicationManager;
import etomo.EtomoDirector;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.ViewType;
import junit.framework.TestCase;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2008</p>
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
 * <p> Revision 1.1  2009/08/24 20:24:19  sueh
 * <p> bug# 1254 Unit tests for SetupDialogExpert.
 * <p> </p>
 */
public final class SetupDialogExpertTest extends TestCase {
  public static final String rcsid = "$Id$";
  
  protected void setUp() throws Exception {
    EtomoDirector.INSTANCE.openTomogram(true,AxisID.ONLY);
  }

  public void testGetInstance() {
    ApplicationManager manager = (ApplicationManager) EtomoDirector.INSTANCE
        .getCurrentManagerForTest();
    manager.openSetupDialog();
    assertNotNull("Manager should be able to create expert.", manager
        .getSetupDialogExpert());
  }

  public void testAction() {
    ApplicationManager manager = (ApplicationManager) EtomoDirector.INSTANCE
        .getCurrentManagerForTest();
    manager.openSetupDialog();
    SetupDialogExpert expert = manager.getSetupDialogExpert();
    expert.action(SetupDialog.SINGLE_AXIS_LABEL);
    expert.action(SetupDialog.SINGLE_FRAME_LABEL);
    expert.action(SetupDialog.MONTAGE_LABEL);
  }

  public void testCheckForSharedDirectory() {
    ApplicationManager manager = (ApplicationManager) EtomoDirector.INSTANCE
        .getCurrentManagerForTest();
    manager.openSetupDialog();
    SetupDialogExpert expert = manager.getSetupDialogExpert();
    expert.checkForSharedDirectory();
  }

  public void testDoAutomation() {
    ApplicationManager manager = (ApplicationManager) EtomoDirector.INSTANCE
        .getCurrentManagerForTest();
    manager.openSetupDialog();
    SetupDialogExpert expert = manager.getSetupDialogExpert();
    expert.doAutomation();
  }

  public void testGetAxisType() {
    ApplicationManager manager = (ApplicationManager) EtomoDirector.INSTANCE
        .getCurrentManagerForTest();
    manager.openSetupDialog();
    SetupDialogExpert expert = manager.getSetupDialogExpert();
    assertEquals("Should default to dual axis.", expert.getAxisType(),
        AxisType.DUAL_AXIS);
  }

  public void testGetContainer() {
    ApplicationManager manager = (ApplicationManager) EtomoDirector.INSTANCE
        .getCurrentManagerForTest();
    manager.openSetupDialog();
    SetupDialogExpert expert = manager.getSetupDialogExpert();
    assertNotNull("Container should exist", expert.getContainer());
  }

  public void testGetCurrentBackupDirectory() {
    ApplicationManager manager = (ApplicationManager) EtomoDirector.INSTANCE
        .getCurrentManagerForTest();
    manager.openSetupDialog();
    SetupDialogExpert expert = manager.getSetupDialogExpert();
    assertTrue(
        "Backup directory was not entered so it should be the current directory.",
        expert.getCurrentBackupDirectory().equals(
            System.getProperty("user.dir")));
  }

  public void testSetViewType() {
    ApplicationManager manager = (ApplicationManager) EtomoDirector.INSTANCE
        .getCurrentManagerForTest();
    manager.openSetupDialog();
    SetupDialogExpert expert = manager.getSetupDialogExpert();
    expert.setViewType(ViewType.MONTAGE);
  }

  public void testDone() {
    ApplicationManager manager = (ApplicationManager) EtomoDirector.INSTANCE
        .getCurrentManagerForTest();
    manager.openSetupDialog();
    assertFalse("Fields are not filled so done should fail.", manager
        .doneSetupDialog());
  }
}
