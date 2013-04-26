package etomo.logic;

import etomo.storage.Directive;
import etomo.storage.DirectiveDescr;
import etomo.storage.DirectiveDescrEtomoColumn;
import etomo.storage.DirectiveValueType;
import etomo.type.AxisID;
import etomo.type.DirectiveFileType;
import etomo.ui.DirectiveDisplaySettings;
import junit.framework.TestCase;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2013</p>
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
public class DirectiveToolTest extends TestCase {
  public static final String rcsid = "$Id:$";

  public void testIsDirectiveIncluded() {
    TestSettings settings = new TestSettings();
    DirectiveTool tool = new DirectiveTool(DirectiveFileType.SCOPE, false, settings);
    assertFalse("Null directive does not cause error",
        tool.isDirectiveIncluded(null, null));

    settings.setIncludeSD(true);
    TestDescr descr = new TestDescr();
    descr.setEtomoColumn(DirectiveDescrEtomoColumn.SD);
    descr.setTemplate(true);
    Directive directive = new Directive(descr);
    assertTrue("can return true when a template editor encounters a template directive",
        tool.isDirectiveIncluded(directive, null));

    descr = new TestDescr();
    descr.setEtomoColumn(DirectiveDescrEtomoColumn.SD);
    descr.setBatch(true);
    directive = new Directive(descr);
    assertFalse("returns false when a template editor encounters a batch directive",
        tool.isDirectiveIncluded(directive, null));

    tool = new DirectiveTool(DirectiveFileType.BATCH, false, settings);
    descr = new TestDescr();
    descr.setEtomoColumn(DirectiveDescrEtomoColumn.SD);
    descr.setBatch(true);
    directive = new Directive(descr);
    assertTrue("can return true when a batch editor encounters a template directive",
        tool.isDirectiveIncluded(directive, null));

    descr = new TestDescr();
    descr.setEtomoColumn(DirectiveDescrEtomoColumn.SD);
    descr.setTemplate(true);
    directive = new Directive(descr);
    assertFalse("returns false when a batch editor encounters a template directive",
        tool.isDirectiveIncluded(directive, null));

    tool = new DirectiveTool(DirectiveFileType.SCOPE, false, settings);
    descr = new TestDescr();
    directive = new Directive(descr);
    assertFalse("include SD is on, but directive doesn't have SD in etomo column",
        tool.isDirectiveIncluded(directive, null));

    settings.setIncludeSD(false);
    descr = new TestDescr();
    descr.setEtomoColumn(DirectiveDescrEtomoColumn.SD);
    directive = new Directive(descr);
    assertFalse("directive has SD in etomo column, but include SD is off",
        tool.isDirectiveIncluded(directive, null));

    settings.setIncludeSD(true);
    assertTrue("directive has SD in etomo column and include SD is on",
        tool.isDirectiveIncluded(directive, null));

    tool = new DirectiveTool(null, false, settings);
    descr = new TestDescr();
    directive = new Directive(descr);
    assertFalse("missing type doesn't cause an error",
        tool.isDirectiveIncluded(directive, null));

    tool = new DirectiveTool(DirectiveFileType.SYSTEM, true, settings);
    directive.setInDirectiveFile(DirectiveFileType.SCOPE, null, true);
    assertFalse("In file but include isn't set",
        tool.isDirectiveIncluded(directive, null));

    settings.setInclude(DirectiveFileType.SCOPE.getIndex(), true);
    assertTrue("In file and include is set", tool.isDirectiveIncluded(directive, null));
    assertTrue("an any directive means that A and B are both in the file",
        tool.isDirectiveIncluded(directive, AxisID.FIRST));
    assertTrue("an any directive means that A and B are both in the file",
        tool.isDirectiveIncluded(directive, AxisID.SECOND));

    settings.setExclude(DirectiveFileType.USER.getIndex(), true);
    assertTrue("directive isn't in the user file, so exclude has no effect",
        tool.isDirectiveIncluded(directive, null));

    directive.setInDirectiveFile(DirectiveFileType.USER, null, true);
    assertFalse("user overrides scope", tool.isDirectiveIncluded(directive, null));

    settings.setInclude(DirectiveFileType.SYSTEM.getIndex(), true);
    directive.setInDirectiveFile(DirectiveFileType.SYSTEM, null, true);
    assertTrue("the file type of the editor overrides all other file types",
        tool.isDirectiveIncluded(directive, null));

    settings = new TestSettings();
    tool = new DirectiveTool(DirectiveFileType.USER, false, settings);
    descr = new TestDescr();
    descr.setEtomoColumn(DirectiveDescrEtomoColumn.SD);
    descr.setTemplate(true);
    directive = new Directive(descr);
    directive.setValue(10);
    assertTrue(
        "for the user editor, modified directives with SD are included when there is no matching file",
        tool.isDirectiveIncluded(directive, null));
  }

  public void testIsToogleDirectiveIncluded() {
    TestSettings settings = new TestSettings();
    DirectiveTool tool = new DirectiveTool(DirectiveFileType.SCOPE, false, settings);
    assertFalse("Null directive does not cause error",
        tool.isToggleDirectiveIncluded(null, null, true));

    settings.setIncludeSDChanged(true);
    TestDescr descr = new TestDescr();
    descr.setEtomoColumn(DirectiveDescrEtomoColumn.SD);
    descr.setTemplate(true);
    Directive directive = new Directive(descr);
    assertTrue("can return true when a template editor encounters a template directive",
        tool.isToggleDirectiveIncluded(directive, null, true));

    descr = new TestDescr();
    descr.setEtomoColumn(DirectiveDescrEtomoColumn.SD);
    descr.setBatch(true);
    directive = new Directive(descr);
    assertFalse("returns false when a template editor encounters a batch directive",
        tool.isToggleDirectiveIncluded(directive, null, true));

    tool = new DirectiveTool(DirectiveFileType.BATCH, false, settings);
    descr = new TestDescr();
    descr.setEtomoColumn(DirectiveDescrEtomoColumn.SD);
    descr.setBatch(true);
    directive = new Directive(descr);
    assertTrue("can return true when a batch editor encounters a batch directive",
        tool.isToggleDirectiveIncluded(directive, null, true));

    tool = new DirectiveTool(DirectiveFileType.BATCH, false, settings);
    descr = new TestDescr();
    descr.setEtomoColumn(DirectiveDescrEtomoColumn.SD);
    descr.setTemplate(true);
    directive = new Directive(descr);
    assertFalse("returns false when a batch editor encounters a template directive",
        tool.isToggleDirectiveIncluded(directive, null, true));

    settings.setIncludeSDChanged(false);
    tool = new DirectiveTool(DirectiveFileType.SCOPE, false, settings);
    descr = new TestDescr();
    directive = new Directive(descr);
    assertFalse("no reason to toggle",
        tool.isToggleDirectiveIncluded(directive, null, true));
    assertFalse("no reason to toggle",
        tool.isToggleDirectiveIncluded(directive, null, false));

    settings.setChangedIndex(DirectiveFileType.SYSTEM.getIndex());
    assertFalse("not in directive file",
        tool.isToggleDirectiveIncluded(directive, null, true));
    assertFalse("not in directive file",
        tool.isToggleDirectiveIncluded(directive, null, false));

    directive.setInDirectiveFile(DirectiveFileType.SYSTEM, null, true);
    assertFalse("exclude not set", tool.isToggleDirectiveIncluded(directive, null, true));
    assertFalse("include not set", tool.isToggleDirectiveIncluded(directive, null, false));

    settings.setInclude(DirectiveFileType.SYSTEM.getIndex(), true);
    assertFalse("no need to toggle",
        tool.isToggleDirectiveIncluded(directive, null, true));
    assertTrue("include is set - toggle to true",
        tool.isToggleDirectiveIncluded(directive, null, false));

    settings.setInclude(DirectiveFileType.SYSTEM.getIndex(), false);
    settings.setExclude(DirectiveFileType.SYSTEM.getIndex(), true);
    assertTrue("exclude is set - toggle to false",
        tool.isToggleDirectiveIncluded(directive, null, true));
    assertFalse("no need to toggle",
        tool.isToggleDirectiveIncluded(directive, null, false));

    settings = new TestSettings();
    tool = new DirectiveTool(DirectiveFileType.SYSTEM, false, settings);
    descr = new TestDescr();
    directive = new Directive(descr);
    settings.setIncludeSDChanged(true);
    assertFalse("SD in not in directive etomo column",
        tool.isToggleDirectiveIncluded(directive, null, true));
    assertFalse("SD in not in directive etomo column",
        tool.isToggleDirectiveIncluded(directive, null, false));

    descr = new TestDescr();
    descr.setEtomoColumn(DirectiveDescrEtomoColumn.SD);
    directive = new Directive(descr);
    assertTrue("include SD is false and curIncludeState is true - toggle",
        tool.isToggleDirectiveIncluded(directive, null, true));
    assertFalse("include SD and curIncludeState are both false - no need to toggle",
        tool.isToggleDirectiveIncluded(directive, null, false));

    settings.setIncludeSD(true);
    assertFalse("include SD and curIncludeState are both true - no need to toggle",
        tool.isToggleDirectiveIncluded(directive, null, true));
    assertTrue("include SD is true and curIncludeState is false - toggle",
        tool.isToggleDirectiveIncluded(directive, null, false));
  }

  public void testIsDirectiveVisible() {
    TestSettings settings = new TestSettings();
    DirectiveTool tool = new DirectiveTool(DirectiveFileType.SCOPE, false, settings);
    assertTrue("always returns true when is includedInGui is true",
        tool.isDirectiveVisible(null, true, false));
    assertTrue("always returns true when is changedInGui is true",
        tool.isDirectiveVisible(null, false, true));

    settings.setShowUnchanged(true);
    settings.setShowHidden(true);
    TestDescr descr = new TestDescr();
    descr.setBatch(true);
    Directive directive = new Directive(descr);
    assertFalse("returns false when a template editor encounters a batch directive",
        tool.isDirectiveVisible(directive, false, false));

    tool = new DirectiveTool(DirectiveFileType.BATCH, false, settings);
    descr = new TestDescr();
    descr.setTemplate(true);
    directive = new Directive(descr);
    assertFalse("returns false when a batch editor encounters a template directive",
        tool.isDirectiveVisible(directive, false, false));

    settings = new TestSettings();
    tool = new DirectiveTool(DirectiveFileType.SYSTEM, false, settings);
    descr = new TestDescr();
    descr.setEtomoColumn(DirectiveDescrEtomoColumn.SO);
    directive = new Directive(descr);
    directive.setValue(10);
    assertTrue("changed and not hidden - visible",
        tool.isDirectiveVisible(directive, false, false));

    settings.setShowUnchanged(true);
    descr = new TestDescr();
    descr.setEtomoColumn(DirectiveDescrEtomoColumn.SO);
    directive = new Directive(descr);
    assertTrue("not changed, but show unchanged is not (and not hidden) - visible",
        tool.isDirectiveVisible(directive, false, false));

    settings.setShowHidden(true);
    descr = new TestDescr();
    directive = new Directive(descr);
    directive.setValue(10);
    assertTrue("changed and hidden, but show hidden is on - visible",
        tool.isDirectiveVisible(directive, false, false));

    settings.setShowUnchanged(true);
    settings.setShowHidden(true);
    descr = new TestDescr();
    directive = new Directive(descr);
    assertTrue("unchanged and hidden, but show changed and show hidden are on - visible",
        tool.isDirectiveVisible(directive, false, false));

    settings.setShowUnchanged(false);
    settings.setShowHidden(false);
    descr = new TestDescr();
    descr.setEtomoColumn(DirectiveDescrEtomoColumn.NE);
    directive = new Directive(descr);
    assertFalse(
        "unchanged and hidden, and show changed and show hidden are off - not visible",
        tool.isDirectiveVisible(directive, false, false));
  }

  private static final class TestDescr implements DirectiveDescr {
    private DirectiveDescrEtomoColumn etomoColumn = null;
    private boolean template = false;
    private boolean batch = false;

    private TestDescr() {
    }

    public String getName() {
      return null;
    }

    public String getDescription() {
      return null;
    }

    public DirectiveValueType getValueType() {
      return null;
    }

    public void setBatch(final boolean input) {
      batch = input;
    }

    public boolean isBatch() {
      return batch;
    }

    public void setTemplate(final boolean input) {
      template = input;
    }

    public boolean isTemplate() {
      return template;
    }

    public void setEtomoColumn(final DirectiveDescrEtomoColumn input) {
      etomoColumn = input;
    }

    public DirectiveDescrEtomoColumn getEtomoColumn() {
      return etomoColumn;
    }
  }

  private static final class TestSettings implements DirectiveDisplaySettings {
    private final boolean[] include = new boolean[DirectiveFileType.NUM];
    private final boolean[] exclude = new boolean[DirectiveFileType.NUM];

    private boolean includeSD = false;
    private int changedIndex = -1;
    private boolean showUnchanged = false;
    private boolean showHidden = false;
    private boolean includeSDChanged = false;

    private TestSettings() {
      for (int i = 0; i < include.length; i++) {
        include[i] = false;
      }
    }

    public void setIncludeSD(final boolean input) {
      includeSD = input;
    }

    public boolean isIncludeSD() {
      return includeSD;
    }

    public void setInclude(final int index, final boolean input) {
      include[index] = input;
    }

    public boolean isInclude(final int index) {
      return include[index];
    }

    public void setExclude(final int index, final boolean input) {
      exclude[index] = input;
    }

    public boolean isExclude(final int index) {
      return exclude[index];
    }

    public void setShowUnchanged(final boolean input) {
      showUnchanged = input;
    }

    public boolean isShowUnchanged() {
      return showUnchanged;
    }

    public void setShowHidden(final boolean input) {
      showHidden = input;
    }

    public boolean isShowHidden() {
      return showHidden;
    }

    public void setIncludeSDChanged(final boolean input) {
      includeSDChanged = input;
    }

    public boolean isIncludeSDChanged() {
      return includeSDChanged;
    }

    public void setChangedIndex(final int input) {
      changedIndex = input;
    }

    public int getChangedIndex() {
      return changedIndex;
    }
  }
}
