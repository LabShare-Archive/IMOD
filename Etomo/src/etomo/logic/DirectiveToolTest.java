package etomo.logic;

import etomo.Arguments.DebugLevel;
import etomo.storage.Directive;
import etomo.storage.DirectiveDescr;
import etomo.storage.DirectiveDescrEtomoColumn;
import etomo.storage.DirectiveDescrFile;
import etomo.storage.DirectiveValueType;
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
        tool.isToggleDirectiveIncluded(null, false));

    settings.setInclude(DirectiveFileType.SCOPE.getIndex(), true);
    TestDescr descr = new TestDescr("setupset.copyarg.dual");
    descr.setTemplate(true);
    Directive directive = new Directive(descr);
    directive.setInDirectiveFile(DirectiveFileType.SCOPE, true);
    assertTrue("can return true when a template editor encounters a template directive",
        tool.isToggleDirectiveIncluded(directive, false));

    descr = new TestDescr("setupset.copyarg.dual");
    descr.setBatch(true);
    directive = new Directive(descr);
    directive.setInDirectiveFile(DirectiveFileType.SCOPE, true);
    assertFalse("returns false when a template editor encounters a batch directive",
        tool.isToggleDirectiveIncluded(directive, false));

    settings = new TestSettings();
    settings.setInclude(DirectiveFileType.BATCH.getIndex(), true);
    tool = new DirectiveTool(DirectiveFileType.BATCH, false, settings);
    descr = new TestDescr("setupset.copyarg.dual");
    descr.setBatch(true);
    directive = new Directive(descr);
    directive.setInDirectiveFile(DirectiveFileType.BATCH, true);
    assertTrue("can return true when a batch editor encounters a template directive",
        tool.isToggleDirectiveIncluded(directive, false));

    descr = new TestDescr("setupset.copyarg.dual");
    descr.setTemplate(true);
    directive = new Directive(descr);
    directive.setInDirectiveFile(DirectiveFileType.BATCH, true);
    assertFalse("returns false when a batch editor encounters a template directive",
        tool.isToggleDirectiveIncluded(directive, false));
    tool = new DirectiveTool(DirectiveFileType.SCOPE, false, settings);
    descr = new TestDescr("setupset.copyarg.dual");
    directive = new Directive(descr);
    assertFalse("include SD is on, but directive doesn't have SD in etomo column",
        tool.isToggleDirectiveIncluded(directive, false));

    tool = new DirectiveTool(null, false, settings);
    descr = new TestDescr("setupset.copyarg.dual");
    directive = new Directive(descr);
    assertFalse("missing type doesn't cause an error",
        tool.isToggleDirectiveIncluded(directive, false));

    tool = new DirectiveTool(DirectiveFileType.SYSTEM, true, settings);
    directive.setInDirectiveFile(DirectiveFileType.SCOPE, true);
    assertFalse("In file but include isn't set",
        tool.isToggleDirectiveIncluded(directive, false));

    settings.setInclude(DirectiveFileType.SCOPE.getIndex(), true);
    assertTrue("In file and include is set",
        tool.isToggleDirectiveIncluded(directive, false));

    settings.setExclude(DirectiveFileType.USER.getIndex(), true);
    assertTrue("directive isn't in the user file, so exclude has no effect",
        tool.isToggleDirectiveIncluded(directive, false));

    directive.setInDirectiveFile(DirectiveFileType.USER, true);
    assertFalse("user overrides scope", tool.isToggleDirectiveIncluded(directive, false));

    settings.setInclude(DirectiveFileType.SYSTEM.getIndex(), true);
    directive.setInDirectiveFile(DirectiveFileType.SYSTEM, true);
    assertTrue("the file type of the editor overrides all other file types",
        tool.isToggleDirectiveIncluded(directive, false));

    settings = new TestSettings();
    tool = new DirectiveTool(DirectiveFileType.USER, false, settings);
    descr = new TestDescr("setupset.copyarg.dual");
    descr.setEtomoColumn(DirectiveDescrEtomoColumn.SD);
    descr.setTemplate(true);
    directive = new Directive(descr);
    directive.setValue(10);
    assertTrue(
        "for the user editor, modified directives with SD are included when there is no matching file",
        tool.isToggleDirectiveIncluded(directive, false));
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
    TestDescr descr = new TestDescr("runtime.Preprocessing.any.removeXrays");
    descr.setBatch(true);
    Directive directive = new Directive(descr);
    assertFalse("returns false when a template editor encounters a batch directive",
        tool.isDirectiveVisible(directive, false, false));

    tool = new DirectiveTool(DirectiveFileType.BATCH, false, settings);
    descr = new TestDescr("runtime.Preprocessing.any.removeXrays");
    descr.setTemplate(true);
    directive = new Directive(descr);
    assertFalse("returns false when a batch editor encounters a template directive",
        tool.isDirectiveVisible(directive, false, false));

    settings = new TestSettings();
    tool = new DirectiveTool(DirectiveFileType.SYSTEM, false, settings);
    descr = new TestDescr("runtime.Preprocessing.any.removeXrays");
    descr.setEtomoColumn(DirectiveDescrEtomoColumn.SO);
    directive = new Directive(descr);
    directive.setValue(10);
    assertTrue("changed and not hidden - visible",
        tool.isDirectiveVisible(directive, false, false));

    settings.setShowUnchanged(true);
    descr = new TestDescr("runtime.Preprocessing.any.removeXrays");
    descr.setEtomoColumn(DirectiveDescrEtomoColumn.SO);
    directive = new Directive(descr);
    assertTrue("not changed, but show unchanged is not (and not hidden) - visible",
        tool.isDirectiveVisible(directive, false, false));

    settings.setShowHidden(true);
    descr = new TestDescr("runtime.Preprocessing.any.removeXrays");
    directive = new Directive(descr);
    directive.setValue(10);
    assertTrue("changed and hidden, but show hidden is on - visible",
        tool.isDirectiveVisible(directive, false, false));

    settings.setShowUnchanged(true);
    settings.setShowHidden(true);
    descr = new TestDescr("runtime.Preprocessing.any.removeXrays");
    directive = new Directive(descr);
    assertTrue("unchanged and hidden, but show changed and show hidden are on - visible",
        tool.isDirectiveVisible(directive, false, false));

    settings.setShowUnchanged(false);
    settings.setShowHidden(false);
    descr = new TestDescr("runtime.Preprocessing.any.removeXrays");
    descr.setEtomoColumn(DirectiveDescrEtomoColumn.NE);
    directive = new Directive(descr);
    assertFalse(
        "unchanged and hidden, and show changed and show hidden are off - not visible",
        tool.isDirectiveVisible(directive, false, false));
  }

  private static final class TestDescr implements DirectiveDescr {
    private final String name;

    private DirectiveDescrEtomoColumn etomoColumn = null;
    private boolean template = false;
    private boolean batch = false;

    private TestDescr(final String name) {
      this.name = name;
    }

    public String getName() {
      return name;
    }

    public String getDescription() {
      return null;
    }

    public String getLabel() {
      return null;
    }

    public DirectiveDescrFile.ChoiceList getChoiceList(final DebugLevel debug) {
      return null;
    }

    public DirectiveValueType getValueType() {
      return DirectiveValueType.UNKNOWN;
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

    public boolean isShowOnlyIncluded() {
      return false;
    }

    public void setChangedIndex(final int input) {
      changedIndex = input;
    }

    public int getChangedIndex() {
      return changedIndex;
    }
  }
}
