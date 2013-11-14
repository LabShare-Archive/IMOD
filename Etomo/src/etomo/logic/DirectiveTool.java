package etomo.logic;

import etomo.Arguments.DebugLevel;
import etomo.EtomoDirector;
import etomo.storage.Directive;
import etomo.storage.DirectiveDescrEtomoColumn;
import etomo.storage.DirectiveValues;
import etomo.type.DirectiveFileType;
import etomo.ui.DirectiveDisplaySettings;

/**
* <p>Description: Handles the display complexities for the directive file editor.</p>
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
public final class DirectiveTool {
  public static final String rcsid = "$Id:$";

  private final DirectiveFileType type;
  private final boolean fileTypeExists;
  private final DirectiveDisplaySettings displaySettings;

  private DebugLevel debug = EtomoDirector.INSTANCE.getArguments().getDebugLevel();

  public DirectiveTool(final DirectiveFileType type, final boolean fileTypeExists,
      final DirectiveDisplaySettings displaySettings) {
    this.displaySettings = displaySettings;
    this.type = type;
    this.fileTypeExists = fileTypeExists;
  }

  public void setDebug(final DebugLevel input) {
    debug = input;
  }

  public void resetDebug() {
    debug = EtomoDirector.INSTANCE.getArguments().getDebugLevel();
  }

  /**
   * Returns true if the include checkbox value needs to be changed.  Always returns false
   * for "Any" directives (param or runtime directives with an axisID that is null),
   * because their setting comes from the A and B directives.
   * @param directive
   * @param axisID
   * @param includeChecked
   * @return
   */
  public boolean isToggleDirectiveIncluded(final Directive directive,
      final boolean includeChecked) {
    if (directive == null) {
      return includeChecked != false;
    }
    if (!isMatchesType(directive)) {
      return includeChecked != false;
    }
    // Check the display settings in order of precedence.
    boolean include = false;
    boolean exclude = false;
    // if the directive is in a current directive file then the include/exclude settings
    // can affect it.
    int matchingIndex = -1;
    if (type != null) {
      matchingIndex = type.getIndex();
    }
    // Check all but the settings for the file type that matches the type of the editor.
    for (int i = 0; i < DirectiveFileType.NUM; i++) {
      // Override the lower precedence settings. If the directive, or the any axis version
      // of the directive is in the file, then it may have an effect.
      if (i != matchingIndex && directive.isInDirectiveFile(i)) {
        // Include and exclude cannot be set at the same time (but they can both be off).
        if (displaySettings.isInclude(i)) {
          include = true;
          exclude = false;
        }
        else if (displaySettings.isExclude(i)) {
          include = false;
          exclude = true;
        }
      }
    }
    // the directive file matching the type of the editor has the highest precedence.
    if (directive.isInDirectiveFile(matchingIndex)) {
      if (displaySettings.isInclude(matchingIndex)) {
        include = true;
        exclude = false;
      }
      else if (displaySettings.isExclude(matchingIndex)) {
        include = false;
        exclude = true;
      }
    }
    if (include) {
      return includeChecked != true;
    }
    if (exclude) {
      return includeChecked != false;
    }
    if (!fileTypeExists) {
      DirectiveValues values = directive.getValues();
      return includeChecked != (type == DirectiveFileType.USER
          && directive.getEtomoColumn() == DirectiveDescrEtomoColumn.SD
          && isMatchesType(directive) && (values.isChanged()));
    }
    return includeChecked != false;
  }

  /**
   * Used whenever necessary to show or hide directives.  Call this after
   * isDirectiveIncluded or isToggleDirectiveIncluded is called.  This function will keep
   * everything where include is checked visible.  It will also keep anything that the
   * user has changed visible.
   * @param directive
   * @param includedInGui
   * @param changedInGui
   * @return
   */
  public boolean isDirectiveVisible(final Directive directive,
      final boolean includedInGui, final boolean changedInGui) {
    if (displaySettings.isShowOnlyIncluded()) {
      return includedInGui;
    }
    // Included and changed directives should always be visible
    if (includedInGui || changedInGui) {
      return true;
    }
    // Hide batch-only directives in a template editor. Hide template-only directives in a
    // batch file editor.
    if (!isMatchesType(directive)) {
      return false;
    }
    // Hide unchanged and hidden directives, unless the display settings say otherwise.
    // Batch directives and undefined directives are never considered hidden.
    DirectiveDescrEtomoColumn etomoColumn = directive.getEtomoColumn();
    directive.setDebug(debug);
    boolean retval = (displaySettings.isShowUnchanged() || directive.getValues()
        .isChanged())
        && (displaySettings.isShowHidden()
            || (etomoColumn != null && etomoColumn != DirectiveDescrEtomoColumn.NE)
            || type == DirectiveFileType.BATCH || directive.getDescription() == null);
    directive.resetDebug();
    return retval;
  }

  /**
   * Distinguishes between batch directives and template directives.
   * @param directive
   * @return
   */
  private boolean isMatchesType(final Directive directive) {
    boolean template = directive.isTemplate();
    boolean batch = directive.isBatch();
    return (!template && !batch) || (batch && type == DirectiveFileType.BATCH)
        || (template && type != DirectiveFileType.BATCH);
  }
}