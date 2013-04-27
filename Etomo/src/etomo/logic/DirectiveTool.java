package etomo.logic;

import etomo.storage.Directive;
import etomo.storage.DirectiveDescrEtomoColumn;
import etomo.type.AxisID;
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

  private boolean debug = false;

  public DirectiveTool(final DirectiveFileType type, final boolean fileTypeExists,
      final DirectiveDisplaySettings displaySettings) {
    this.displaySettings = displaySettings;
    this.type = type;
    this.fileTypeExists = fileTypeExists;
  }

  void setDebug(final boolean input) {
    debug = input;
  }

  /**
   * For initialization.  Returns true if the directive's include checkbox should be
   * checked.  Include and exclude checkboxes are checked in order of precedence, and only
   * have an effect on directives with have a relationship to them.  So for the include SD
   * checkbox to have an effect, the directive must have an "SD" in its Etomo column (see
   * directives.csv).  For the other include checkboxes, and the exclude checkboxes, to
   * have an effect, the directive must be in a saved directive file of the same type in
   * the dataset directory.
   * @param directive
   * @param axisID
   * @return
   */
  public boolean isDirectiveIncluded(final Directive directive, final AxisID axisID) {
    if (directive == null) {
      return false;
    }
    if (!isMatchesType(directive)) {
      return false;
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
      // Override the lower precedence settings.
      if (i != matchingIndex && directive.isInDirectiveFile(i, axisID)) {
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
    if (directive.isInDirectiveFile(matchingIndex, axisID)) {
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
      return true;
    }
    // If none of the display settings are in effect and the file of the same type as the
    // editor is not in the dataset directory, return true for directives that match this
    // fall-back.
    if (!exclude && !fileTypeExists) {
      return type == DirectiveFileType.USER
          && directive.getEtomoColumn() == DirectiveDescrEtomoColumn.SD
          && isMatchesType(directive) && directive.getValues().isChanged(axisID);
    }
    return false;
  }

  /**
   * Not for initialization.  This function should be called each time a display setting
   * is changed.  It will return true if the directive's include checkbox state should be
   * switched.  If the change is that an include or exclude directive file checkbox has
   * been turned off, this function will return false.
   * @param directive
   * @param axisID
   * @param curIncludeState - selection state of the directive's include checkbox
   * @return true if state of the directive should be toggled
   */
  public boolean isToggleDirectiveIncluded(final Directive directive,
      final AxisID axisID, final boolean curIncludeState) {
    if (directive == null) {
      return false;
    }
    if (!isMatchesType(directive)) {
      return false;
    }
    int index = displaySettings.getChangedIndex();
    // Has one of the directive file include/exclude checkboxes has changed?
    if (index != -1) {
      // Is this directive affected by this change? Does the change mean the
      // curIncludeState is wrong now?
      return directive.isInDirectiveFile(index, axisID)
          && (!curIncludeState && displaySettings.isInclude(index))
          || (curIncludeState && displaySettings.isExclude(index));
    }
    return false;
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
    DirectiveDescrEtomoColumn etomoColumn = directive.getEtomoColumn();
    return (displaySettings.isShowUnchanged() || directive.getValues().isChanged())
        && (displaySettings.isShowHidden() || (etomoColumn != null && etomoColumn != DirectiveDescrEtomoColumn.NE));
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