package etomo.logic;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.storage.ComFile;
import etomo.storage.Directive;
import etomo.storage.DirectiveDescrFile;
import etomo.storage.DirectiveDescrSection;
import etomo.storage.DirectiveMap;
import etomo.storage.DirectiveName;
import etomo.storage.DirectiveType;
import etomo.storage.LogFile;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyStatement;
import etomo.storage.autodoc.ReadOnlyStatementList;
import etomo.storage.autodoc.Statement;
import etomo.storage.autodoc.StatementLocation;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.DirectiveFileType;
import etomo.type.UserConfiguration;

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
public final class DirectiveEditorBuilder {
  public static final String rcsid = "$Id:$";

  private static final String SECTION_OTHER_HEADER = "Other Directives";
  private static final AxisID AXID_ID = AxisID.ONLY;
  private static final String SPECIAL_CASE_PROGRAM_NAME = "ctfplotter";

  private final DirectiveMap directiveMap = new DirectiveMap();
  private final List<DirectiveDescrSection> sectionArray = new ArrayList<DirectiveDescrSection>();
  private boolean[] fileTypeExists = new boolean[DirectiveFileType.NUM];
  private final List<String> droppedDirectives = new ArrayList<String>();

  private final BaseManager manager;
  private final DirectiveFileType type;

  private boolean debug = false;
  private DirectiveDescrSection otherSection = null;

  public DirectiveEditorBuilder(final BaseManager manager, final DirectiveFileType type) {
    this.manager = manager;
    this.type = type;
  }

  /**
   * Builds a list of directives.  Gets the directive names and descriptions from
   * directives.csv and the local directive file matching the type member variable.  Gets
   * the values and default values from ApplicationManager and the .com and origcoms/.com
   * files.
   * @param errmsg - may be null
   * @return errmsg
   */
  public StringBuffer build(final AxisType sourceAxisType, StringBuffer errmsg) {
    // reset
    directiveMap.clear();
    sectionArray.clear();
    for (int i = 0; i < DirectiveFileType.NUM; i++) {
      fileTypeExists[i] = false;
    }
    otherSection = null;
    // Load and update directives, and load sections.
    if (errmsg == null) {
      errmsg = new StringBuffer();
    }
    Directive directive = null;
    // Load the directives from directives.csv.
    DirectiveDescrFile.Iterator descrIterator = DirectiveDescrFile.INSTANCE.getIterator(
        manager, AXID_ID);
    if (descrIterator != null) {
      // Skip title and column header
      descrIterator.hasNext();
      descrIterator.hasNext();
      DirectiveDescrSection section = null;
      int dCount = 0;
      boolean matchesType = false;
      while (descrIterator.hasNext()) {
        descrIterator.next();
        // Get the section - directives following this section are associated with it.
        if (descrIterator.isSection()) {
          if (section != null) {
            // Record whether any of the directives in the section match the directive
            // file type. Directives that don't match the type cannot be edited in this
            // editor.
            section.setContainsEditableDirectives(matchesType);
            if (dCount == 0) {
              sectionArray.remove(section);
            }
          }
          section = new DirectiveDescrSection(descrIterator.getSectionHeader());
          matchesType = false;
          dCount = 0;
          sectionArray.add(section);
        }
        else if (descrIterator.isDirective()) {
          // Get the directive
          directive = new Directive(descrIterator.getDirectiveDescription());
          // Only save valid directives.
          if (directive.isValid()) {
            // Check to see if this directive matches the type
            if (!matchesType
                && (type != DirectiveFileType.BATCH && (directive.isTemplate() || !directive
                    .isBatch()))
                || (type == DirectiveFileType.BATCH && (!directive.isTemplate() || directive
                    .isBatch()))) {
              matchesType = true;
            }
            dCount++;
            // Store the directive in the map
            directiveMap.put(directive.getKey(), directive);
            // Store the directive under current section - directives are always stored
            // under sections, so section should not be null.
            if (section != null) {
              section.add(directive);
            }
            else {
              System.err.println("Error: directive " + directive.getTitle()
                  + ", not inside a section in the directives.csv file.  It cannot be "
                  + "edited.");
            }
          }
        }
      }
      DirectiveDescrFile.INSTANCE.releaseIterator(descrIterator);
    }
    // Add information and undocumented directives from the current directive files.
    updateFromLocalDirectiveFile(DirectiveFileType.SCOPE, errmsg);
    updateFromLocalDirectiveFile(DirectiveFileType.SYSTEM, errmsg);
    updateFromLocalDirectiveFile(DirectiveFileType.USER, errmsg);
    updateFromLocalDirectiveFile(DirectiveFileType.BATCH, errmsg);
    // update setupset and runtime
    manager.updateDirectiveMap(directiveMap, errmsg);
    // update paramMap from *.com and origcoms/*.com
    // Get a sorted list of comparam directive names
    DirectiveMap.Iterator iterator = directiveMap.keySet(DirectiveType.COM_PARAM)
        .iterator();
    AxisID firstAxisID;
    if (sourceAxisType == AxisType.DUAL_AXIS) {
      firstAxisID = AxisID.FIRST;
    }
    else {
      firstAxisID = AxisID.ONLY;
    }
    // A or only axis
    ComFile comFile = new ComFile(manager, firstAxisID);
    Map<String, String> commandMap = null;
    ComFile comFileDefaults = new ComFile(manager, firstAxisID, "origcoms");
    Map<String, String> commandMapDefaults = null;
    DirectiveName directiveName = new DirectiveName();
    while (iterator.hasNext()) {
      directiveName.setKey(iterator.next());
      // save values
      commandMap = getCommandMap(directiveName, comFile, commandMap, errmsg, false);
      if (commandMap != null) {
        setDirectiveValue(commandMap, directiveName, false);
      }
      // save default values
      commandMapDefaults = getCommandMap(directiveName, comFileDefaults,
          commandMapDefaults, errmsg, true);
      if (commandMapDefaults != null) {
        setDirectiveValue(commandMapDefaults, directiveName, true);
      }
    }
    return errmsg;
  }

  /**
   * Updates existing directives and loads ones that are not in directive.csv
   * @param type
   * @param errmsg for errors - must not be null
   * @return true if the local directive file exists
   */
  private boolean updateFromLocalDirectiveFile(final DirectiveFileType type,
      final StringBuffer errmsg) {
    Directive directive = null;
    File directiveFile = null;
    if (type != null && (directiveFile = type.getLocalFile(manager, AXID_ID)) != null
        && directiveFile.exists()) {
      fileTypeExists[type.getIndex()] = true;
      try {
        ReadOnlyStatementList statementList = AutodocFactory.getInstance(manager,
            directiveFile);
        if (statementList != null) {
          StatementLocation location = statementList.getStatementLocation();
          ReadOnlyStatement statement = null;
          DirectiveName directiveName = new DirectiveName();
          while ((statement = statementList.nextStatement(location)) != null) {
            if (statement.getType() == Statement.Type.NAME_VALUE_PAIR) {
              String leftSide = statement.getLeftSide();
              AxisID axisID = directiveName.setKey(leftSide);
              directive = directiveMap.get(directiveName.getKey());
              if (directive == null) {
                // Handle undefined directives.
                directive = new Directive(directiveName);
                // Ignoring B axis values and directives.
                if (axisID == AxisID.SECOND) {
                  break;
                }
                // Only comparam directives are used generically by batchruntomo, so
                // undefined ones may be useful.
                if (directive.isValid() && directive.getType() == DirectiveType.COM_PARAM) {
                  // If otherSection hasn't been created, create it and add it to
                  // sectionArray.
                  if (otherSection == null) {
                    otherSection = new DirectiveDescrSection(SECTION_OTHER_HEADER);
                    sectionArray.add(otherSection);
                  }
                  otherSection.add(directive.getKey());
                  directiveMap.put(directive.getKey(), directive);
                }
                else {
                  directive = null;
                  // Save directives that don't go into the editor.
                  droppedDirectives.add(leftSide);
                }
              }
              if (directive != null) {
                directive.setInDirectiveFile(type, true);
              }
            }
          }
          return true;
        }
      }
      catch (IOException e) {
        e.printStackTrace();
        errmsg.append("Unable to load " + directiveFile.getName() + ".  "
            + e.getMessage() + "  ");
      }
      catch (LogFile.LockException e) {
        errmsg.append("Unable to load " + directiveFile.getName() + ".  "
            + e.getMessage() + "  ");
      }
    }
    return false;
  }

  /**
   * Uses the directiveName as a guide and decides whether to open a com file or create a
   * new commandMap.  If neither of these things are necessary, returns the existing
   * commandMap.
   * @param directiveName
   * @param comFile
   * @param commandMap
   * @return
   */
  private Map<String, String> getCommandMap(final DirectiveName directiveName,
      final ComFile comFile, Map<String, String> commandMap, final StringBuffer errmsg,
      final boolean origcoms) {

    // Get a new comfile and origcoms comfile each time the comfile name changes in the
    // sorted list of comparam directives.
    String comFileName = directiveName.getComFileName();
    if (!comFile.equalsComFileName(comFileName)) {
      comFile.setComFileName(comFileName);
    }
    // Get a new program from the comfile and origcoms comfile each time the program
    // name or comfile name changes in the sorted list of comparam directives.
    String programName = directiveName.getProgramName();
    if (!comFile.equalsProgramName(programName)) {
      commandMap = comFile.getCommandMap(programName, errmsg);
      //Handled special case setupset directives.
      if (origcoms && comFile.equalsComFileName(SPECIAL_CASE_PROGRAM_NAME)
          && comFile.equalsProgramName(SPECIAL_CASE_PROGRAM_NAME)) {
        setSpecialCaseDefaultValues(commandMap);
      }
    }
    return commandMap;
  }

  /**
   * Handles cases where a setupset directive has a default value that is stored in a
   * .com file.
   * @param commandMap
   * @param errmsg
   */
  private void setSpecialCaseDefaultValues(Map<String, String> commandMap) {
    DirectiveName fromDirectiveName = new DirectiveName();
    DirectiveName toDirectiveName = new DirectiveName();
    String fromPrefix = "comparam.";
    String toPrefix = "setupset.copyarg.";
    fromDirectiveName.setKey(fromPrefix + SPECIAL_CASE_PROGRAM_NAME + "."
        + SPECIAL_CASE_PROGRAM_NAME + ".ExpectedDefocus");
    toDirectiveName.setKey(toPrefix + "defocus");
    setDefaultDirectiveValue(commandMap, fromDirectiveName, toDirectiveName);
    fromDirectiveName.setKey(fromPrefix + SPECIAL_CASE_PROGRAM_NAME + "."
        + SPECIAL_CASE_PROGRAM_NAME + ".Voltage");
    toDirectiveName.setKey(toPrefix + "voltage");
    setDefaultDirectiveValue(commandMap, fromDirectiveName, toDirectiveName);
    fromDirectiveName.setKey(fromPrefix + SPECIAL_CASE_PROGRAM_NAME + "."
        + SPECIAL_CASE_PROGRAM_NAME + ".SphericalAberration");
    toDirectiveName.setKey(toPrefix + "Cs");
    setDefaultDirectiveValue(commandMap, fromDirectiveName, toDirectiveName);
  }

  /**
   * Using fromDirectiveName as a key, gets a value from commandMap.  Using
   * toDirectiveName as a key, gets a directive from directiveMap.  Places the value in
   * the default value of the directive.
   * @param commandMap - list of values
   * @param fromDirectiveName - key to commandMap
   * @param toDirectiveName - key to DirectiveMap
   * @param axisID - the value belongs to a specific axis
   */
  private void setDefaultDirectiveValue(final Map<String, String> commandMap,
      final DirectiveName fromDirectiveName, final DirectiveName toDirectiveName) {
    if (commandMap == null) {
      return;
    }
    // Pull out the default value from the command map, using the "from" directive name.
    if (commandMap.containsKey(fromDirectiveName.getParameterName())) {
      Directive toDirective = directiveMap.get(toDirectiveName.getKey());
      // Set the command map value in the "to" directive
      String value = commandMap.get(fromDirectiveName.getParameterName());
      if (value != null) {
        toDirective.setDefaultValue(value);
      }
      else {
        // no value - treat as a boolean
        toDirective.setDefaultValue(true);
      }
    }
  }

  /**
   * Using directiveName as the key, gets a value from commandMap and gets a directive
   * from directiveMap.  Places the value in the directive.
   * @param commandMap - list of values
   * @param directiveName - key
   * @param axisID - the value belongs to a specific axis
   * @param isDefaultValue - the value is a default value
   */
  private void setDirectiveValue(final Map<String, String> commandMap,
      final DirectiveName directiveName, final boolean isDefaultValue) {
    if (commandMap == null) {
      return;
    }
    // Pull out the value or default value from the program command.
    if (commandMap.containsKey(directiveName.getParameterName())) {
      Directive directive = directiveMap.get(directiveName.getKey());
      // Set value in the directive
      String value = commandMap.get(directiveName.getParameterName());
      if (value != null) {
        if (!isDefaultValue) {
          directive.setValue(value);
        }
        else {
          directive.setDefaultValue(value);
        }
      }
      else {
        // no value - treat as a boolean
        if (!isDefaultValue) {
          directive.setValue(true);
        }
        else {
          directive.setDefaultValue(true);
        }
      }
    }
  }

  public List<DirectiveDescrSection> getSectionArray() {
    return sectionArray;
  }

  public boolean[] getFileTypeExists() {
    return fileTypeExists;
  }

  public List<String> getDroppedDirectives() {
    return droppedDirectives;
  }

  public File getDefaultSaveLocation() {
    File dir = null;
    if (type == DirectiveFileType.USER) {
      UserConfiguration userConfig = EtomoDirector.INSTANCE.getUserConfiguration();
      if (userConfig.isUserTemplateDirSet()) {
        dir = new File(userConfig.getUserTemplateDir());
      }
      else {
        dir = ConfigTool.getDefaultUserTemplateDir();
      }
    }
    if (dir != null) {
      return dir;
    }
    return new File(manager.getPropertyUserDir());
  }

  public DirectiveMap getDirectiveMap() {
    return directiveMap;
  }
}
