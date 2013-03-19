package etomo;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import etomo.storage.ComFile;
import etomo.storage.Directive;
import etomo.storage.DirectiveEditorSection;
import etomo.storage.DirectiveMap;
import etomo.storage.DirectiveName;
import etomo.storage.DirectiveType;
import etomo.storage.DirectivesDescriptionFile;
import etomo.storage.LogFile;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyStatement;
import etomo.storage.autodoc.ReadOnlyStatementList;
import etomo.storage.autodoc.Statement;
import etomo.storage.autodoc.StatementLocation;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.DirectiveFileType;
import etomo.ui.swing.DirectiveEditor;
import etomo.ui.swing.UIHarness;

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

  private final DirectiveMap paramMap = new DirectiveMap(true);
  private final List<DirectiveEditorSection> sectionArray = new ArrayList<DirectiveEditorSection>();
  private final DirectiveMap setupMap = new DirectiveMap();

  private final AxisID axisID;
  private final ApplicationManager manager;
  private final DirectiveFileType type;

  private DirectiveEditorSection otherSection = null;

  public DirectiveEditorBuilder(final ApplicationManager manager, final AxisID axisID,
      DirectiveFileType type) {
    this.axisID = axisID;
    this.manager = manager;
    this.type = type;
  }

  /**
   * Builds a list of directives.  Gets the directive names and descriptions from
   * directives.csv and the local directive file matching the type member variable.  Gets
   * the values and default values from ApplicationManager and the .com and origcoms/.com
   * files.
   */
  public DirectiveEditor build() {
    StringBuffer errmsg = new StringBuffer();
    Directive directive = null;
    // Load the directives from directives.csv.
    DirectivesDescriptionFile.Iterator descrIterator = DirectivesDescriptionFile.INSTANCE
        .getIterator(manager, axisID);
    if (descrIterator != null) {
      DirectiveEditorSection section = null;
      while (descrIterator.hasNext()) {
        descrIterator.next();
        // Get the section - directives following this section are associated with it.
        if (descrIterator.isSection()) {
          section = new DirectiveEditorSection(descrIterator.getSectionHeader());
          sectionArray.add(section);
        }
        else {
          // Get the directive
          directive = new Directive(descrIterator.getDirectiveDescription());
          // Store the directive under current section
          if (section == null) {
            // This shouldn't happen because all directives in directives.csv are listed
            // under a header.
            if (otherSection == null) {
              otherSection = new DirectiveEditorSection(SECTION_OTHER_HEADER);
            }
            section = otherSection;
          }
          section.add(directive);
          // Store the directive in a map
          if (directive.equals(DirectiveType.SETUP_SET)) {
            setupMap.put(directive.getName(), directive);
          }
          else {
            paramMap.put(directive.getName(), directive);
          }
        }
      }
    }
    // Update directives from the matching template. Directives that where not defined in
    // directives.csv will be loaded here.
    File matchingTemplateFile = null;
    if (type != null
        && (matchingTemplateFile = type.getLocalFile(manager, axisID)) != null) {
      try {
        ReadOnlyStatementList statementList = AutodocFactory.getInstance(manager,
            matchingTemplateFile);
        StatementLocation location = statementList.getStatementLocation();
        ReadOnlyStatement statement = null;
        while ((statement = statementList.nextStatement(location)) != null) {
          if (statement.getType() == Statement.Type.NAME_VALUE_PAIR) {
            String name = statement.getLeftSide();
            if (DirectiveType.SETUP_SET.equals(statement.getLeftSide(0))) {
              directive = setupMap.get(name);
            }
            else {
              directive = paramMap.get(name);
            }
            if (directive == null) {
              // Handle undefined directives. They can only be comparam directives.
              directive = new Directive(name);
              if (directive.equals(DirectiveType.COM_PARAM)) {
                if (otherSection == null) {
                  otherSection = new DirectiveEditorSection(SECTION_OTHER_HEADER);
                }
                otherSection.add(directive.getName());
                paramMap.put(directive.getName(), directive);
              }
              else {
                System.err.println("Warning: Invalid directive in "
                    + matchingTemplateFile.getName() + ": " + directive.getName() + ".");
              }
            }
            directive.setMatchingTemplate(true);
          }
        }
      }
      catch (IOException e) {
        e.printStackTrace();
        errmsg.append("Unable to load " + matchingTemplateFile.getName() + ".  "
            + e.getMessage() + "  ");
      }
      catch (LogFile.LockException e) {
        errmsg.append("Unable to load " + matchingTemplateFile.getName() + ".  "
            + e.getMessage() + "  ");
      }
    }
    // update setupset and runtime
    manager.updateDirectiveMaps(setupMap, paramMap, type, errmsg);
    // update paramMap from *.com and origcoms/*.com
    // Get a sorted list of comparam directive names
    DirectiveMap.Iterator iterator = paramMap.keySet(DirectiveType.COM_PARAM).iterator();
    boolean dualAxis = manager.getBaseMetaData().getAxisType() == AxisType.DUAL_AXIS;
    AxisID firstAxisID;
    if (dualAxis) {
      firstAxisID = AxisID.FIRST;
    }
    else {
      firstAxisID = AxisID.ONLY;
    }
    // A axis
    AxisID curAxisID = firstAxisID;
    ComFile comFileA = new ComFile(manager, curAxisID);
    Map<String, String> commandMapA = null;
    ComFile comFileADefaults = new ComFile(manager, curAxisID, "origcoms");
    Map<String, String> commandMapADefaults = null;
    // B axis
    ComFile comFileB = null;
    Map<String, String> commandMapB = null;
    ComFile comFileBDefaults = null;
    Map<String, String> commandMapBDefaults = null;
    if (dualAxis) {
      curAxisID = AxisID.SECOND;
      comFileB = new ComFile(manager, curAxisID);
      comFileBDefaults = new ComFile(manager, curAxisID, "origcoms");
    }
    curAxisID = firstAxisID;
    DirectiveName directiveName = new DirectiveName();
    while (iterator.hasNext()) {
      directiveName.set(iterator.next());
      // save axis A value
      commandMapA = getCommandMap(directiveName, comFileA, curAxisID, commandMapA, errmsg);
      if (commandMapA != null) {
        setDirectiveValue(commandMapA, paramMap, directiveName, curAxisID, false);
      }
      // save axis A default value
      commandMapADefaults = getCommandMap(directiveName, comFileADefaults, curAxisID,
          commandMapADefaults, errmsg);
      if (commandMapADefaults != null) {
        setDirectiveValue(commandMapADefaults, paramMap, directiveName, curAxisID, false);
      }
      if (dualAxis) {
        curAxisID = AxisID.SECOND;
        // save axis B value
        commandMapB = getCommandMap(directiveName, comFileB, curAxisID, commandMapB,
            errmsg);
        if (commandMapB != null) {
          setDirectiveValue(commandMapB, paramMap, directiveName, curAxisID, false);
        }
        // save axis B default value
        commandMapBDefaults = getCommandMap(directiveName, comFileBDefaults, curAxisID,
            commandMapBDefaults, errmsg);
        if (commandMapBDefaults != null) {
          setDirectiveValue(commandMapBDefaults, paramMap, directiveName, curAxisID,
              false);
        }
      }
      curAxisID = firstAxisID;
    }
    if (errmsg.length() > 0) {
      UIHarness.INSTANCE.openMessageDialog(manager, errmsg.toString(),
          "Problems Building Editor", axisID);
    }
    return DirectiveEditor.getInstance(manager, axisID, sectionArray, setupMap, paramMap);
  }

  /**
   * Uses the directiveName as a guide and decides whether to open a com file or create a
   * new commandMap.  If neither of these things are necessary, returns the existing
   * commandMap.
   * @param directiveName
   * @param comFile
   * @param axisID
   * @param commandMap
   * @return
   */
  private Map<String, String> getCommandMap(final DirectiveName directiveName,
      final ComFile comFile, final AxisID axisID, Map<String, String> commandMap,
      final StringBuffer errmsg) {
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
    }
    return commandMap;
  }

  /**
   * Using directiveName as the key, gets a value from commandMap and gets a directive
   * from paramMap.  Places the value in the directive.
   * @param commandMap - list of values
   * @param paramMap - list of directives - must not be null
   * @param directiveName - key
   * @param axisID - the value belongs to a specific axis
   * @param isDefaultValue - the value is a default value
   */
  private void setDirectiveValue(final Map<String, String> commandMap,
      final DirectiveMap paramMap, final DirectiveName directiveName,
      final AxisID axisID, final boolean isDefaultValue) {
    if (commandMap == null) {
      return;
    }
    // Pull out the value and default value from the program commands.
    if (commandMap.containsKey(directiveName.getParameterName())) {
      Directive directive = paramMap.get(directiveName.get());
      // Set value in the directive
      String value = commandMap.get(directiveName.getParameterName());
      if (value != null) {
        if (!isDefaultValue) {
          directive.setValue(axisID, value);
        }
        else {
          directive.setDefaultValue(axisID, value);
        }
      }
      else {
        // no value - treat as a boolean
        if (!isDefaultValue) {
          directive.setValue(axisID, true);
        }
        else {
          directive.setDefaultValue(axisID, true);
        }
      }
    }
  }
}
