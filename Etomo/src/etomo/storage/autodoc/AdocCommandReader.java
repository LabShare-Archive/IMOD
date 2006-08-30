package etomo.storage.autodoc;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.LinkedList;

import etomo.type.AxisID;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
public final class AdocCommandReader {
  public static final String rcsid = "$Id$";

  private final Autodoc autodoc;
  private final LinkedList locationStack = new LinkedList();
  private final String autodocName;
  private final String sectionType;

  private ReadOnlyNameValuePairList list = null;
  private SectionLocation sectionLoc = null;
  private NameValuePairLocation pairLoc = null;
  private boolean readingSections = false;
  private boolean readingCommands = false;
  private boolean verbose = false;
  private boolean done = false;
  private String name = null;
  private Autodoc functionAutodoc = null;
  private File functionLocationSourceDir = null;
  private AxisID axisID = null;
  private String info = null;
  private boolean function = false;

  public AdocCommandReader(Autodoc autodoc, String sectionType) {
    this.autodoc = autodoc;
    this.sectionType = sectionType;
    sectionLoc = autodoc.getSectionLocation(sectionType);
    list = autodoc;
    name = autodoc.getName();
    autodocName = name;
    functionAutodoc = autodoc;
    setInfo();
  }

  public void setFunctionLocationSourceDir(File functionLocationSourceDir) {
    this.functionLocationSourceDir = functionLocationSourceDir;
  }

  public void setAxisID(AxisID axisID) {
    this.axisID = axisID;
    setInfo();
  }

  public void setVerbose() {
    verbose = true;
  }

  public boolean isReadingSections() {
    return readingSections;
  }

  public boolean isReadingCommands() {
    return readingCommands;
  }

  public boolean isDone() {
    return done;
  }

  public void setDone() {
    done = true;
  }

  public String getName() {
    return name;
  }

  public void setInfo() {
    StringBuffer buffer = new StringBuffer();
    if (axisID != null) {
      buffer.append(axisID.toString() + ":");
    }
    if (name != null) {
      buffer.append(name.toString() + ":");
    }
    if (function) {
      if (functionAutodoc != null) {
        buffer.append(functionAutodoc.getName() + ":");
      }
      if (list != null) {
        buffer.append(list.getName() + ":");
      }
    }
    if (buffer.length() == 0) {
      info = AdocCommandReader.class.getName();
    }
    else {
      info = buffer.toString();
    }
  }

  public String getInfo() {
    return info;
  }

  /**
   * Gets the next command in the current section.  An adoc command will cause
   * this function to push the current autodoc and open the autodoc
   * specified in the adoc command.  If the function is unable to get a command,
   * it will attempt to pop to a previous autodoc.  If a null command is passed
   * to it, the function will create a new command and return it, otherwise it
   * will reuse the command parameter.
   * @param command
   * @return
   */
  public AdocCommand nextCommand(AdocCommand command, AdocCommandFactory factory) {
    readingCommands = true;
    //create command if it is null
    if (command == null) {
      if (factory == null) {
        throw new IllegalArgumentException(
            "command is null and factory is null");
      }
      command = factory.newAdocCommand();
    }
    command.reset();
    if (list == null) {
      return command;
    }
    //if starting a list, get the pair location
    if (pairLoc == null) {
      pairLoc = list.getNameValuePairLocation();
    }
    //get the pair
    NameValuePair pair = list.nextNameValuePair(pairLoc);
    if (pair == null) {
      //if this is the end of a function section, end the function and read the
      //next command from the calling autodoc
      if (function) {
        endFunction();
        return nextCommand(command, factory);
      }
      return command;
    }
    if (verbose) {
      System.err.println(axisID.toString() + ": " + pair.getString());
    }
    //place the pair in command
    command.set(pair);
    if (command.isFunctionLocation()) {
      //if the function location command (adoc) is found, change the function
      //autodoc and get the next command
      setFunctionAutodoc(command);
      return nextCommand(command, factory);
    }
    if (command.isFunction()) {
      //if the function command is found, get the function section and get the
      //first command from it
      callFunction(command);
      return nextCommand(command, factory);
    }
    return command;
  }

  public void nextSection() {
    if (sectionLoc == null) {
      throw new IllegalStateException(axisID
          + ": do not call nextSection on a secondary autodoc");
    }
    list = autodoc.nextSection(sectionLoc);
    setSection();
  }

  public void setSection(String sectionName) {
    if (sectionLoc == null) {
      throw new IllegalStateException(axisID
          + ": do not call setSection on a secondary autodoc");
    }
    list = autodoc.getSection(sectionType, sectionName);
    setSection();
  }

  private void setSection() {
    readingSections = true;
    pairLoc = null;
    name = null;
    readingCommands = false;
    if (list == null) {
      getInfo();
      done = true;
      if (verbose) {
        System.err.println(autodoc.getName() + ":  done with test.");
      }
      return;
    }
    name = list.getName();
    getInfo();
    if (verbose) {
      System.err.println(axisID.toString() + ":" + list.getString());
    }
  }

  /**
   * Pushes the current location onto the stack and gets a new location based
   * on the function call.
   * @param command
   */
  private void callFunction(AdocCommand command) {
    if (functionLocationSourceDir == null) {
      throw new IllegalStateException(
          "A secondary autodo source directory must be set when secondary autodocs are in use.");
    }
    if (!command.isFunction()) {
      throw new IllegalStateException("command=" + command);
    }
    //push old location
    Location location = new Location(list, sectionLoc, pairLoc, function);
    locationStack.addLast(location);
    //set values from the current autodoc
    list = functionAutodoc.getSection(command.getAction().toString(), command
        .getValue());
    if (list == null) {
      throw new IllegalStateException(functionAutodoc.getName()
          + ":missing section:" + command.toString());
    }
    sectionLoc = null;
    pairLoc = null;
    function = true;
    setInfo();
    if (verbose) {
      System.err.println(axisID.toString() + ":start:" + list.getString());
    }
  }

  private void setFunctionAutodoc(AdocCommand command) {
    String functionLocation = command.getValue();
    if (functionLocation == null) {
      functionAutodoc = autodoc;
    }
    else {
      try {
        functionAutodoc = Autodoc.getUITestAxisInstance_test(
            functionLocationSourceDir, command.getValue(), AxisID.ONLY);
      }
      catch (FileNotFoundException e) {
        e.printStackTrace();
        return;
      }
      catch (IOException e) {
        e.printStackTrace();
        return;
      }
    }
  }

  /**
   * the the top autodoc status off of the stack and makes it the current
   * autodoc
   */
  private void endFunction() {
    Location location = (Location) locationStack.removeLast();
    if (location == null) {
      list = null;
      return;
    }
    if (verbose) {
      System.err.println(axisID.toString() + ":end:" + list.getString());
    }
    list = location.getList();
    sectionLoc = location.getSectionLoc();
    pairLoc = location.getPairLoc();
    function = location.isFunction();
    setInfo();
  }

  private static final class Location {
    private final ReadOnlyNameValuePairList list;
    private final SectionLocation sectionLoc;
    private final NameValuePairLocation pairLoc;
    private final boolean function;

    Location(ReadOnlyNameValuePairList list, SectionLocation sectionLoc,
        NameValuePairLocation pairLoc, boolean function) {
      this.list = list;
      this.sectionLoc = sectionLoc;
      this.pairLoc = pairLoc;
      this.function = function;
    }

    ReadOnlyNameValuePairList getList() {
      return list;
    }

    SectionLocation getSectionLoc() {
      return sectionLoc;
    }

    NameValuePairLocation getPairLoc() {
      return pairLoc;
    }

    boolean isFunction() {
      return function;
    }

    public String toString() {
      return "list=" + list.getName() + ",\nsectionLoc=" + sectionLoc
          + ",\npairLoc=" + pairLoc;
    }
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.4  2006/08/18 23:14:01  sueh
 * <p> bug# 852 callFunction:  throw an exception when the function isn't found
 * <p>
 * <p> Revision 1.3  2006/08/08 17:45:57  sueh
 * <p> bug# 852 Removed the secondary autodoc functionality.  Added functionality to
 * <p> call a function section.  Added functionality to set the location of a function
 * <p> section.
 * <p>
 * <p> Revision 1.2  2006/05/18 20:52:19  sueh
 * <p> made compatible with java 1.4 (class getSimpleName doesn't exist in 1.4)
 * <p>
 * <p> Revision 1.1  2006/04/28 20:55:16  sueh
 * <p> bug# 787 Was UITestSection.  Reads name/value pairs sequentially in
 * <p> Autodoc files.  Can global or section name/value pairs.  Can jump to a
 * <p> section via section name.  Can only read from one section type.  Parses
 * <p> each name/value pair with an AdocCommand.
 * <p>
 * <p> Revision 1.1  2006/04/25 19:40:56  sueh
 * <p> bug# 787 Manages pairs, sections, and secondary autodocs.  Can get the
 * <p> next section or the next pair.  Automatically switches to and from
 * <p> secondary autodocs.
 * <p> </p>
 */
