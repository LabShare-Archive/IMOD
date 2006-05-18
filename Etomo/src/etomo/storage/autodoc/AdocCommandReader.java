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
  private final LinkedList autodocStatusStack = new LinkedList();
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
  private String secondaryAutodocName = null;
  private File secondaryAutodocSourceDir = null;
  private AxisID axisID = null;
  private String info = null;

  public AdocCommandReader(Autodoc autodoc, String sectionType) {
    this.autodoc = autodoc;
    this.sectionType = sectionType;
    sectionLoc = autodoc.getSectionLocation(sectionType);
    list = autodoc;
    name = autodoc.getName();
    autodocName = name;
    secondaryAutodocName = name;
    setInfo();
  }

  public void setSecondaryAutodocSourceDir(File secondaryAutodocSourceDir) {
    this.secondaryAutodocSourceDir = secondaryAutodocSourceDir;
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
    if (secondaryAutodocName != null) {
      buffer.append(secondaryAutodocName.toString() + ":");
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
      //if this is the end of a secondary autodoc, pop it and read the next
      //command in the preceding autodoc
      if (sectionLoc == null) {
        popAutodoc();
        return nextCommand(command, factory);
      }
      return command;
    }
    if (verbose) {
      System.err.println(axisID.toString() + ": " + pair.getString());
    }
    //place the pair in command
    command.set(pair);
    //if the adoc command is found, get the secondary autodoc and get the command
    //from it
    if (command.isSecondaryAutodoc()) {
      pushAutodoc(command);
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
   * Pushes the current autodoc status onto the stack and gets a new autodoc based
   * on the command.
   * @param command
   */
  private void pushAutodoc(AdocCommand command) {
    if (secondaryAutodocSourceDir == null) {
      throw new IllegalStateException(
          "A secondary autodo source directory must be set when secondary autodocs are in use.");
    }
    if (!command.isSecondaryAutodoc()) {
      throw new IllegalStateException("command=" + command);
    }
    //try to get the new autodoc
    Autodoc secondaryAutodoc;
    try {
      secondaryAutodoc = Autodoc.getUITestAxisInstance_test(
          secondaryAutodocSourceDir, command.getValue(), AxisID.ONLY);
    }
    catch (FileNotFoundException e) {
      e.printStackTrace();
      return;
    }
    catch (IOException e) {
      e.printStackTrace();
      return;
    }
    //push autodoc status
    AutodocStatus autodocStatus = new AutodocStatus(list, sectionLoc, pairLoc);
    autodocStatusStack.addLast(autodocStatus);
    //set values from the new autodoc
    list = secondaryAutodoc;
    sectionLoc = null;
    pairLoc = null;
    secondaryAutodocName = list.getName();
    getInfo();
  }

  /**
   * the the top autodoc status off of the stack and makes it the current
   * autodoc
   */
  private void popAutodoc() {
    AutodocStatus autodocStatus = (AutodocStatus) autodocStatusStack
        .removeLast();
    if (autodocStatus == null) {
      list = null;
      return;
    }
    list = autodocStatus.getList();
    sectionLoc = autodocStatus.getSectionLoc();
    pairLoc = autodocStatus.getPairLoc();
    if (sectionLoc == null) {
      secondaryAutodocName = list.getName();
    }
    else {
      secondaryAutodocName = autodocName;
    }
    getInfo();
  }

  private static final class AutodocStatus {
    private final ReadOnlyNameValuePairList list;
    private final SectionLocation sectionLoc;
    private final NameValuePairLocation pairLoc;

    AutodocStatus(ReadOnlyNameValuePairList list, SectionLocation sectionLoc,
        NameValuePairLocation pairLoc) {
      this.list = list;
      this.sectionLoc = sectionLoc;
      this.pairLoc = pairLoc;
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

    public String toString() {
      return "list=" + list.getName() + ",\nsectionLoc=" + sectionLoc
          + ",\npairLoc=" + pairLoc;
    }
  }
}
/**
 * <p> $Log$
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
