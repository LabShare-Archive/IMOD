package etomo.ui;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.LinkedList;

import etomo.storage.autodoc.Autodoc;
import etomo.storage.autodoc.NameValuePair;
import etomo.storage.autodoc.NameValuePairLocation;
import etomo.storage.autodoc.ReadOnlyNameValuePairList;
import etomo.storage.autodoc.SectionLocation;
import etomo.type.AxisID;
import etomo.type.DialogType;
import etomo.type.UITestAction;

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
final class UITestSection {
  public static final String rcsid = "$Id$";

  private final Autodoc autodoc;
  private final AxisID axisID;
  private final File autodocSourceDir;
  private final LinkedList autodocStatusStack = new LinkedList();

  private ReadOnlyNameValuePairList list = null;
  private SectionLocation sectionLoc = null;
  private NameValuePairLocation pairLoc = null;
  private boolean started = false;
  private boolean startedList = false;
  private boolean verbose = false;
  private boolean done = false;
  private String name = null;
  private DialogType dialogType = null;

  UITestSection(AxisID axisID, File autodocSourceDir, Autodoc autodoc) {
    this.autodoc = autodoc;
    this.axisID = axisID;
    this.autodocSourceDir = autodocSourceDir;
    sectionLoc = autodoc
        .getSectionLocation(UITestConstants.DIALOG_SECTION_TYPE);
  }

  void setVerbose() {
    verbose = true;
  }

  boolean isStarted() {
    return started;
  }

  boolean isStartedList() {
    return startedList;
  }

  boolean isDone() {
    return done;
  }

  String getName() {
    return name;
  }
  
  String getInfo() {
    return axisID + ":" + name;
  }

  DialogType getDialogType() {
    return dialogType;
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
  UITestCommand nextCommand(UITestCommand command) {
    startedList = true;
    //create command if it is null
    if (command == null) {
      command = new UITestCommand();
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
        return nextCommand(command);
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
    if (command.getAction() == UITestAction.ADOC) {
      pushAutodoc(command);
      return nextCommand(command);
    }
    return command;
  }

  void next() {
    if (sectionLoc == null) {
      throw new IllegalStateException(axisID
          + ": do not call next on a secondary autodoc");
    }
    started = true;
    list = autodoc.nextSection(sectionLoc);
    pairLoc = null;
    name = null;
    startedList = false;
    dialogType = null;
    if (list == null) {
      done = true;
      if (verbose) {
        System.err.println(autodoc.getName() + ":  done with test.");
      }
      return;
    }
    name = list.getName();
    dialogType = DialogType.getInstance(name);
    if (verbose) {
      System.err.println(axisID.toString() + ":" + list.getString());
    }
  }

  /**
   * Pushes the current autodoc status onto the stack and gets a new autodoc based
   * on the command.
   * @param command
   */
  private void pushAutodoc(UITestCommand command) {
    //try to get the new autodoc
    Autodoc secondaryAutodoc;
    try {
      secondaryAutodoc = Autodoc.getUITestAxisInstance_test(autodocSourceDir,
          command.getValue(), AxisID.ONLY);
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
  }

  private final class AutodocStatus {
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
      return "list=" + list.getName() + ",\nsectionLoc=" + sectionLoc + ",\npairLoc="
          + pairLoc;
    }
  }
}
/**
 * <p> $Log$ </p>
 */