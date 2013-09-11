package etomo.ui.swing;

import java.awt.Component;
import java.awt.Container;
import java.awt.HeadlessException;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.util.ArrayList;

import javax.swing.Icon;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JOptionPane;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.process.ProcessMessages;
import etomo.storage.autodoc.AutodocTokenizer;
import etomo.type.AxisID;
import etomo.type.BaseMetaData;
import etomo.type.FrameType;
import etomo.type.UITestActionType;
import etomo.type.UITestSubjectType;
import etomo.type.UserConfiguration;
import etomo.ui.UIComponent;
import etomo.util.Utilities;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2010</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 *   
 */
abstract class AbstractFrame extends JFrame implements UIComponent, SwingComponent {
  public static final String rcsid = "$Id$";

  private static final int MAX_MESSAGE_LINES = 20;
  private static final int MESSAGE_WIDTH = 60;
  private static final boolean PRINT_NAMES = EtomoDirector.INSTANCE.getArguments()
      .isPrintNames();
  private static final String OK = "OK";
  private static final String ETOMO_QUESTION = "Etomo question";
  private static final String YES = "Yes";
  private static final String NO = "No";
  private static final String CANCEL = "Cancel";
  private static final String[] YES_NO_LABEL_ARRAY = new String[] { YES, NO };
  private static final int NO_INDEX = 1;
  private static final String[] OK_LABEL_ARRAY = new String[] { OK };

  private boolean verbose = false;

  abstract void menuFileAction(ActionEvent actionEvent);

  abstract void menuToolsAction(ActionEvent actionEvent);

  abstract void menuViewAction(ActionEvent actionEvent);

  abstract void menuOptionsAction(ActionEvent actionEvent);

  abstract void menuHelpAction(ActionEvent actionEvent);

  abstract FrameType getFrameType();

  abstract void cancel();

  abstract void save(AxisID axisID);

  abstract void saveAs();

  abstract void close();

  public SwingComponent getUIComponent() {
    return this;
  }

  public Component getComponent() {
    return this;
  }

  public void setVisible(boolean visible) {
    if (visible) {
      UserConfiguration userConfiguration = EtomoDirector.INSTANCE.getUserConfiguration();
      if (!EtomoDirector.INSTANCE.getArguments().isIgnoreLoc()
          && userConfiguration.isLastLocationSet(getFrameType())) {
        setLocation(userConfiguration.getLastLocationX(getFrameType()),
            userConfiguration.getLastLocationY(getFrameType()));
      }
    }
    super.setVisible(visible);
  }

  final void setVerbose(boolean verbose) {
    this.verbose = verbose;
  }

  AxisID getAxisID() {
    return AxisID.ONLY;
  }

  void pack(boolean force) {
    if (!force && !EtomoDirector.INSTANCE.getUserConfiguration().isAutoFit()) {
      setVisible(true);
    }
    else {
      Rectangle bounds = getBounds();
      bounds.height++;
      bounds.width++;
      setBounds(bounds);
      try {
        super.pack();
      }
      catch (NullPointerException e) {
        e.printStackTrace();
      }
    }
  }

  void repaint(AxisID axisID) {
    repaint();
  }

  void pack(AxisID axisID) {
    pack();
  }

  void pack(AxisID axisID, boolean force) {
    pack(force);
  }

  void repaintWindow() {
    repaintContainer(this);
    this.repaint();
  }

  private void repaintContainer(Container container) {
    Component[] comps = container.getComponents();
    for (int i = 0; i < comps.length; i++) {
      if (comps[i] instanceof Container) {
        Container cont = (Container) comps[i];
        repaintContainer(cont);
      }
      comps[i].repaint();
    }
  }

  void menuFileMRUListAction(ActionEvent event) {
  }

  /**
   * Open a message dialog
   * @param message
   * @param title
   */
  void displayMessage(BaseManager manager, String message, String title, AxisID axisID) {
    openMessageDialog(manager, axisID, message, title);
  }

  void displayMessage(final BaseManager manager, final Component parentcComponent,
      final String message, final String title, final AxisID axisID) {
    openMessageDialog(manager, parentcComponent, axisID, message, title);
  }

  void displayWarningMessage(final BaseManager manager, final Component parentcComponent,
      final String message, final String title, final AxisID axisID) {
    openWarningMessageDialog(manager, parentcComponent, axisID, message, title);
  }

  void displayMessage(final BaseManager manager, final Component parentcComponent,
      final String[] message, final String title, final AxisID axisID) {
    openMessageDialog(manager, parentcComponent, axisID, message, title);
  }

  /**
   * Open a message dialog
   * @param message
   * @param title
   */
  void displayMessage(BaseManager manager, String message, String title) {
    openMessageDialog(manager, AxisID.ONLY, message, title);
  }

  void displayInfoMessage(BaseManager manager, String message, String title, AxisID axisID) {
    openInfoMessageDialog(manager, axisID, message, title);
  }

  int displayYesNoCancelMessage(BaseManager manager, String message, AxisID axisID) {
    return openYesNoCancelDialog(manager, axisID, message);
  }

  boolean displayYesNoMessage(BaseManager manager, String[] message, AxisID axisID) {
    return openYesNoDialog(manager, axisID, message);
  }

  boolean displayYesNoMessage(BaseManager manager, String message, AxisID axisID) {
    return openYesNoDialog(manager, axisID, message);
  }

  boolean openYesNoDialogWithDefaultNo(final BaseManager manager, final String message,
      final String title, final AxisID axisID) {
    return openYesNoDialog(manager, axisID, message, title, NO_INDEX, true);
  }

  boolean displayDeleteMessage(BaseManager manager, String message[], AxisID axisID) {
    return openDeleteDialog(manager, axisID, message);
  }

  /**
   * Open a message dialog
   * @param message
   * @param title
   */
  void displayMessage(BaseManager manager, String[] message, String title, AxisID axisID) {
    openMessageDialog(manager, axisID, message, title);
  }

  void displayErrorMessage(BaseManager manager, ProcessMessages processMessages,
      String title, AxisID axisID) {
    openErrorMessageDialog(manager, axisID, processMessages, title);
  }

  boolean displayYesNoWarningDialog(BaseManager manager, String message, AxisID axisID) {
    return openYesNoWarningDialog(manager, axisID, message);
  }

  void displayWarningMessage(BaseManager manager, ProcessMessages processMessages,
      String title, AxisID axisID) {
    openWarningMessageDialog(manager, axisID, processMessages, title);
  }

  /**
   * Open a message dialog with a wrapped message with the dataset appended.
   * @param message
   * @param title
   */
  void openMessageDialog(BaseManager manager, AxisID axisID, String message, String title) {
    showOptionPane(manager, axisID, wrap(manager, message), title,
        JOptionPane.ERROR_MESSAGE);
  }

  void openMessageDialog(final BaseManager manager, final Component parentcComponent,
      final AxisID axisID, final String message, final String title) {
    showOptionPane(manager, parentcComponent, axisID, wrap(manager, message), title,
        JOptionPane.ERROR_MESSAGE);
  }

  void openWarningMessageDialog(final BaseManager manager,
      final Component parentcComponent, final AxisID axisID, final String message,
      final String title) {
    showOptionPane(manager, parentcComponent, axisID, wrap(manager, message), title,
        JOptionPane.WARNING_MESSAGE);
  }

  void openMessageDialog(final BaseManager manager, final Component parentcComponent,
      final AxisID axisID, final String[] message, final String title) {
    showOptionPane(manager, parentcComponent, axisID, wrap(manager, message), title,
        JOptionPane.ERROR_MESSAGE);
  }

  void openWarningMessageDialog(BaseManager manager, AxisID axisID,
      ProcessMessages processMessages, String title) {
    showOptionPane(manager, axisID, wrapWarning(manager, processMessages), title,
        JOptionPane.ERROR_MESSAGE);
  }

  void openErrorMessageDialog(BaseManager manager, AxisID axisID,
      ProcessMessages processMessages, String title) {
    showOptionPane(manager, axisID, wrapError(manager, processMessages), title,
        JOptionPane.ERROR_MESSAGE);
  }

  boolean openYesNoWarningDialog(BaseManager manager, AxisID axisID, String message) {
    int result = showOptionPane(manager, axisID, wrap(manager, message), "Etomo Warning",
        JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE,
        YES_NO_LABEL_ARRAY[NO_INDEX], false, YES_NO_LABEL_ARRAY);
    return result == 0;
  }

  /**
   * Open a Yes or No question dialog
   * @param message
   * @return
   */
  boolean openYesNoDialog(BaseManager manager, AxisID axisID, String message) {
    int result = showOptionConfirmPane(manager, axisID, wrap(manager, message),
        ETOMO_QUESTION, JOptionPane.YES_NO_OPTION, YES_NO_LABEL_ARRAY);
    return result == JOptionPane.YES_OPTION;
  }

  /**
   * Open a Yes or No question dialog.  Control which option is the default.
   * @param manager
   * @param axisID
   * @param message
   * @param title
   * @return
   */
  boolean openYesNoDialog(final BaseManager manager, final AxisID axisID,
      final String message, final String title, final int initialValueIndex,
      final boolean overrideDefaultLabels) {
    int result = showOptionConfirmPane(manager, axisID, wrap(manager, message), title,
        JOptionPane.YES_NO_OPTION, YES_NO_LABEL_ARRAY[initialValueIndex],
        overrideDefaultLabels, YES_NO_LABEL_ARRAY);
    return result == JOptionPane.YES_OPTION;
  }

  /**
   * Open a Yes or No question dialog
   * @param message
   * @return
   */
  boolean openDeleteDialog(final BaseManager manager, final AxisID axisID,
      final String[] message) {
    int result = showOptionPane(manager, axisID, wrap(manager, message), "Delete File?",
        JOptionPane.DEFAULT_OPTION, JOptionPane.QUESTION_MESSAGE, null, true,
        new String[] { "Delete", NO });
    return result == 0;
  }

  /**
   * Open a Yes or No question dialog
   * @param message
   * @return
   */
  boolean openYesNoDialog(BaseManager manager, AxisID axisID, String[] message) {
    int result = showOptionConfirmPane(manager, axisID, wrap(manager, message),
        ETOMO_QUESTION, JOptionPane.YES_NO_OPTION, YES_NO_LABEL_ARRAY);
    return result == JOptionPane.YES_OPTION;
  }

  void openInfoMessageDialog(BaseManager manager, AxisID axisID, String message,
      String title) {
    showOptionPane(manager, axisID, wrap(manager, message), title,
        JOptionPane.INFORMATION_MESSAGE);
  }

  /**
   * Open a message dialog
   * @param message
   * @param title
   */
  void openMessageDialog(BaseManager manager, AxisID axisID, String[] message,
      String title) {
    showOptionPane(manager, axisID, wrap(manager, message), title,
        JOptionPane.ERROR_MESSAGE);
  }

  /**
   * Open a Yes, No or Cancel question dialog
   * @param message
   * @return int state of the users select
   */
  int openYesNoCancelDialog(BaseManager manager, AxisID axisID, String message) {
    return showOptionConfirmPane(manager, axisID, wrap(manager, message), ETOMO_QUESTION,
        JOptionPane.YES_NO_CANCEL_OPTION, new String[] { YES, NO, CANCEL });
  }

  private int showOptionConfirmPane(BaseManager manager, AxisID axisID, String[] message,
      String title, int optionType, String[] optionStrings) {
    return showOptionPane(manager, axisID, message, title, optionType,
        JOptionPane.QUESTION_MESSAGE, null, false, optionStrings);
  }

  private int showOptionConfirmPane(final BaseManager manager, final AxisID axisID,
      final String[] message, final String title, final int optionType,
      final String initialValue, final boolean overrideDefaultLabels,
      final String[] optionStrings) {
    return showOptionPane(manager, axisID, message, title, optionType,
        JOptionPane.QUESTION_MESSAGE, initialValue, overrideDefaultLabels, optionStrings);
  }

  private final String[] wrapWarning(BaseManager manager, ProcessMessages processMessages) {
    ArrayList messageArray = setupMessageArray(manager);
    for (int i = 0; i < processMessages.warningListSize(); i++) {
      messageArray = wrap(processMessages.getWarning(i), messageArray);
    }
    return toStringArray(messageArray);
  }

  /**
   * Add the current dataset name to the message and wrap
   * @param message
   * @return
   */
  private final String[] wrapError(BaseManager manager, ProcessMessages processMessages) {
    ArrayList messageArray = setupMessageArray(manager);
    for (int i = 0; i < processMessages.errorListSize(); i++) {
      messageArray = wrap(processMessages.getError(i), messageArray);
    }
    return toStringArray(messageArray);
  }

  /**
   * Add the current dataset name to the message and wrap
   * @param message
   * @return
   */
  private String[] wrap(BaseManager manager, String message) {
    ArrayList messageArray = setupMessageArray(manager);
    messageArray = wrap(message, messageArray);
    return toStringArray(messageArray);
  }

  /**
   * Add the current dataset name to the message and wrap
   * @param message
   * @return
   */
  private String[] wrap(BaseManager manager, String[] message) {
    ArrayList messageArray = setupMessageArray(manager);
    for (int i = 0; i < message.length; i++) {
      messageArray = wrap(message[i], messageArray);
    }
    return toStringArray(messageArray);
  }

  private final String[] toStringArray(ArrayList arrayList) {
    if (arrayList.size() == 1) {
      String[] returnArray = { (String) arrayList.get(0) };
      return returnArray;
    }
    return (String[]) arrayList.toArray(new String[arrayList.size()]);
  }

  private final ArrayList setupMessageArray(BaseManager manager) {
    ArrayList messageArray = new ArrayList();
    if (manager != null) {
      BaseMetaData metaData = manager.getBaseMetaData();
      if (metaData != null) {
        messageArray.add(manager.getName() + ":");
      }
    }
    return messageArray;
  }

  /**
   * wrap the message and place it in messageArray
   * @param messagePiece
   * @param messageArray
   * @return messageArray
   */
  private ArrayList wrap(String messagePiece, ArrayList messageArray) {
    if (messagePiece == null) {
      if (messageArray.size() == 0) {
        messageArray.add(" ");
      }
      return messageArray;
    }
    if (messagePiece.equals("\n")) {
      messageArray.add(" ");
      return messageArray;
    }
    // first - break up the message piece by line
    String[] messagePieceArray = messagePiece.split("\n");
    // second - break up each line by maximum length
    for (int i = 0; i < messagePieceArray.length; i++) {
      // handle empty lines
      if (messagePieceArray[i] == null || messagePieceArray[i].length() == 0) {
        messageArray.add(" ");
      }
      else {
        int messageLength = messagePieceArray[i].length();
        int messageIndex = 0;
        while (messageIndex < messageLength && messageArray.size() < MAX_MESSAGE_LINES) {
          int endIndex = Math.min(messageLength, messageIndex + MESSAGE_WIDTH);
          StringBuffer newLine = new StringBuffer(messagePieceArray[i].substring(
              messageIndex, endIndex));
          // overflowing line - look for whitespace or a comma
          messageIndex = endIndex;
          char lastChar = ' ';
          while (messageIndex < messageLength
              && messagePieceArray[i].substring(messageIndex, messageIndex + 1).matches(
                  "\\S+") && lastChar != ',') {
            lastChar = messagePieceArray[i].charAt(messageIndex++);
            newLine.append(lastChar);
          }
          messageArray.add(newLine.toString());
        }
      }
    }
    return messageArray;
  }

  private void showOptionPane(BaseManager manager, AxisID axisID, String[] message,
      String title, int messageType) {
    showOptionPane(manager, axisID, message, title, JOptionPane.DEFAULT_OPTION,
        messageType, null, false, OK_LABEL_ARRAY);
  }

  private void showOptionPane(final BaseManager manager,
      final Component parentcComponent, final AxisID axisID, final String[] message,
      final String title, final int messageType) {
    showOptionPane(manager, parentcComponent, axisID, message, title,
        JOptionPane.DEFAULT_OPTION, messageType, null, false, OK_LABEL_ARRAY);
  }

  private int showOptionPane(final BaseManager manager, final AxisID axisID,
      final String[] message, final String title, final int optionType,
      final int messageType, final Object initialValue,
      final boolean overrideDefaultLabels, final String[] optionLabels) {
    int result = showOptionDialog(manager, axisID, this, message, title, optionType,
        messageType, null, initialValue, overrideDefaultLabels, optionLabels);
    return result;
  }

  private int showOptionPane(final BaseManager manager, final Component parentComponent,
      final AxisID axisID, final String[] message, final String title,
      final int optionType, final int messageType, final Object initialValue,
      final boolean overrideDefaultLabels, final String[] optionStrings) {
    int result = showOptionDialog(manager, axisID, parentComponent, message, title,
        optionType, messageType, null, initialValue, overrideDefaultLabels, optionStrings);
    return result;
  }

  /**
   * Shows all pop up message dialogs.  Pass in in BaseManager so that the
   * @param uiComponent
   * @param message
   * @param title
   * @param optionType
   * @param messageType
   * @param icon
   * @param initialValue
   * @param overrideDefaultLabels
   * @param optionLabels
   * @param manager
   * @return
   * @throws HeadlessException
   */
  private int showOptionDialog(final BaseManager manager, final AxisID axisID,
      final Component parentComponent, final String[] message, final String title,
      final int optionType, final int messageType, final Icon icon,
      final Object initialValue, final boolean overrideDefaultLabels,
      final String[] optionLabels) throws HeadlessException {
    if (manager != null) {
      manager.logMessage(message, title, axisID);
    }
    else {
      System.err.println(Utilities.getDateTimeStamp() + "\n" + title + " - " + axisID
          + " axis:");
      for (int i = 0; i < message.length; i++) {
        System.err.println(message[i]);
      }
    }
    JOptionPane pane = null;
    // Decide whether to pass an array of button labels (to override the defaults) or
    // null.
    if (overrideDefaultLabels) {
      pane = new JOptionPane(message, messageType, optionType, icon, optionLabels,
          initialValue);
    }
    else {
      pane = new JOptionPane(message, messageType, optionType, icon, null, initialValue);
    }

    pane.setInitialValue(initialValue);
    pane.setComponentOrientation(((parentComponent == null) ? JOptionPane.getRootFrame()
        : parentComponent).getComponentOrientation());

    JDialog dialog = pane.createDialog(parentComponent, title);

    pane.selectInitialValue();
    String name = Utilities.convertLabelToName(title);
    pane.setName(name);
    printName(name, optionLabels, title, message);
    dialog.setVisible(true);
    dialog.dispose();

    Object selectedValue = pane.getValue();

    if (selectedValue == null)
      return JOptionPane.CLOSED_OPTION;
    // If a null array of options was passed, pane returns an integer.
    if (!overrideDefaultLabels || optionLabels == null) {
      if (selectedValue instanceof Integer) {
        return ((Integer) selectedValue).intValue();
      }
      return JOptionPane.CLOSED_OPTION;
    }
    // If an array of options was passed, pane returns the label of the button selected.
    for (int counter = 0, maxCounter = optionLabels.length; counter < maxCounter; counter++) {
      if (optionLabels[counter].equals(selectedValue)) {
        return counter;
      }
    }
    return JOptionPane.CLOSED_OPTION;
  }

  private synchronized final void printName(String name, String[] optionStrings,
      String title, String[] message) {
    if (PRINT_NAMES) {
      // print waitfor popup name/value pair
      StringBuffer buffer = new StringBuffer(UITestActionType.WAIT.toString()
          + AutodocTokenizer.SEPARATOR_CHAR + UITestSubjectType.POPUP.toString()
          + AutodocTokenizer.SEPARATOR_CHAR + name + ' '
          + AutodocTokenizer.DEFAULT_DELIMITER + ' ');
      // if there are options, then print a popup name/value pair
      if (optionStrings != null && optionStrings.length > 0) {
        buffer.append(optionStrings[0]);
        for (int i = 1; i < optionStrings.length; i++) {
          buffer.append(',' + optionStrings[i]);
        }
        System.out.println(buffer);
      }
    }
    if (verbose) {
      // if verbose then print the popup title and message
      System.err.println("Popup:");
      System.err.println(title);
      if (message != null) {
        for (int i = 0; i < message.length; i++) {
          System.err.println(message[i]);
        }
      }
    }
  }
}
