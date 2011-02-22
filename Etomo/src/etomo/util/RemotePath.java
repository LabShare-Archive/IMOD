package etomo.util;

import java.io.File;
import java.io.IOException;
import java.util.Vector;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.storage.CpuAdoc;
import etomo.storage.LogFile;
import etomo.storage.Network;
import etomo.storage.Node;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.AutodocTokenizer;
import etomo.storage.autodoc.ReadOnlyAttribute;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.storage.autodoc.ReadOnlySection;
import etomo.type.AxisID;
import etomo.type.EtomoAutodoc;

/**
 * <p>Description: A singleton class which loads mount rules from the cpu.adoc
 * file and uses them to translate local paths into paths which can be used on
 * remote computers which share a file system with the current host computer.
 * Currently this class only works with Linux and MacIntosh.
 * 
 * Example:
 * 
 * mountrule.1.local = /private/var/automount/home
 * mountrule.1.remote = /home
 * 
 * Run getRemotePath():
 * 
 * Input:  local path = /private/var/automount/home/georgeorr
 * Return:  remote path = /home/georgeorr
 * 
 * 
 * Mountname:
 * 
 * The %mountname tag can be placed in a remote mount rule and is converted into
 * a computer-specific mount name.  The mount name can come from either the
 * Computer section name for the current host computer or the value of a
 * mountname attribute under that section.  The mountname attribute is necessary
 * when the Computer section name, which is used to connect to the computer, is
 * different from a shared directory name specific to the computer.
 * 
 * Examples (the current host is frodo):
 * 
 * Getting the mount name from the Computer section:
 * 
 * mountrule.2.local = /localscratch
 * mountrule.2.remote = /scratch/%mountname
 * 
 * [Computer = frodo]
 * 
 * Run getRemotePath():
 * 
 * Input:  local path = /localscratch
 * Return:  remote path = /scratch/frodo
 * 
 * 
 * Getting the mount name from the mountname under the Computer section:
 * 
 * mountrule.2.local = /localscratch
 * mountrule.2.remote = /scratch/%mountname
 * 
 * [Computer = frodo.colorado.edu]
 * mountname = frodo
 * 
 * Run getRemotePath():
 * 
 * Input:  local path = /localscratch
 * Return:  remote path = /scratch/frodo
 * 
 * 
 * Mount rules must start with a "/".  They represent absolute directory paths.
 * However mount rules do not have to end at the end of a directory name.  This
 * allows a single rule to be used in a wider variety of cases.
 * 
 * Example:

 * mountrule.1.local = /localscratch
 * mountrule.1.remote = /scratch/%mountname
 * 
 * mountrule.2.local = /private/var/automount/home
 * mountrule.2.remote = /home
 * 
 * [Computer = frodo]
 * 
 * Run getRemotePath():
 * 
 * Input:  local path = /localscratch2
 * Return:  remote path = /scratch/frodo2
 * 
 * Input:  local path = /private/var/automount/home2/georgeorr
 * Return:  remote path = /home2/georgeorr
 * 
 * 
 * The order of mount rules and mount rules in sections:
 * 
 * Mount rules can be placed in both the global attribute area and under the
 * Computer sections.  The order in which the mount rules are tested is based on
 * the number contained in each rule, not the order of the rules in the file.
 * Mount rules should start from 1 in the global attribute area and in each
 * section.  The section rules for the current host computer are tested first.
 * Then the global rules are tested.  The first match is used, so the mount
 * rules should be ordered from most specific to least specific.
 * 
 * Examples:
 * 
 * Correct order:
 * 
 * mountrule.1.local = /private/var/automount/home
 * mountrule.1.remote = /home
 * 
 * mountrule.2.local = /private/var/automount
 * mountrule.2.remote = /scratch
 * 
 * Run getRemotePath():
 * 
 * Input:  local path = /private/var/automount/home/georgeorr
 * Return:  remote path = /home/georgeorr
 * 
 * Input:  local path = /private/var/automount/georgeorr
 * Return:  remote path = /scratch/georgeorr
 * 
 * 
 * Correct order with sections:
 * 
 * mountrule.1.local = /private/var/automount
 * mountrule.1.remote = /scratch
 * 
 * [Computer = frodo]
 * mountrule.1.local = /private/var/automount/home
 * mountrule.1.remote = /home
 *  
 * Run getRemotePath():
 * 
 * Input:  local path = /private/var/automount/home/georgeorr
 * Return:  remote path = /home/georgeorr
 * 
 * Input:  local path = /private/var/automount/georgeorr
 * Return:  remote path = /scratch/georgeorr
 * 
 * 
 * Incorrect order:
 * 
 * mountrule.1.local = /private/var/automount
 * mountrule.1.remote = /scratch
 * 
 * mountrule.2.local = /private/var/automount/home
 * mountrule.2.remote = /home
 * 
 * Run getRemotePath():
 * 
 * Input:  local path = /private/var/automount/home/georgeorr
 * Return:  remote path = /scratch/home/georgeorr
 * 
 * Input:  local path = /private/var/automount/georgeorr
 * Return:  remote path = /scratch/georgeorr
 * 
 * The mount rule 2 is never used.
 * 
 * 
 * Overriding a global mount rule:
 * 
 * A global mount rule can be overridden for the current host computer by
 * placing a local rule with an identical value in the current host computer's
 * section.
 * 
 * Example:
 * 
 * mountrule.1.local = /localscratch
 * mountrule.1.remote = /scratch/%mountname
 * 
 * [Computer = frodo]
 * 
 * [Computer = pippin]
 * mountrule.1.local = /localscratch
 * mountrule.1.remote = /localscratch
 * 
 * Run getRemotePath() on frodo:
 * 
 * Input:  local path = /localscratch
 * Return:  remote path = /scratch/frodo
 * 
 * Run getRemotePath() on pippin:
 * 
 * Input:  local path = /localscratch
 * Return:  remote path = /localscratch
 * 
 * 
 * Requirements:
 * 
 * It is required to have a local rule and a remote rule with the same number
 * and in the same area (global attributes area or Computer section).  Each
 * mount rule attribute must have a value.
 * 
 * When %mountname is used, then a Computer section for the current host
 * computer must exist.  If %mountname is used and the Computer section for the
 * current host computer is set to "localhost", then a mountname attribute is
 * required for that section.
 * 
 * Example:
 * 
 * Correct use of localhost Computer section with %mountname:
 * 
 * mountrule.1.local = /localscratch
 * mountrule.1.remote = /scratch/%mountname
 * 
 * [Computer = localhost]
 * mountrule = automount
 * 
 * Run getRemotePath():
 * 
 * Input:  local path = /localscratch
 * Return:  remote path = /scratch/automount
 * 
 * 
 * Incorrect use of localhost Computer section with %mountname:
 * 
 * mountrule.1.local = /localscratch
 * mountrule.1.remote = /scratch/%mountname
 * 
 * [Computer = localhost]
 * 
 * Run getRemotePath():
 * 
 * Input:  local path = /localscratch
 * Return:  null
 * 
 * Warnings of problems with the mount rules in cpu.adoc will be placed in
 * etomo_err.log</p>
 * 
 * <p>Copyright: Copyright (c) 2005</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */

public final class RemotePath {
  public static final String rcsid = "$Id$";

  public static final RemotePath INSTANCE = new RemotePath();
  static final String MOUNT_RULE = "mountrule";
  static final String LOCAL = "local";
  static final String REMOTE = "remote";
  static final String AUTODOC = AutodocFactory.CPU;
  static final String MOUNT_NAME = "mountname";
  static final String MOUNT_NAME_TAG = EtomoAutodoc.VAR_TAG + MOUNT_NAME;

  private Vector localMountRules = null;
  private Vector remoteMountRules = null;
  private boolean mountRulesLoaded = false;
  private String mountName = null;
  private String hostName = null;
  private ReadOnlySection localSection = null;

  private RemotePath() {
  }

  /**
   * Loads mount rules if necesasry.  Finds the first local rule which matches
   * localPath.  Converts localPath to a remote path.
   * @param localPath
   * @return remote path or null
   */
  public final String getRemotePath(BaseManager manager, String localPath, AxisID axisID)
      throws InvalidMountRuleException {
    loadMountRules(manager, axisID);
    return getRemotePath(getRule(localPath), localPath);
  }

  /**
   * Loads mount rules if necesasry.
   * @param sectionName
   * @return true if sectionName equals "localhost" or matches the section name
   * of the local computer
   */
  public final boolean isLocalSection(String sectionName, BaseManager manager,
      AxisID axisID) {
    if (sectionName == null) {
      return false;
    }
    if (sectionName.equals(Node.LOCAL_HOST_NAME)) {
      return true;
    }
    loadMountRules(manager, axisID);
    //check the local section
    if (localSection == null) {
      return false;
    }
    if (sectionName.equals(localSection.getName())) {
      return true;
    }
    return false;
  }

  /**
   * Given the index of a local mount rule which matches localPath, converts
   * localPath to a remote path.
   * @param ruleIndex
   * @param localPath
   * @return remote path or null
   */
  private final String getRemotePath(int ruleIndex, String localPath)
      throws InvalidMountRuleException {
    if (ruleIndex == -1) {
      return null;
    }
    String remoteMountRule = (String) remoteMountRules.get(ruleIndex);
    //look for %mountname in remote mount rule
    int mountNameIndex = remoteMountRule.indexOf(MOUNT_NAME_TAG);
    if (mountNameIndex != -1) {
      if (mountName == null) {
        throw new InvalidMountRuleException(
            "Unable to use remote mount rule, \""
                + remoteMountRule
                + "\" because %mountname has no value.  "
                + "Check $IMOD_CALIB_DIR/cpu.adoc.  "
                + "The mount name is defined by the section associated with your local computer.  "
                + "It will be set to the section name or, optionally, to the value of a mountname attribute in the section.");
      }
      //substitute mount name
      StringBuffer buffer = new StringBuffer();
      if (mountNameIndex > 0) {
        buffer.append(remoteMountRule.substring(0, mountNameIndex));
      }
      buffer.append(mountName);
      if (mountNameIndex + MOUNT_NAME_TAG.length() < remoteMountRule.length()) {
        buffer.append(remoteMountRule.substring(mountNameIndex + MOUNT_NAME_TAG.length(),
            remoteMountRule.length()));
      }
      remoteMountRule = buffer.toString();
    }
    //create remote path
    return remoteMountRule
        + localPath.substring(((String) localMountRules.get(ruleIndex)).length(),
            localPath.length());
  }

  /**
   * Loads mount rules from cpu.adoc.  Tries to find a Computer section for the
   * current host computer.  Loads section mount rules if a section has been
   * found.  Loads global mount rules.
   * @param manager
   * @param axisID
   */
  private synchronized final void loadMountRules(BaseManager manager, AxisID axisID) {
    //only try to load mount rules once
    if (mountRulesLoaded) {
      return;
    }
    mountRulesLoaded = true;
    localMountRules = new Vector();
    remoteMountRules = new Vector();

    ReadOnlyAutodoc autodoc;
    try {
      autodoc = AutodocFactory.getInstance(manager, AUTODOC, axisID);
    }
    catch (IOException e) {
      e.printStackTrace();
      return;
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
      return;
    }
    if (autodoc == null) {
      return;
    }
    //first load section-level mount rules
    //look for a section name that is the same as the output of hostname
    hostName = Network.getLocalHostName(manager, axisID, manager.getPropertyUserDir());
    if ((localSection = loadMountRules(autodoc, CpuAdoc.COMPUTER_SECTION_TYPE, hostName,
        true)) == null) {
      //try looking for a section name that is the same as the stripped version
      //of the hostname.
      if (hostName != null) {
        int stripIndex = hostName.indexOf('.');
        if (stripIndex == -1
            || (localSection = loadMountRules(autodoc, CpuAdoc.COMPUTER_SECTION_TYPE,
                hostName.substring(0, stripIndex), true)) == null) {
          //look for a section name called "localhost"
          localSection = loadMountRules(autodoc, CpuAdoc.COMPUTER_SECTION_TYPE,
              Node.LOCAL_HOST_NAME, false);
        }
      }
    }
    //load global mount rules
    loadMountRules(autodoc);
  }

  /**
   * Loads mount rules from a section if the section exists.
   * @param autodoc
   * @param sectionName
   * @return false if section doesn't exist
   */
  private final ReadOnlySection loadMountRules(ReadOnlyAutodoc autodoc,
      String sectionType, String sectionName, boolean sectionNameCanBeMountName) {
    ReadOnlySection section = autodoc.getSection(sectionType, sectionName);
    if (section == null) {
      return null;
    }
    //set mount name
    ReadOnlyAttribute mountName = section.getAttribute(MOUNT_NAME);
    if (mountName == null) {
      if (sectionNameCanBeMountName) {
        //use section name as mount name
        this.mountName = sectionName;
      }
    }
    else {
      //load mount name from "mountname" attribute
      this.mountName = mountName.getValue();
      if (this.mountName == null) {
        //allow empty mount name
        this.mountName = "";
      }
    }
    //load mount rules
    loadMountRules(section.getAttribute(MOUNT_RULE), sectionType, sectionName);
    return section;
  }

  /**
   * Loads mount rules from the global attribute area of cpu.adoc.
   * @param autodoc
   */
  private final void loadMountRules(ReadOnlyAutodoc autodoc) {
    loadMountRules(autodoc.getAttribute(MOUNT_RULE), null, null);
  }

  /**
   * Loads mount rules from a mountrule attribute.
   * @param mountRules
   */
  private final void loadMountRules(ReadOnlyAttribute mountRules, String sectionType,
      String sectionName) {
    if (mountRules == null) {
      return;
    }
    //load the rules in order of the mountrule number
    int attributeNumber = 1;
    ReadOnlyAttribute numberAttribute = mountRules.getAttribute(attributeNumber);
    while (numberAttribute != null) {
      ReadOnlyAttribute localRule = numberAttribute.getAttribute(LOCAL);
      ReadOnlyAttribute remoteRule = numberAttribute.getAttribute(REMOTE);
      //run valid rule check
      if (isValidRule(localRule, remoteRule, attributeNumber, sectionType, sectionName)) {
        //add rule
        localMountRules.add(localRule.getValue());
        remoteMountRules.add(remoteRule.getValue());
      }
      numberAttribute = mountRules.getAttribute(++attributeNumber);
    }
  }

  /**
   * Checks the validity of a localRule and remoteRule.  Writes warning
   * messages to etomo_err.log.
   * @param localRule
   * @param remoteRule
   * @param mountRuleNumber
   * @param sectionName
   * @return true if the rule is valid, false if it is invalid
   */
  private final boolean isValidRule(ReadOnlyAttribute localRule,
      ReadOnlyAttribute remoteRule, int mountRuleNumber, String sectionType,
      String sectionName) {
    //create the start of the error message
    StringBuffer errorTitle = new StringBuffer("Warning:  Problem");
    if (hostName != null) {
      errorTitle.append(" using " + hostName);
    }
    errorTitle.append(" with " + DatasetFiles.getAutodocName(AUTODOC));
    if (sectionType == null) {
      sectionType = CpuAdoc.COMPUTER_SECTION_TYPE;
    }
    String sectionHeader = AutodocTokenizer.OPEN_CHAR + sectionType + ' '
        + AutodocTokenizer.DEFAULT_DELIMITER + ' ';
    if (sectionName != null) {
      errorTitle.append(", section " + sectionHeader + sectionName
          + AutodocTokenizer.CLOSE_CHAR);
    }
    errorTitle.append(" - ");
    //validate local rule
    if (!isValidRule(localRule, mountRuleNumber, errorTitle.toString(), "local")) {
      return false;
    }
    //validate remote rule
    if (!isValidRule(remoteRule, mountRuleNumber, errorTitle.toString(), "remote")) {
      return false;
    }
    //can't use %mountname if there is no mount name
    //causes:  either no section for this computer or the localhost section
    //doesn't have a mountname attribute
    String remoteValue = remoteRule.getValue();
    if (remoteValue != null && remoteValue.indexOf(MOUNT_NAME_TAG) != -1
        && mountName == null) {
      System.err
          .println(errorTitle
              + "remote mount rule "
              + mountRuleNumber
              + ".  Cannot use "
              + MOUNT_NAME_TAG
              + " because there is no mountname.\nEither there is no mountname entry under the "
              + sectionHeader + Node.LOCAL_HOST_NAME + AutodocTokenizer.CLOSE_CHAR
              + " section or there is no section for this computer.\n");
      //pass this problem so that it can be shown to the user
      return true;
    }
    return true;
  }

  /**
   * Checks the validity of a rule.  Writes warning messages to etomo_err.log.
   * @param rule
   * @param mountRuleNumber
   * @param errorTitle
   * @param ruleType
   * @return
   */
  private final boolean isValidRule(ReadOnlyAttribute rule, int mountRuleNumber,
      String errorTitle, String ruleType) {
    //rule must exist
    if (rule == null) {
      System.err.println(errorTitle + ruleType + " mount rule " + mountRuleNumber
          + " is missing.");
      return false;
    }
    String value = rule.getValue();
    //local rule must not have an empty value
    if (value == null) {
      System.err.println(errorTitle + ruleType + " mount rule " + mountRuleNumber
          + " cannot be blank.");
      return false;
    }
    File path = new File(value);
    if (!path.isAbsolute()) {
      System.err.println(errorTitle + ruleType + " mount rule " + mountRuleNumber
          + " must be an absolute directory path.");
      return false;
    }
    return true;
  }

  /**
   * Tries to match localPath against a local rule.
   * @param localPath
   * @return the index of the matching local rule
   */
  private final int getRule(String localPath) {
    //check the rules in order
    for (int i = 0; i < localMountRules.size(); i++) {
      if (localPath.startsWith((String) localMountRules.get(i))) {
        return i;
      }
    }
    return -1;
  }

  /**
   * Resets the instances so that mount rules can be reloaded.
   */
  final void reset() {
    if (!EtomoDirector.INSTANCE.getArguments().isTest()) {
      throw new IllegalStateException();
    }
    localMountRules = null;
    remoteMountRules = null;
    mountRulesLoaded = false;
    mountName = null;
    hostName = null;
  }

  /**
   * For testing.  Calls the static getHostName() function.
   * @param manager
   * @param axisID
   * @return
   */
  final String getHostName_test(BaseManager manager, AxisID axisID) {
    if (!EtomoDirector.INSTANCE.getArguments().isTest()) {
      throw new IllegalStateException();
    }
    return Network.getLocalHostName(manager, axisID, manager.getPropertyUserDir());
  }

  /**
   * For testing.
   * @return mountRulesLoaded
   */
  final boolean isMountRulesLoaded_test() {
    if (!EtomoDirector.INSTANCE.getArguments().isTest()) {
      throw new IllegalStateException();
    }
    return mountRulesLoaded;
  }

  /**
   * For testing.
   * @return localMountRules == null
   */
  final boolean localMountRulesIsNull_test() {
    if (!EtomoDirector.INSTANCE.getArguments().isTest()) {
      throw new IllegalStateException();
    }
    return localMountRules == null;
  }

  /**
   * For testing.
   * @return localMountRules.size()
   */
  final int getLocalMountRulesSize_test() {
    if (!EtomoDirector.INSTANCE.getArguments().isTest()) {
      throw new IllegalStateException();
    }
    return localMountRules.size();
  }

  /**
   * For testing.
   * @return remoteMountRules == null
   */
  final boolean remoteMountRulesIsNull_test() {
    if (!EtomoDirector.INSTANCE.getArguments().isTest()) {
      throw new IllegalStateException();
    }
    return remoteMountRules == null;
  }

  /**
   * For testing.
   * @return remoteMountRules.size()
   */
  final int getRemoteMountRulesSize_test() {
    if (!EtomoDirector.INSTANCE.getArguments().isTest()) {
      throw new IllegalStateException();
    }
    return remoteMountRules.size();
  }

  public static final class InvalidMountRuleException extends Exception {
    InvalidMountRuleException(String message) {
      super(message);
    }
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.25  2010/02/17 05:05:58  sueh
 * <p> bug# 1301 Using manager instead of manager key for popping up
 * <p> messages.
 * <p>
 * <p> Revision 1.24  2010/01/11 23:59:15  sueh
 * <p> bug# 1299 Removed responsibility anything other then cpu.adoc from
 * <p> CpuAdoc.  Placed responsibility for information about the network in the
 * <p> Network class.
 * <p>
 * <p> Revision 1.23  2009/10/01 18:52:55  sueh
 * <p> bug# 1233 In loadMountRules checking for null to make sure that etomo
 * <p> can't get stuck when leaving a dialog.
 * <p>
 * <p> Revision 1.22  2009/04/13 22:58:35  sueh
 * <p> bug# 1207 Made getHostName available to other classes.
 * <p>
 * <p> Revision 1.21  2009/03/17 00:46:43  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.20  2009/02/04 23:38:24  sueh
 * <p> bug# 1158 Changed id and exception classes in LogFile.
 * <p>
 * <p> Revision 1.19  2007/12/26 22:41:38  sueh
 * <p> bug# 1052 Moved argument handling from EtomoDirector to a separate class.
 * <p>
 * <p> Revision 1.18  2007/11/12 15:04:21  sueh
 * <p> bug# 1047 Formatted.
 * <p>
 * <p> Revision 1.17  2007/09/27 21:07:12  sueh
 * <p> bug# 1044 Added Queue sections to CpuAdoc.
 * <p>
 * <p> Revision 1.16  2007/09/07 00:31:09  sueh
 * <p> bug# 989 Using a public INSTANCE to refer to the EtomoDirector singleton
 * <p> instead of getInstance and createInstance.
 * <p>
 * <p> Revision 1.15  2007/07/17 21:45:48  sueh
 * <p> bug# 108 Moved LOCAL_HOST to CpuAdoc.
 * <p>
 * <p> Revision 1.14  2007/06/08 22:22:53  sueh
 * <p> Corrected typo in function getRemotePath.
 * <p>
 * <p> Revision 1.13  2007/05/21 22:32:00  sueh
 * <p> bug# 1000 Moved ProcessorTable.SECTION_TYPE to CpuAdoc.
 * <p>
 * <p> Revision 1.12  2007/03/21 19:50:37  sueh
 * <p> bug# 964 Limiting access to autodoc classes by using ReadOnly interfaces.
 * <p> Added AutodocFactory to create Autodoc instances.
 * <p>
 * <p> Revision 1.11  2007/03/15 21:55:43  sueh
 * <p> bug# 964 Added ReadOnlyAttribute, which is used as an interface for Attribute,
 * <p> unless the Attribute needs to be modified.
 * <p>
 * <p> Revision 1.10  2007/03/05 21:29:40  sueh
 * <p> bug# 964 Stop controlling autodoc instances, except for the standard ones.
 * <p>
 * <p> Revision 1.9  2007/03/01 01:48:02  sueh
 * <p> bug# 964 Added LogFile to Autodoc.
 * <p>
 * <p> Revision 1.8  2006/07/20 23:15:11  sueh
 * <p> bug# 885 GetRemotePath():  throw an InvalidMountRuleException if the mount
 * <p> name hasn't been replaced in the remote path.
 * <p>
 * <p> Revision 1.7  2006/05/22 22:52:40  sueh
 * <p> bug# 577 Placed commands in a String[] rather then a String.
 * <p>
 * <p> Revision 1.6  2006/04/25 19:41:31  sueh
 * <p> bug# 787 Moved the autodoc variable tag (%) to EtomoAutodoc.
 * <p>
 * <p> Revision 1.5  2006/01/12 17:39:10  sueh
 * <p> bug# 798 Moved the autodoc classes to etomo.storage.autodoc.
 * <p>
 * <p> Revision 1.4  2006/01/11 23:22:42  sueh
 * <p> bug# 675 Changed Attribute.getUnformattedValue to getValue.
 * <p>
 * <p> Revision 1.3  2005/12/01 00:26:13  sueh
 * <p> bug# 775 Added isSectionLocal().  Saving the local section when loading
 * <p> rules so that isSectionLocal() can use it.
 * <p>
 * <p> Revision 1.2  2005/11/14 22:36:45  sueh
 * <p> bug# 733 updated description.
 * <p>
 * <p> Revision 1.1  2005/11/10 18:20:29  sueh
 * <p> bug# 733 Class to translate a local directory path to a remote directory
 * <p> path.
 * <p> </p>
 */
