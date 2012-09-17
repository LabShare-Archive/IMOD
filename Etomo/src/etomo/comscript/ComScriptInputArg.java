package etomo.comscript;

/**
 * <p>Description: This class models a single standard input entry for a command
 * in an IMOD com script.  The associate comments are also handled with each
 * line returned as a separate element of a String array.</p>
 *
 * <p>Copyright: Copyright (c) 2002</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 * <p> $Log$
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.2  2002/09/17 23:36:05  rickg
 * <p> source reformat
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */

public class ComScriptInputArg {
  public static final String rcsid = "$Id$";

  private String[] comments = new String[0];
  private String argument = null;
  private boolean debug = false;

  /**
   * Default constructor.  A zero length String is created to represent the
   * comments and the argument is null.
   */
  public ComScriptInputArg() {
  }

  /**
   * Copy constructor
   */
  public ComScriptInputArg(ComScriptInputArg src) {
    argument = src.getArgument();
    comments = src.getComments();
  }

  /**
   * Set the argument line.  The whole line is considerered the argument,
   * comments are not parsed from the line.
   * @param a string containing the argument line.
   */
  public void setArgument(String argument) {
    setArgument(argument, false);
  }

  /**
   * Set the argument line.  The argument is considered the first whitespace
   * separated token in the line.  The remainder of the line is appended onto
   * the comments for this input argument.
   * @param a string containing the argument line.
   * @param a boolean specifying whether to parse the comments or not
   */
  public void setArgument(String argument, boolean parseComments) {
    if (debug) {
      System.out.println("ComScriptInputArg:setArgument:argument=" + argument
          + ",parseComments=" + parseComments);
    }
    if (parseComments) {
      String[] parse = argument.split("\\s+", 2);
      this.argument = parse[0];
      if (parse.length > 1) {
        String[] append = new String[1];
        if (parse[1].startsWith("#")) {
          append[0] = parse[1];
        }
        else {
          append[0] = "# " + parse[1];
        }
        addComments(append);
      }
    }
    else {
      this.argument = argument;
    }
  }

  /**
   * Set the argument line with an integer.
   */
  public void setArgument(boolean arg) {
    if (arg)
      argument = "1";
    else
      argument = "0";
  }

  /**
   * Set the argument line with an integer.
   */
  public void setArgument(int arg) {
    argument = String.valueOf(arg);
  }

  /**
   * Set the argument line with a double.
   */
  public void setArgument(double arg) {
    argument = String.valueOf(arg);
  }

  /**
   * Set the argument line with FortranInputString.
   */
  public void setArgument(FortranInputString arg) {
    argument = arg.toString();
  }

  /**
   * Get the argument line.
   */
  public String getArgument() {
    return argument;
  }

  /**
   * Set the comments for this argument.
   * @param comments an array of Strings, each element of the array represents
   * a line of comments.
   */
  public void setComments(String[] comments) {
    //  make a defensive copy of the array
    this.comments = new String[comments.length];
    for (int i = 0; i < comments.length; i++) {
      this.comments[i] = comments[i];
    }
  }

  public boolean setDebug(boolean input) {
    boolean oldDebug = debug;
    debug = input;
    return oldDebug;
  }

  public String toString() {
    return "[" + argument + "]";
  }

  /**
   * Get the comments as an array of Strings, each comment line is a separate
   * element in the array.
   * @return a String array containing the comments, this is a zero length array
   * if there are no comments.
   */
  public String[] getComments() {
    //  make a defensive copy of the array
    String[] safeArray = new String[comments.length];
    for (int i = 0; i < comments.length; i++) {
      safeArray[i] = comments[i];
    }
    return safeArray;
  }

  /**
   * Add the array of comments on to the end of the existing comments.
   * @param a String array containing the comment lines to append.
   */
  public void addComments(String[] append) {
    String[] existingComments = comments;
    comments = new String[existingComments.length + append.length];

    for (int i = 0; i < existingComments.length; i++) {
      comments[i] = existingComments[i];
    }

    for (int i = 0; i < append.length; i++) {
      comments[i + existingComments.length] = append[i];
    }
  }
}
