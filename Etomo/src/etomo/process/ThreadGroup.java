package etomo.process;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.1  2006/10/10 05:14:18  sueh
 * <p> bug# 931 Override ThreadGroup, so that uncaught exceptions can be handled
 * <p> during a UITest.
 * <p> </p>
 */
class ThreadGroup extends java.lang.ThreadGroup {
  public static final String rcsid = "$Id$";

  public ThreadGroup(String name) {
    super(name);
  }

  public ThreadGroup(ThreadGroup parent, String name) {
    super(parent, name);
  }

  public void uncaughtException(Thread thread, Throwable throwable) {
    super.uncaughtException(thread, throwable);
    //Handle an uncaught runtime exception when using UITest.  This is only
    //necessary when there is no parent ThreadGroup or the parent is the
    //java.lang ThreadGroup.  The "non-runtime" exceptions should cause Etomo to
    //fail or be handled by Etomo.
    if (throwable instanceof ThreadDeath) {
      return;
    }
    java.lang.ThreadGroup parent = getParent();
    if (parent != null && parent instanceof etomo.process.ThreadGroup) {
      return;
    }
    UncaughtException.INSTANCE.setThrowable(throwable);
  }
}
