package etomo.process;
import etomo.ApplicationManager;
import etomo.type.AxisID;
import etomo.type.AxisType;

import java.io.File;
/*
 * <p>Description: This class manages the opening, closing and sending of 
 * messages to the appropriate imod processes.</p>
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
 * <p> </p>
 */
public class ImodManager {
  public static final String rcsid = "$Id$";
  
  private ApplicationManager appManager;
	private AxisType axisType;

	private int wIDRawStackA = 0;
	private int wIDRawStackB = 0;
	private int wIDCoarseAlignedA = 0;
	private int wIDCoarseAlignedB = 0;
	private int wIDFineAlignedA = 0;
	private int wIDFineAlignedB = 0;
	private int wIDSampleA = 0;
	private int wIDSampleB = 0;
	private int wIDTomgramA = 0;
	private int wIDTomogramB = 0;
	private int wIDCombined = 0;
	
	/**
	 * Default constructor, this only manages a single imod instance and will
	 * throw an exception if the AxisID is not AxisID.ONLY when specified.
	 * AxisType can also be respecified by the setAxisType method.
	 */
	public ImodManager(ApplicationManager appMgr) { 
		appManager = appMgr;
		axisType = AxisType.SINGLE_AXIS;
	}
	

	/**
	 * Construction with AxisType specification.  An axisType of
	 * AxisType.DUAL_AXIS will manage two instances of imod and requires the
	 * specifiation of which AxisID each method call is operating on.
	 */
	public ImodManager(ApplicationManager appMgr, AxisType createAxisType) {
		appManager = appMgr;
		axisType = createAxisType;		
	}
	

	/**
	 * Set the axis type of this ImodManager
	 */
	public void setAxisType(AxisType newAxisType) {
		// FIXME how else does the flag state need to be modified
		axisType = newAxisType;
	}
	
	/**
	 * Open an imod window with the specified model.  If imod could not open the
	 * image file and Exception is thrown containing the requested image filename.
	 */
	public void openImod(String imageFile) throws Exception {
		//
		//  
	}
	
  private void startSystemProgramThread(String command) {
    SystemProgram sysProgram = new SystemProgram(command);
    sysProgram.setWorkingDirectory(new File(appManager.getWorkingDirectory()));

    //  Start the system program thread
    Thread sysProgThread = new Thread(sysProgram);
    sysProgThread.start();
    System.out.println("Started " + command);
    System.out.println("  working directory: " +
      appManager.getWorkingDirectory());
  }
	
}
