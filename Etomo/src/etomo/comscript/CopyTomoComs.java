package etomo.comscript;

import java.io.File;
import java.io.IOException;

import etomo.process.SystemProgram;
import etomo.type.AxisType;
import etomo.type.ConstMetaData;
import etomo.type.DataSource;
import etomo.type.TiltAngleType;
import etomo.type.ViewType;
/**
 * <p>
 * Description:
 * </p>
 * 
 * <p>
 * Copyright: Copyright (c) 2002
 * </p>
 * 
 * <p>
 * Organization: Boulder Laboratory for 3D Fine Structure, University of
 * Colorado
 * </p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p>
 * $Log$
 * <p>
 * Revision 2.10 2003/05/13 19:59:43 rickg
 * <p>
 * Added -f option to tcsh call
 * <p>
 * <p>
 * Revision 2.9 2003/05/12 23:23:14 rickg
 * <p>
 * Changed tcsh call to tcsh -ec
 * <p>
 * <p>
 * Revision 2.8 2003/05/12 01:21:05 rickg
 * <p>
 * Added explici tcsh call to copytomocoms
 * <p>
 * <p>
 * Revision 2.7 2003/05/08 23:19:03 rickg
 * <p>
 * Standardized debug setting
 * <p>
 * <p>
 * Revision 2.6 2003/05/07 22:31:59 rickg
 * <p>
 * Don't need to set working directory since it defaults to user.dir
 * <p>
 * System property user.dir now defines the working directory
 * <p>
 * <p>
 * Revision 2.5 2003/04/29 20:22:38 rickg
 * <p>
 * Handles all three cases of tilt angle specification now
 * <p>
 * <p>
 * Revision 2.4 2003/04/24 17:46:54 rickg
 * <p>
 * Changed fileset name to dataset name
 * <p>
 * <p>
 * Revision 2.3 2003/03/20 17:22:18 rickg
 * <p>
 * Comment update
 * <p>
 * <p>
 * Revision 2.2 2003/03/02 23:30:41 rickg
 * <p>
 * Combine layout in progress
 * <p>
 * <p>
 * Revision 2.1 2003/01/29 20:45:45 rickg
 * <p>
 * Debug messages to stderr instead of stdout
 * <p>
 * <p>
 * Revision 2.0 2003/01/24 20:30:31 rickg
 * <p>
 * Single window merge to main branch
 * <p>
 * <p>
 * Revision 1.4 2002/10/10 18:54:01 rickg
 * <p>
 * Enabled SystemProgram debugging and remove local
 * <p>
 * writing to stdout.
 * <p>
 * <p>
 * Revision 1.3 2002/10/09 21:35:44 rickg
 * <p>
 * Removed stdout messages, can now be gotten from the enableDebug method
 * <p>
 * in SystemProgram
 * <p>
 * <p>
 * Revision 1.2 2002/10/09 21:19:40 rickg
 * <p>
 * Reformat from emacs
 * <p>
 * <p>
 * Revision 1.1 2002/09/09 22:57:02 rickg
 * <p>
 * Initial CVS entry, basic functionality not including combining
 * <p>
 * </p>
 */

public class CopyTomoComs {
	public static final String rcsid =
		"$Id$";
	SystemProgram copytomocoms;
	String commandLine = "";
	int exitValue;
	ConstMetaData metaData;

	public CopyTomoComs(ConstMetaData metaData, String IMODPath) {

		this.metaData = metaData;
		String IMODBinPath = IMODPath + File.separator + "bin" + File.separator;

		//  Create a new SystemProgram object for copytomocom, set the
		//  working directory and stdin array.
		commandLine = "tcsh -ef " + IMODBinPath + "copytomocoms";
		copytomocoms = new SystemProgram(commandLine);

		genStdInputSequence();
	}

	/**
	 * Return the current command line string
	 * 
	 * @return
	 */
	public String getCommandLine() {
		return commandLine;
	}

	/**
	 * Generate the standard input sequence
	 */
	private void genStdInputSequence() {
		String[] tempStdInput = new String[19];

		//  compile the input sequence to copytomocoms
		int lineCount = 0;

		//  Axis type: single or dual
		if (metaData.getAxisType() == AxisType.SINGLE_AXIS) {
			tempStdInput[lineCount++] = "1";
		}
		else {
			tempStdInput[lineCount++] = "2";
		}

		//  Data source: CCD or film
		if (metaData.getDataSource() == DataSource.CCD) {
			tempStdInput[lineCount++] = "c";
		}
		else {
			tempStdInput[lineCount++] = "f";
		}

		//  View type: single or montaged
		if (metaData.getViewType() == ViewType.SINGLE_VIEW) {
			tempStdInput[lineCount++] = "n";
		}
		else {
			tempStdInput[lineCount++] = "y";
		}

		//  CCDEraser and local alignment entries
		//  Always yes tiltalign relies on local entries to save default values
		//  even if they are not used.
		tempStdInput[lineCount++] = "y";
		tempStdInput[lineCount++] = "y";

		//  Dataset name
		tempStdInput[lineCount++] = metaData.getDatasetName();

		//  Backup directory
		tempStdInput[lineCount++] = metaData.getBackupDirectory();

		//  Pixel size
		tempStdInput[lineCount++] = String.valueOf(metaData.getPixelSize());

		//  Fiducial diameter
		tempStdInput[lineCount++] = String.valueOf(metaData.getFiducialDiameter());

		// Image rotation
		tempStdInput[lineCount++] = String.valueOf(metaData.getImageRotation());

		//  Extract the tilt angle data from the stack
		if (metaData.getTiltAngleSpecA().getType() == TiltAngleType.EXTRACT) {
			tempStdInput[lineCount++] = "y";
			tempStdInput[lineCount++] = "0";
		}

		//  Specify a range, creating the rawtilt file
		else if (metaData.getTiltAngleSpecA().getType() == TiltAngleType.RANGE) {
			tempStdInput[lineCount++] = "n";
			tempStdInput[lineCount++] = "1";
			tempStdInput[lineCount++] =
				String.valueOf(
					metaData.getTiltAngleSpecA().getRangeMin()
						+ ","
						+ String.valueOf(metaData.getTiltAngleSpecA().getRangeStep()));
		}
		// Use an existing rawtilt file (this assumes that one is there and has
		// not been deleted by checkTiltAngleFiles()
		else if (metaData.getTiltAngleSpecA().getType() == TiltAngleType.FILE) {
			tempStdInput[lineCount++] = "0";
		}

		else {
			//  TODO Specification of all tilt alngles is not yet implemented
			tempStdInput[lineCount++] = "n";
			tempStdInput[lineCount++] = "-1";
			System.err.println(
				"Specification of all tilt alngles is not yet implemented");
		}

		//  Exclude list
		tempStdInput[lineCount++] = metaData.getExcludeProjectionsA();

		// Second axis entries
		if (metaData.getAxisType() == AxisType.DUAL_AXIS) {

			//    Image rotation
			tempStdInput[lineCount++] = String.valueOf(metaData.getImageRotation());

			//    Extract the tilt angle data from the stack
			if (metaData.getTiltAngleSpecB().getType() == TiltAngleType.EXTRACT) {
				tempStdInput[lineCount++] = "y";
				tempStdInput[lineCount++] = "0";
			}

			//    Specify a range, creating the rawtilt file
			else if (metaData.getTiltAngleSpecB().getType() == TiltAngleType.RANGE) {
				tempStdInput[lineCount++] = "n";
				tempStdInput[lineCount++] = "1";
				tempStdInput[lineCount++] =
					String.valueOf(
						metaData.getTiltAngleSpecB().getRangeMin()
							+ ","
							+ String.valueOf(metaData.getTiltAngleSpecB().getRangeStep()));
			}

			//    Specify a range, creating the rawtilt file
			else if (metaData.getTiltAngleSpecB().getType() == TiltAngleType.FILE) {
				tempStdInput[lineCount++] = "0";
			}

			else {
				//  TODO Specification of all tilt alngles is not yet implemented
				tempStdInput[lineCount++] = "n";
				tempStdInput[lineCount++] = "-1";
				System.err.println(
					"Specification of all tilt alngles is not yet implemented");
			}

			//  Exclude list
			tempStdInput[lineCount++] = metaData.getExcludeProjectionsB();
		}

		//  Copy the temporary stdInput to the real stdInput to get the number
		//  of array elements correct
		String[] stdInput = new String[lineCount];
		for (int i = 0; i < lineCount; i++) {
			stdInput[i] = tempStdInput[i];
		}
		copytomocoms.setStdInput(stdInput);

	}
	/**
	 * Execute the copytomocoms script
	 * 
	 * @return @throws
	 *         IOException
	 */
	public int run() throws IOException {
		int exitValue;

		//  Delete the rawtilt files if extract raw tilts is selected
		checkTiltAngleFiles();

		//  Execute the script
		copytomocoms.setDebug(true);
		copytomocoms.run();
		exitValue = copytomocoms.getExitValue();

		//  TODO we really need to find out what the exception/error condition was
		if (exitValue != 0) {
			throw (new IOException(copytomocoms.getExceptionMessage()));
		}
		return exitValue;
	}

	public String[] getStdError() {
		return copytomocoms.getStdError();
	}

	/**
	 * Check to see if the tilt angle files exist and the tilt angle type is not
	 * FILE. They need to be deleted because the copytomocoms script is not
	 * consistent in the sequence of responses expected.
	 */
	private void checkTiltAngleFiles() {
		String workingDirectory = System.getProperty("user.dir");
		if (metaData.getAxisType() == AxisType.SINGLE_AXIS) {
			if (metaData.getTiltAngleSpecA().getType() != TiltAngleType.FILE) {
				File rawTiltFile =
					new File(workingDirectory, metaData.getDatasetName() + ".rawtlt");
				if (rawTiltFile.exists()) {
					rawTiltFile.delete();
				}
			}
		}
		else {
			if (metaData.getTiltAngleSpecA().getType() != TiltAngleType.FILE) {
				File rawTiltFile =
					new File(workingDirectory, metaData.getDatasetName() + "a.rawtlt");
				if (rawTiltFile.exists()) {
					rawTiltFile.delete();
				}
			}
			if (metaData.getTiltAngleSpecB().getType() != TiltAngleType.FILE) {
				File rawTiltFile =
					new File(workingDirectory, metaData.getDatasetName() + "b.rawtlt");
				if (rawTiltFile.exists()) {
					rawTiltFile.delete();
				}
			}
		}
	}
}
