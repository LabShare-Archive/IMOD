package etomo.type;

import java.util.Properties;

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
 * <p> Revision 1.5  2010/01/13 21:54:29  sueh
 * <p> bug# 1298 Removed lstThresholdArray.
 * <p>
 * <p> Revision 1.4  2009/12/08 02:44:39  sueh
 * <p> bug# 1286 Removed parserLstThresholds and parserIterationListSize from
 * <p> PeetState; saving lstThresholds and iterationListSize after prmParser is run.
 * <p>
 * <p> Revision 1.3  2009/12/01 00:24:37  sueh
 * <p> bug# 1285 Added getParserLstThresholds and resetLstThresholdsArray.
 * <p>
 * <p> Revision 1.2  2007/08/29 21:44:54  sueh
 * <p> bug# 1041 Made BaseState an abstract class.
 * <p>
 * <p> Revision 1.1  2007/05/11 16:05:45  sueh
 * <p> bug# 964 Class to store the states of variables used in processes after
 * <p> the processes are finished.
 * <p> </p>
 */
public final class PeetState extends BaseState {
  public static final String rcsid = "$Id$";

  private static final String KEY = "PeetState";
  private static final String CURRENT_VERSION = "1.1";
  private static final String PARSER_KEY = "Parser";
  private static final String ITERATION_LIST_SIZE_KEY = "IterationListSize";
  private static final String LST_THRESHOLDS_KEY = "LstThresholds";

  /**
   * @deprecated
   * Backward compatbility:  put the data saved by this variable into
   * iterationListSize.  Either they are the same or iterationListSize is
   * missing.
   */
  private final EtomoNumber parserIterationListSize = new EtomoNumber(PARSER_KEY + "."
      + ITERATION_LIST_SIZE_KEY);
  /**
   * @deprecated
   * Backward compatbility:  put the data saved by this variable into
   * lstThresholdsArray.  Either they are the same or lstThresholdsArray is
   * missing.
   */
  private final IntKeyList parserLstThresholdsArray = IntKeyList
      .getStringInstance(PARSER_KEY + "." + LST_THRESHOLDS_KEY);
  private final EtomoNumber iterationListSize = new EtomoNumber(ITERATION_LIST_SIZE_KEY);
  private final EtomoVersion version = EtomoVersion.getEmptyInstance("Version");

  public void store(Properties props) {
    store(props, "");
  }

  public void load(Properties props) {
    load(props, "");
  }

  public void setIterationListSize(final int input) {
    iterationListSize.set(input);
  }

  public int getIterationListSize() {
    return iterationListSize.getInt();
  }

  public void store(final Properties props, String prepend) {
    super.store(props, prepend);
    prepend = createPrepend(prepend);
    version.set(CURRENT_VERSION);
    version.store(props, prepend);
    iterationListSize.store(props, prepend);
  }

  public boolean equals(PeetState input) {
    return super.equals(input);
  }

  public void load(final Properties props, String prepend) {
    super.load(props, prepend);
    //reset
    parserIterationListSize.reset();
    parserLstThresholdsArray.reset();
    iterationListSize.reset();
    version.reset();
    //load
    prepend = createPrepend(prepend);
    version.load(props, prepend);
    //backward compatibility for version 1.0
    if (version.le(EtomoVersion.getDefaultInstance("1.0"))) {
      parserIterationListSize.load(props, prepend);
      iterationListSize.set(parserIterationListSize);
      parserLstThresholdsArray.load(props, prepend);
    }
    else {
      iterationListSize.load(props, prepend);
    }
  }

  String createPrepend(final String prepend) {
    if (prepend == "") {
      return KEY;
    }
    return prepend + "." + KEY;
  }
}
