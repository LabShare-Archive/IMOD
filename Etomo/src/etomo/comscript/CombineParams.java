package etomo.comscript;

import java.util.Properties;

import etomo.storage.Storable;
import etomo.type.CombinePatchSize;
import etomo.type.FiducialMatch;

/**
 * <p>Description: </p>
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
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */

public class CombineParams extends ConstCombineParams implements Storable {
  public static final String rcsid =
    "$Id$";

  public void setMatchBtoA(boolean isBtoA) {
    matchBtoA = isBtoA;
  }

  public void setFiducialMatch(FiducialMatch match) {
    fiducialMatch = match;
  }

  public void setFiducialMatchListA(String list) {
    fiducialMatchListA.parseString(list);
  }

  public void setFiducialMatchListB(String list) {
    fiducialMatchListB.parseString(list);
  }

  public void setPatchSize(CombinePatchSize size) {
    patchSize = size;
  }

  public void setPatchRegionModel(String modelFileName) {
    patchRegionModel = modelFileName;
  }

  public void setTempDirectory(String directoryName) {
    tempDirectory = directoryName;
  }

  public void setManualCleanup(boolean isManual) {
    manualCleanup = isManual;
  }

  /**
   *  Insert the objects attributes into the properties object.
   */
  public void store(Properties props) {
    store(props, "");
  }
  public void store(Properties props, String prepend) {
    String group;
    if (prepend == "") {
      group = "Combine.";
    }
    else {
      group = prepend + "Combine.";
    }
    props.setProperty(group + "MatchBtoA", String.valueOf(matchBtoA));
  }

  /**
   *  Get the objects attributes from the properties object.
   */
  public void load(Properties props) {
    load(props, "");
  }
  public void load(Properties props, String prepend) {
    String group;
    if (prepend == "") {
      group = "Combine.";
    }
    else {
      group = prepend + "Combine.";
    }
    matchBtoA =
      Boolean
        .valueOf(props.getProperty(group + "MatchBtoA", "true"))
        .booleanValue();

  }

}
