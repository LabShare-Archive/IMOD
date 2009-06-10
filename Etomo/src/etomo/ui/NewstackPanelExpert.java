package etomo.ui;

import etomo.ProcessSeries;
import etomo.type.ProcessResultDisplay;
import etomo.type.Run3dmodMenuOptions;

interface NewstackPanelExpert {
  public void newst(ProcessResultDisplay processResultDisplay,
      ProcessSeries processSeries, Deferred3dmodButton deferred3dmodButton,
      Run3dmodMenuOptions run3dmodMenuOptions);
}
