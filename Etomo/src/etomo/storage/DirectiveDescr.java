package etomo.storage;

import etomo.Arguments.DebugLevel;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2013</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
* 
* <p> $Log$ </p>
*/
public interface DirectiveDescr {
  public static final String rcsid = "$Id:$";

  public String getName();

  public String getDescription();

  public DirectiveValueType getValueType();

  public boolean isBatch();

  public boolean isTemplate();

  public DirectiveDescrEtomoColumn getEtomoColumn();

  public String getLabel();

  public DirectiveDescrFile.ChoiceList getChoiceList(DebugLevel debug);
}