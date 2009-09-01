package etomo.ui;

import etomo.comscript.ConstNewstParam;
import etomo.comscript.FortranInputSyntaxException;
import etomo.comscript.NewstParam;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2008</p>
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
 * <p> Revision 1.1  2009/06/12 19:49:51  sueh
 * <p> bug# 1221 Interface for a display containing the newstack parameters.
 * <p> </p>
 */
public interface NewstackDisplay {
  public static final String rcsid = "$Id$";

  public void getParameters(NewstParam newstParam)
      throws FortranInputSyntaxException;

  public void setParameters(ConstNewstParam param);

  public boolean validate();
}
