package etomo.type;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2007</p>
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
 * <p> Revision 1.1  2007/04/13 19:56:28  sueh
 * <p> bug# 964 Interface for enumeration types, so that they can be stored in radio
 * <p> buttons.
 * <p> </p>
 */
public interface EnumeratedType {
  public static final String rcsid = "$Id$";

  public boolean isDefault();

  public ConstEtomoNumber getValue();
  
  public String toString();
  
  public String getLabel();
}
