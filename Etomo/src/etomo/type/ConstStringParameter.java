package etomo.type;

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
* <p> $Log$ </p>
*/
public interface ConstStringParameter {
  public static final String rcsid = "$Id$";

  public boolean equals(String input);

  public boolean endsWith(String input);

  public boolean isEmpty();

  public String toString();
}
