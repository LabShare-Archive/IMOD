package etomo.storage;

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
public final class DirectiveDescrEtomoColumn {
  public static final String rcsid = "$Id:$";

  public static final DirectiveDescrEtomoColumn NE = new DirectiveDescrEtomoColumn("NE");
  static final DirectiveDescrEtomoColumn NES = new DirectiveDescrEtomoColumn("NES");
  public static final DirectiveDescrEtomoColumn SD = new DirectiveDescrEtomoColumn("SD");
  public static final DirectiveDescrEtomoColumn SO = new DirectiveDescrEtomoColumn("SO");

  private final String tag;

  public String toString() {
    return tag;
  }

  private DirectiveDescrEtomoColumn(final String tag) {
    this.tag = tag;
  }

  static DirectiveDescrEtomoColumn getInstance(String input) {
    if (input.equals(NE.tag)) {
      return NE;
    }
    if (input.equals(NES.tag)) {
      return NES;
    }
    if (input.equals(SD.tag)) {
      return SD;
    }
    if (input.equals(SO.tag)) {
      return SO;
    }
    return null;
  }
}