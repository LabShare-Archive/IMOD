package etomo.storage.autodoc;
/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2006</p>
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
* <p> Revision 1.1  2006/04/25 18:55:05  sueh
* <p> bug# 787 Added ReadOnlyNameValuePairList so that name/value
* <p> pairs can be read from either a global or section area using the same
* <p> code.
* <p> </p>
*/
public interface ReadOnlyStatementList {
  public static  final String  rcsid =  "$Id$";
  
  public String getString();
  public StatementLocation getStatementLocation();
  public Statement nextStatement(StatementLocation location);
  public String getName();
}
/**
* <p> $Log$
* <p> Revision 1.1  2006/04/25 18:55:05  sueh
* <p> bug# 787 Added ReadOnlyNameValuePairList so that name/value
* <p> pairs can be read from either a global or section area using the same
* <p> code.
* <p> </p>
*/