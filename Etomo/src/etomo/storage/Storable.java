package etomo.storage;

import java.util.Properties;

/*
 * <p>Description: Defines the methods necessary to save and store data objects</p>
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
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */

public interface Storable {
  /**
   * The store methods expects the object to add the key/element pairs to the
   * Properties object passed.
   */
  public void store(Properties props);

  /**
   * The store methods expects the object to add the key/element pairs to the
   * Properties object passed, prepending the string suuplied to the key.
   */
  public void store(Properties props, String prepend);

  /**
   * The load method provides a loaded Properties object so that the object can
   * get its stored data through the getProperty call
   */
  public void load(Properties props);

  /**
   * The load method provides a loaded Properties object so that the object can
   * get its stored data through the getProperty call.  The prepend string
   * tells the loading object what to prepend to the key to find its data.
   */
  public void load(Properties props, String prepend);

}
