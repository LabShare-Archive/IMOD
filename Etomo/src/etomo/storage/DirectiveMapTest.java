package etomo.storage;

import java.util.NoSuchElementException;

import junit.framework.TestCase;

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
public class DirectiveMapTest extends TestCase {
  public static final String rcsid = "$Id:$";

  private static final String COM_PARAM_A = "comparam.eraser.ccderaser.DiffCriterion";
  private static final String COM_PARAM_B = "comparam.eraser.ccderaser.MaximumRadius";
  private static final String COM_PARAM_C = "comparam.eraser.ccderaser.ModelFile";
  private static final String COM_PARAM_D = "comparam.eraser.ccderaser.PeakCriterion";
  private static final String SETUP_SET_A = "setupset.copyarg.dual";
  private static final String SETUP_SET_B = "setupset.copyarg.name";

  private final DirectiveMap map = new DirectiveMap();

  public DirectiveMapTest(String name) {
    super(name);
  }

  public void testSortedMap() {
    DirectiveName directiveName = new DirectiveName();
    directiveName.setKey(COM_PARAM_C);
    Directive directive = new Directive(directiveName);
    map.put(directive.getKey(), directive);

    DirectiveMap.Iterator iterator = map.keySet(DirectiveType.COM_PARAM).iterator();
    assertTrue("1 comparam directive was added to map", iterator.hasNext());
    assertEquals("hasNext doesn't increment", COM_PARAM_C, iterator.next());

    directiveName.setKey(COM_PARAM_B);
    directive = new Directive(directiveName);
    map.put(directive.getKey(), directive);
    directiveName.setKey(SETUP_SET_B);
    directive = new Directive(directiveName);
    map.put(directive.getKey(), directive);
    directiveName.setKey(COM_PARAM_D);
    directive = new Directive(directiveName);
    map.put(directive.getKey(), directive);
    directiveName.setKey(COM_PARAM_A);
    directive = new Directive(directiveName);
    map.put(directive.getKey(), directive);
    directiveName.setKey(SETUP_SET_A);
    directive = new Directive(directiveName);
    map.put(directive.getKey(), directive);

    iterator = map.keySet(DirectiveType.COM_PARAM).iterator();
    for (int i = 0; i < 10; i++) {
      assertTrue("comparam directives where added to map and hasNext doesn't increment",
          iterator.hasNext());
    }
    assertEquals("map is sorted", COM_PARAM_A, iterator.next());
    assertEquals("next increments", COM_PARAM_B, iterator.next());
    assertTrue("4 comparam directives where added to map", iterator.hasNext());
    assertEquals("hasNext doesn't increment", COM_PARAM_C, iterator.next());
    assertTrue("hasNext sees the last comparam directive", iterator.hasNext());
    assertEquals("next retrieves the last comparam directive", COM_PARAM_D,
        iterator.next());
    assertFalse("no more comparam directives", iterator.hasNext());

    iterator = map.keySet(DirectiveType.SETUP_SET).iterator();
    assertEquals("map is sorted", SETUP_SET_A, iterator.next());
    assertTrue("hasNext sees the last setupset directive", iterator.hasNext());
    assertEquals("next retrieves the last comparam directive", SETUP_SET_B,
        iterator.next());
    assertFalse("no more setupset directives", iterator.hasNext());
    assertFalse("no more setupset directives", iterator.hasNext());
    try {
      iterator.next();
      fail("nothing left to retrieve");
    }
    catch (NoSuchElementException e) {
    }

    iterator = map.keySet(DirectiveType.RUNTIME).iterator();
    try {
      iterator.next();
      fail("nothing left to retrieve");
    }
    catch (NoSuchElementException e) {
    }
    assertFalse("no runtime directives", iterator.hasNext());

    iterator = map.keySet(null).iterator();
    assertEquals("map is sorted", COM_PARAM_A, iterator.next());
    assertTrue("more directives", iterator.hasNext());
    assertEquals("map is sorted, hasNext does not increment", COM_PARAM_B,
        iterator.next());
    assertEquals("map is sorted", COM_PARAM_C, iterator.next());
    assertEquals("map is sorted", COM_PARAM_D, iterator.next());
    assertEquals("all directives are in this iterator", SETUP_SET_A, iterator.next());
    assertEquals("map is sorted", SETUP_SET_B, iterator.next());
    try {
      iterator.next();
      fail("nothing left to retrieve");
    }
    catch (NoSuchElementException e) {
    }
  }
}
