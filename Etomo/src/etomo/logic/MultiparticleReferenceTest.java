package etomo.logic;

import etomo.type.EtomoNumber;
import junit.framework.TestCase;

/**
* <p>Description: Test of MultiparticleReference.</p>
* 
* <p>Copyright: Copyright 2012</p>
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
public class MultiparticleReferenceTest extends TestCase {
  public static final String rcsid = "$Id:$";

  public void testGetNumEntries() {
    assertEquals("k:  2   3      4     5     6     7      8      9     10",
        MultiparticleReference.getNumEntries(), 9);
  }

  public void testGetParticleCount() {
    assertEquals("result is corrected", 4, MultiparticleReference.getParticleCount(-1)
        .intValue());
    assertEquals("result is corrected", 1024, MultiparticleReference
        .getParticleCount(100).intValue());
    assertEquals("l^k", 8, MultiparticleReference.getParticleCount(1).intValue());
    assertEquals("l^k", 32, MultiparticleReference.getParticleCount(3).intValue());
    assertEquals("l^k", 512, MultiparticleReference.getParticleCount(7).intValue());
  }

  public void testGetDefaultIndex() {
    assertEquals("default is k = 5", MultiparticleReference.getDefaultIndex(), 3);
  }

  public void testConvertLevelToIndex() {
    EtomoNumber index = new EtomoNumber();
    MultiparticleReference.convertLevelToIndex("-1", index);
    assertEquals("result is corrected", 0, index.getInt());
    MultiparticleReference.convertLevelToIndex("100", index);
    assertEquals("result is corrected", 8, index.getInt());
    MultiparticleReference.convertLevelToIndex("4", index);
    assertEquals("k starts at 2", 2, index.getInt());
    MultiparticleReference.convertLevelToIndex("6", index);
    assertEquals("k starts at 2", 4, index.getInt());
    MultiparticleReference.convertLevelToIndex("8", index);
    assertEquals("k starts at 2", 6, index.getInt());

    assertEquals("result is corrected", 0, MultiparticleReference.convertLevelToIndex(-1));
    assertEquals("result is corrected", 8,
        MultiparticleReference.convertLevelToIndex(100));
    assertEquals("k starts at 2", 2, MultiparticleReference.convertLevelToIndex(4));
    assertEquals("k starts at 2", 4, MultiparticleReference.convertLevelToIndex(6));
    assertEquals("k starts at 2", 6, MultiparticleReference.convertLevelToIndex(8));
  }

  public void testConvertIndexToLevel() {
    assertEquals("result is corrected", "2",
        MultiparticleReference.convertIndexToLevel(-1));
    assertEquals("result is corrected", "10",
        MultiparticleReference.convertIndexToLevel(100));
    assertEquals("k starts at 2", "3", MultiparticleReference.convertIndexToLevel(1));
    assertEquals("k starts at 2", "5", MultiparticleReference.convertIndexToLevel(3));
    assertEquals("k starts at 2", "9", MultiparticleReference.convertIndexToLevel(7));
  }
}
