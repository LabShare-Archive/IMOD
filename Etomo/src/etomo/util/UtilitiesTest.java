package etomo.util;

import junit.framework.TestCase;
/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2005</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
*/
public class UtilitiesTest extends TestCase {
  public static  final String  rcsid =  "$Id$";

  public final void test_round() {
    int significantDigits = 6;
    //returns values it can't round unchanged
    assertTrue(Utilities.round(0, significantDigits) == 0);
    assertTrue(Double.isInfinite(Utilities.round(Double.NEGATIVE_INFINITY, significantDigits)));
    assertTrue(Double.isInfinite(Utilities.round(Double.POSITIVE_INFINITY, significantDigits)));
    assertTrue(Double.isNaN(Utilities.round(Double.NaN, significantDigits)));
    assertTrue(Utilities.round(123456789, 0) == 123456789);
    assertTrue(Utilities.round(123456789, -3) == 123456789);
    //return values with significant digits <= significantDigits unchanged
    assertTrue(Utilities.round(1.1, significantDigits) == 1.1);
    assertTrue(Utilities.round(1.01, significantDigits) == 1.01);
    assertTrue(Utilities.round(1.001, significantDigits) == 1.001);
    assertTrue(Utilities.round(1.0001, significantDigits) == 1.0001);
    assertTrue(Utilities.round(1.00001, significantDigits) == 1.00001);
    //rounds to significantDigits
    assertTrue(Utilities.round(1.000001, significantDigits) == 1);
    assertTrue(Utilities.round(1.0000001, significantDigits) == 1);
    assertTrue(Utilities.round(1.00000001, significantDigits) == 1);
    assertTrue(Utilities.round(1.000000001, significantDigits) == 1);
    //return values with significant digits <= significantDigits unchanged
    assertTrue(Utilities.round(1234560000., significantDigits) == 1234560000);
    assertTrue(Utilities.round(123456000., significantDigits) == 123456000);
    assertTrue(Utilities.round(12345600., significantDigits) == 12345600);
    assertTrue(Utilities.round(1234560., significantDigits) == 1234560);
    assertTrue(Utilities.round(123456., significantDigits) == 123456);
    assertTrue(Utilities.round(12345.6, significantDigits) == 12345.6);
    assertTrue(Utilities.round(1234.56, significantDigits) == 1234.56);
    assertTrue(Utilities.round(123.456, significantDigits) == 123.456);
    assertTrue(Utilities.round(12.3456, significantDigits) == 12.3456);
    assertTrue(Utilities.round(1.23456, significantDigits) == 1.23456);
    System.out.println();
    assertTrue(Utilities.round(.123456, significantDigits) == .123456);
    System.out.println();
    assertTrue(Utilities.round(.123456, significantDigits) == .123456);
    assertTrue(Utilities.round(.0123456, significantDigits) == .0123456);
    assertTrue(Utilities.round(.00123456, significantDigits) == .00123456);
    assertTrue(Utilities.round(.000123456, significantDigits) == .000123456);
    assertTrue(Utilities.round(.0000123456, significantDigits) == .0000123456);
    //rounds to significantDigits
    assertTrue(Utilities.round(1234567890000., significantDigits) == 1234570000000.);
    assertTrue(Utilities.round(123456789000., significantDigits) == 123457000000.);
    assertTrue(Utilities.round(12345678900., significantDigits) == 12345700000.);
    assertTrue(Utilities.round(1234567890., significantDigits) == 1234570000);
    assertTrue(Utilities.round(123456789., significantDigits) == 123457000);
    assertTrue(Utilities.round(12345678.9, significantDigits) == 12345700);
    assertTrue(Utilities.round(1234567.89, significantDigits) == 1234570);
    assertTrue(Utilities.round(123456.789, significantDigits) == 123457);
    assertTrue(Utilities.round(12345.6789, significantDigits) == 12345.7);
    assertTrue(Utilities.round(1234.56789, significantDigits) == 1234.57);
    assertTrue(Utilities.round(123.456789, significantDigits) == 123.457);
    assertTrue(Utilities.round(12.3456789, significantDigits) == 12.3457);
    assertTrue(Utilities.round(1.23456789, significantDigits) == 1.23457);
    assertTrue(Utilities.round(.123456789, significantDigits) == .123457);
    assertTrue(Utilities.round(.0123456789, significantDigits) == .0123457);
    assertTrue(Utilities.round(.00123456789, significantDigits) == .00123457);
    assertTrue(Utilities.round(.000123456789, significantDigits) == .000123457);
    assertTrue(Utilities.round(1234560010000., significantDigits) == 1234560000000.);
    assertTrue(Utilities.round(123456001000., significantDigits) == 123456000000.);
    assertTrue(Utilities.round(12345600100., significantDigits) == 12345600000.);
    assertTrue(Utilities.round(1234560010., significantDigits) == 1234560000);
    assertTrue(Utilities.round(123456001., significantDigits) == 123456000);
    assertTrue(Utilities.round(12345600.1, significantDigits) == 12345600);
    assertTrue(Utilities.round(1234560.01, significantDigits) == 1234560);
    assertTrue(Utilities.round(123456.001, significantDigits) == 123456);
    assertTrue(Utilities.round(12345.6001, significantDigits) == 12345.6);
    assertTrue(Utilities.round(1234.56001, significantDigits) == 1234.56);
    assertTrue(Utilities.round(123.456001, significantDigits) == 123.456);
    assertTrue(Utilities.round(12.3456001, significantDigits) == 12.3456);
    assertTrue(Utilities.round(1.23456001, significantDigits) == 1.23456);
    assertTrue(Utilities.round(.123456001, significantDigits) == .123456);
    assertTrue(Utilities.round(.0123456001, significantDigits) == .0123456);
    assertTrue(Utilities.round(.00123456001, significantDigits) == .00123456);
    assertTrue(Utilities.round(.000123456001, significantDigits) == .000123456);
  }
}
/**
* <p> $Log$ </p>
*/