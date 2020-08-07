/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 02:47:21 UTC 2020
 */

package org.apache.commons.math.linear;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.linear.OpenMapRealVector;
import org.apache.commons.math.linear.RealVector;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class OpenMapRealVector_ESTest extends OpenMapRealVector_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Double[] doubleArray0 = new Double[12];
      double[] doubleArray1 = new double[5];
      doubleArray1[0] = 3419.4995;
      doubleArray1[1] = (-1024.1920497993697);
      doubleArray1[2] = (-1679.6484281166374);
      doubleArray1[3] = 866.76386957918;
      doubleArray1[4] = (-1481.79);
      OpenMapRealVector openMapRealVector0 = new OpenMapRealVector(doubleArray1, 5128413.890199895);
      OpenMapRealVector openMapRealVector1 = new OpenMapRealVector((RealVector) openMapRealVector0);
      openMapRealVector1.combineToSelf(6436671.693944757, 6436671.693944757, doubleArray1);
      // Undeclared exception!
      openMapRealVector1.ebeMultiply((RealVector) openMapRealVector0);
  }
}
