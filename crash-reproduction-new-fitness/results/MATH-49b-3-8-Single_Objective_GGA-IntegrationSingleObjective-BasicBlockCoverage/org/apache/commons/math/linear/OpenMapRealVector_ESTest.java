/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 22:14:41 UTC 2020
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
      double[] doubleArray0 = new double[13];
      doubleArray0[0] = 1.0;
      doubleArray0[1] = 0.0;
      doubleArray0[2] = 106.15;
      doubleArray0[3] = Double.NEGATIVE_INFINITY;
      doubleArray0[4] = 39.871;
      doubleArray0[5] = (-995.71);
      doubleArray0[6] = (-995.71);
      doubleArray0[7] = 0.0;
      doubleArray0[8] = 0.0;
      OpenMapRealVector openMapRealVector0 = new OpenMapRealVector(doubleArray0, 39.871);
      OpenMapRealVector openMapRealVector1 = openMapRealVector0.projection(doubleArray0);
      openMapRealVector1.mapDivide(57.529297179);
      OpenMapRealVector openMapRealVector2 = openMapRealVector0.ebeMultiply(doubleArray0);
      OpenMapRealVector openMapRealVector3 = openMapRealVector0.mapAddToSelf(106.15);
      OpenMapRealVector openMapRealVector4 = openMapRealVector3.subtract(openMapRealVector0);
      openMapRealVector3.getDistance(doubleArray0);
      OpenMapRealVector openMapRealVector5 = openMapRealVector4.ebeMultiply((RealVector) openMapRealVector0);
      openMapRealVector5.subtract((RealVector) openMapRealVector3);
      // Undeclared exception!
      openMapRealVector0.ebeMultiply((RealVector) openMapRealVector2);
  }
}
