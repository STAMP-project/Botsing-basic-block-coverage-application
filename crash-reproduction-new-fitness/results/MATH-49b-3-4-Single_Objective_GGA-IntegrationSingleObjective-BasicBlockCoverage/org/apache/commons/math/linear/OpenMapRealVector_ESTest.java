/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 22:14:23 UTC 2020
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
      double[] doubleArray0 = new double[2];
      doubleArray0[0] = 4421.9952;
      doubleArray0[1] = (-3870.262904315);
      OpenMapRealVector openMapRealVector0 = new OpenMapRealVector(doubleArray0, 4421.9952);
      openMapRealVector0.getL1Norm();
      openMapRealVector0.getNorm();
      OpenMapRealVector openMapRealVector1 = new OpenMapRealVector(doubleArray0, 593.261);
      openMapRealVector1.getL1Norm();
      openMapRealVector0.getNorm();
      OpenMapRealVector openMapRealVector2 = new OpenMapRealVector(openMapRealVector0);
      OpenMapRealVector openMapRealVector3 = openMapRealVector0.mapAddToSelf(8292.258104315);
      openMapRealVector3.getL1Norm();
      openMapRealVector3.dotProduct(doubleArray0);
      openMapRealVector0.getL1Norm();
      OpenMapRealVector openMapRealVector4 = openMapRealVector0.ebeMultiply((RealVector) openMapRealVector3);
      openMapRealVector3.setEntry(0, 1172.417448254021);
      // Undeclared exception!
      openMapRealVector4.ebeMultiply((RealVector) openMapRealVector0);
  }
}
