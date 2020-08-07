/*
 * This file was automatically generated by EvoSuite
 * Tue Mar 31 10:25:53 UTC 2020
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
      Double[] doubleArray0 = new Double[5];
      Double double0 = new Double(0.0);
      doubleArray0[0] = double0;
      Double double1 = new Double((-322.64));
      doubleArray0[1] = double1;
      doubleArray0[2] = double1;
      Double double2 = new Double(0.7937005259840998);
      doubleArray0[3] = double2;
      doubleArray0[4] = doubleArray0[2];
      OpenMapRealVector openMapRealVector0 = new OpenMapRealVector(doubleArray0, (-322.64));
      RealVector realVector0 = openMapRealVector0.mapSubtract((double) doubleArray0[3]);
      OpenMapRealVector openMapRealVector1 = new OpenMapRealVector(doubleArray0);
      // Undeclared exception!
      openMapRealVector1.ebeMultiply(realVector0);
  }
}
