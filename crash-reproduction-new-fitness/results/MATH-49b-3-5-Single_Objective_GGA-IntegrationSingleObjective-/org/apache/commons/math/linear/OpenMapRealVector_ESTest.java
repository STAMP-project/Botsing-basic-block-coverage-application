/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 22:18:44 UTC 2020
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
      double[] doubleArray0 = new double[3];
      doubleArray0[0] = 1.1102230246251565E-16;
      doubleArray0[1] = (-1039.4973133860713);
      doubleArray0[2] = 1.1102230246251565E-16;
      OpenMapRealVector openMapRealVector0 = new OpenMapRealVector(doubleArray0, 1.1102230246251565E-16);
      OpenMapRealVector openMapRealVector1 = new OpenMapRealVector(openMapRealVector0);
      // Undeclared exception!
      openMapRealVector1.ebeMultiply((RealVector) openMapRealVector0);
  }
}
