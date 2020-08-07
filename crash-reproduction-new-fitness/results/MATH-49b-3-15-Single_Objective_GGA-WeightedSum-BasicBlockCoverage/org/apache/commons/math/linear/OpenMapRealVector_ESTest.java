/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 21:38:51 UTC 2020
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
      Double[] doubleArray0 = new Double[2];
      Double double0 = Double.valueOf((-0.6153103517275761));
      doubleArray0[0] = double0;
      Double double1 = new Double((double) doubleArray0[0]);
      doubleArray0[1] = double0;
      OpenMapRealVector openMapRealVector0 = new OpenMapRealVector(doubleArray0, 0.0);
      OpenMapRealVector openMapRealVector1 = openMapRealVector0.subtract(openMapRealVector0);
      OpenMapRealVector openMapRealVector2 = openMapRealVector0.ebeMultiply((RealVector) openMapRealVector1);
      openMapRealVector1.sparseIterator();
      int int0 = (-1026);
      double double2 = 0.06750710874566598;
      OpenMapRealVector openMapRealVector3 = new OpenMapRealVector(doubleArray0, 0.06750710874566598);
      // Undeclared exception!
      openMapRealVector3.ebeMultiply((RealVector) openMapRealVector2);
  }
}
