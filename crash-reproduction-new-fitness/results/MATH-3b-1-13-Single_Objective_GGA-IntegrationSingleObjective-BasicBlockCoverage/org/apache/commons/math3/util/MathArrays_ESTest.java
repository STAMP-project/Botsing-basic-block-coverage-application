/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 22:10:27 UTC 2020
 */

package org.apache.commons.math3.util;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math3.util.MathArrays;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class MathArrays_ESTest extends MathArrays_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      double[] doubleArray0 = new double[2];
      doubleArray0[0] = 0.0;
      doubleArray0[1] = (-1.0);
      MathArrays.linearCombination(doubleArray0, doubleArray0);
      MathArrays.OrderDirection mathArrays_OrderDirection0 = MathArrays.OrderDirection.DECREASING;
      boolean boolean0 = false;
      MathArrays.checkOrder(doubleArray0, mathArrays_OrderDirection0, false, false);
      MathArrays.linearCombination(doubleArray0, doubleArray0);
      MathArrays.distanceInf(doubleArray0, doubleArray0);
      MathArrays.distance(doubleArray0, doubleArray0);
      int int0 = 33;
      MathArrays.copyOf(doubleArray0, 33);
      double[] doubleArray1 = new double[1];
      doubleArray1[0] = 0.0;
      // Undeclared exception!
      MathArrays.linearCombination(doubleArray1, doubleArray1);
  }
}
