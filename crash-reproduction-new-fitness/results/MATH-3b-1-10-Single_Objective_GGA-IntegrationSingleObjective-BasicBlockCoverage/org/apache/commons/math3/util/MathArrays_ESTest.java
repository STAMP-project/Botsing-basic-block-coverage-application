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
      double[] doubleArray0 = new double[5];
      doubleArray0[0] = 1.0;
      doubleArray0[1] = 0.0;
      doubleArray0[2] = 1878.97;
      doubleArray0[3] = (-590.837521399);
      double[] doubleArray1 = MathArrays.ebeDivide(doubleArray0, doubleArray0);
      double[] doubleArray2 = MathArrays.copyOf(doubleArray1);
      MathArrays.linearCombination(doubleArray2, doubleArray1);
      double[] doubleArray3 = new double[1];
      doubleArray3[0] = (-590.837521399);
      // Undeclared exception!
      MathArrays.linearCombination(doubleArray3, doubleArray3);
  }
}
