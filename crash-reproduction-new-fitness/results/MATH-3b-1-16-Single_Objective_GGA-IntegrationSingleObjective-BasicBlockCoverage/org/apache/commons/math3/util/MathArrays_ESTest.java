/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 02:32:30 UTC 2020
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
      long[][] longArray0 = new long[0][4];
      MathArrays.checkRectangular(longArray0);
      double[] doubleArray0 = new double[1];
      doubleArray0[0] = (-1612.28);
      // Undeclared exception!
      MathArrays.linearCombination(doubleArray0, doubleArray0);
  }
}
