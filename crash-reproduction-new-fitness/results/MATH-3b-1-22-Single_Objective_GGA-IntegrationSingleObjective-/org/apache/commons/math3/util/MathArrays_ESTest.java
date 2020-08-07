/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 02:38:22 UTC 2020
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
      MathArrays.linearCombination(0.0, 6.283185307179586, 0.0, 0.0);
      double[] doubleArray0 = null;
      double[] doubleArray1 = new double[1];
      doubleArray1[0] = 0.0;
      // Undeclared exception!
      MathArrays.linearCombination(doubleArray1, doubleArray1);
  }
}
