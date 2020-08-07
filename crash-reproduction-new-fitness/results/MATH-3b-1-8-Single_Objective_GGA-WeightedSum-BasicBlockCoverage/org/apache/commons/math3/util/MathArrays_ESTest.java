/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 21:30:52 UTC 2020
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
      double[] doubleArray0 = new double[1];
      doubleArray0[0] = 527.5;
      MathArrays.scaleInPlace(527.5, doubleArray0);
      float[] floatArray0 = new float[6];
      floatArray0[0] = 699.6F;
      floatArray0[1] = (-1644.804F);
      floatArray0[2] = 180.85F;
      floatArray0[3] = (-1644.804F);
      floatArray0[4] = 180.85F;
      floatArray0[5] = 1960.0F;
      MathArrays.equalsIncludingNaN(floatArray0, floatArray0);
      // Undeclared exception!
      MathArrays.linearCombination(doubleArray0, doubleArray0);
  }
}
