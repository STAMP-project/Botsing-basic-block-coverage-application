/*
 * This file was automatically generated by EvoSuite
 * Mon Mar 30 14:59:56 UTC 2020
 */

package org.apache.commons.math.linear;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.linear.EigenDecompositionImpl;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class EigenDecompositionImpl_ESTest extends EigenDecompositionImpl_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      double[] doubleArray0 = new double[8];
      doubleArray0[1] = (-0.7959188811121187);
      doubleArray0[3] = (-0.7959188811121187);
      doubleArray0[6] = (-0.051492);
      doubleArray0[7] = (-0.7959188811121187);
      double[] doubleArray1 = new double[7];
      doubleArray1[0] = (-0.7959188811121187);
      doubleArray1[2] = (-0.7959188811121187);
      doubleArray1[3] = (-0.051492);
      doubleArray1[4] = (-0.7959188811121187);
      doubleArray1[5] = (-0.051492);
      doubleArray1[1] = (-0.051492);
      EigenDecompositionImpl eigenDecompositionImpl0 = new EigenDecompositionImpl(doubleArray0, doubleArray1, (-0.7959188811121187));
  }
}
