/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 11:44:42 UTC 2020
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
      double[] doubleArray0 = new double[7];
      doubleArray0[0] = (-2284.7);
      doubleArray0[1] = (-2284.7);
      doubleArray0[2] = 1755.0;
      double[] doubleArray1 = new double[6];
      doubleArray1[0] = 2.2250738585072014E-308;
      doubleArray1[1] = (-2284.7);
      doubleArray1[2] = 100.0;
      doubleArray1[3] = 1755.0;
      doubleArray1[4] = (-2729.9);
      EigenDecompositionImpl eigenDecompositionImpl0 = new EigenDecompositionImpl(doubleArray0, doubleArray1, (-2820.70056542));
      EigenDecompositionImpl eigenDecompositionImpl1 = new EigenDecompositionImpl(doubleArray0, doubleArray1, 3482.957411);
  }
}
