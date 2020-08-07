/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 11:47:47 UTC 2020
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
      double[] doubleArray0 = new double[9];
      doubleArray0[0] = 1.175201193643801;
      double[] doubleArray1 = new double[8];
      doubleArray1[0] = (-3590.94150702117);
      doubleArray1[1] = 1.175201193643801;
      doubleArray1[2] = 1.175201193643801;
      doubleArray1[3] = 1.175201193643801;
      doubleArray1[4] = (-3590.94150702117);
      doubleArray1[5] = 1.175201193643801;
      doubleArray1[6] = 1.175201193643801;
      doubleArray1[7] = (-3590.94150702117);
      EigenDecompositionImpl eigenDecompositionImpl0 = new EigenDecompositionImpl(doubleArray0, doubleArray1, (-3590.94150702117));
  }
}
