/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 11:49:50 UTC 2020
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
      doubleArray0[0] = (-329.13);
      doubleArray0[2] = 0.0;
      doubleArray0[3] = 2163.7861494124;
      doubleArray0[8] = 3146.657;
      doubleArray0[5] = 0.3010299956639812;
      doubleArray0[8] = 4.0;
      double[] doubleArray1 = new double[8];
      doubleArray1[0] = (-1518.3);
      doubleArray1[1] = (-2.5584599522535876);
      doubleArray1[2] = 0.3010299956639812;
      doubleArray1[3] = 3146.657;
      doubleArray1[4] = (-1518.3);
      doubleArray1[5] = 1597.54896504357;
      doubleArray1[6] = 2163.7861494124;
      EigenDecompositionImpl eigenDecompositionImpl0 = new EigenDecompositionImpl(doubleArray0, doubleArray1, 2163.7861494124);
  }
}
