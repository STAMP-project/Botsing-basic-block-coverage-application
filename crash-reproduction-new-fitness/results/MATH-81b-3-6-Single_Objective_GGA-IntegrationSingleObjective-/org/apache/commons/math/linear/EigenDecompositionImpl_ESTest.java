/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 11:46:57 UTC 2020
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
      double[] doubleArray0 = new double[3];
      doubleArray0[0] = 205.2925854232999;
      double double0 = 2.6434028644834904E10;
      double[] doubleArray1 = new double[5];
      double[] doubleArray2 = new double[4];
      doubleArray2[0] = 0.0;
      doubleArray2[1] = 4.0;
      doubleArray2[2] = 205.2925854232999;
      doubleArray2[3] = (-192.8467461252);
      EigenDecompositionImpl eigenDecompositionImpl0 = new EigenDecompositionImpl(doubleArray1, doubleArray2, 0.0);
  }
}
