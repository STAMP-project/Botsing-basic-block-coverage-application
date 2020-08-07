/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 11:44:53 UTC 2020
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
      double[] doubleArray0 = new double[5];
      doubleArray0[0] = (-4720.0104392578);
      doubleArray0[1] = (-4720.0104392578);
      doubleArray0[2] = (-4720.0104392578);
      doubleArray0[3] = 0.04690158944972961;
      doubleArray0[4] = (-4720.0104392578);
      double[] doubleArray1 = new double[4];
      doubleArray1[0] = 0.04690158944972961;
      doubleArray1[1] = (-4720.0104392578);
      doubleArray1[2] = 6.0;
      doubleArray1[3] = (-4720.0104392578);
      EigenDecompositionImpl eigenDecompositionImpl0 = new EigenDecompositionImpl(doubleArray0, doubleArray1, 1465.20006);
      eigenDecompositionImpl0.getD();
      EigenDecompositionImpl eigenDecompositionImpl1 = new EigenDecompositionImpl(doubleArray0, doubleArray1, 0.0);
  }
}
