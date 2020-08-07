/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 16:14:06 UTC 2020
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
      Double double0 = new Double((-9.204353207992714));
      double[] doubleArray0 = new double[8];
      doubleArray0[0] = (double) 37;
      doubleArray0[1] = (double) 37;
      doubleArray0[3] = (-9.204353207992714);
      doubleArray0[4] = (double) 37;
      doubleArray0[2] = (-9.204353207992714);
      double double1 = 3297.73;
      double[] doubleArray1 = new double[7];
      doubleArray1[0] = (double) 37;
      doubleArray1[1] = (-1578.7184109397);
      doubleArray1[2] = 3297.73;
      doubleArray1[3] = (-9.204353207992714);
      doubleArray1[4] = (double) 37;
      doubleArray1[5] = (-3445.64046);
      doubleArray1[6] = (double) 37;
      EigenDecompositionImpl eigenDecompositionImpl0 = new EigenDecompositionImpl(doubleArray0, doubleArray1, 84.76661377549043);
  }
}
