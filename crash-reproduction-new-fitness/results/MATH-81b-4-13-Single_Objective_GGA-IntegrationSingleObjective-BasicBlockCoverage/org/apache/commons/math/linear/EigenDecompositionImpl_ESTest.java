/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 11:48:15 UTC 2020
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
      double[] doubleArray0 = new double[4];
      doubleArray0[0] = 22.19250822519072;
      double double0 = (-3442.1191392167);
      doubleArray0[2] = (-3442.1191392167);
      doubleArray0[2] = 1.1;
      double[] doubleArray1 = new double[3];
      doubleArray1[0] = 1.1;
      doubleArray1[1] = (-3442.1191392167);
      doubleArray1[2] = 1.1;
      EigenDecompositionImpl eigenDecompositionImpl0 = new EigenDecompositionImpl(doubleArray0, doubleArray1, 942.3);
  }
}
