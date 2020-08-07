/*
 * This file was automatically generated by EvoSuite
 * Mon Mar 30 14:47:44 UTC 2020
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
      double[] doubleArray1 = new double[8];
      doubleArray1[5] = (-3260.1);
      doubleArray1[6] = (-453613.0);
      doubleArray1[7] = (-453613.0);
      EigenDecompositionImpl eigenDecompositionImpl0 = new EigenDecompositionImpl(doubleArray0, doubleArray1, (-3260.1));
  }
}
