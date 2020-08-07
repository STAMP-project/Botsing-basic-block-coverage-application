/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 11:49:36 UTC 2020
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
      doubleArray0[0] = (-0.274156535188806);
      doubleArray0[1] = (-3100.752435135136);
      doubleArray0[2] = (-0.274156535188806);
      doubleArray0[3] = (-0.274156535188806);
      doubleArray0[4] = (-0.274156535188806);
      doubleArray0[5] = (-0.274156535188806);
      doubleArray0[6] = (-3100.752435135136);
      double[] doubleArray1 = new double[8];
      doubleArray1[1] = (-3100.752435135136);
      doubleArray1[2] = (-3100.752435135136);
      doubleArray1[3] = (-3100.752435135136);
      doubleArray1[4] = (-0.274156535188806);
      EigenDecompositionImpl eigenDecompositionImpl0 = new EigenDecompositionImpl(doubleArray1, doubleArray0, (-1279.359325661));
  }
}
