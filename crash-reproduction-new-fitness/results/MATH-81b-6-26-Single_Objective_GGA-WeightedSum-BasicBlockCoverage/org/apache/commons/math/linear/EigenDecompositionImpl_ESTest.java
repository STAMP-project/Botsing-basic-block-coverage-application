/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 16:12:51 UTC 2020
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
      Double[] doubleArray0 = new Double[10];
      Double double0 = new Double(0.17423143393723572);
      double[] doubleArray1 = new double[8];
      doubleArray1[0] = (-1532.0);
      doubleArray1[1] = 0.17423143393723572;
      doubleArray1[2] = 0.17648921767524842;
      doubleArray1[3] = 663.8987;
      doubleArray1[4] = 0.17423143393723572;
      doubleArray1[5] = 2664.3882310677;
      doubleArray1[6] = 0.17648921767524842;
      doubleArray1[7] = 0.17423143393723572;
      double[] doubleArray2 = new double[7];
      doubleArray2[0] = 2664.3882310677;
      doubleArray2[1] = 0.17423143393723572;
      doubleArray2[2] = (-1532.0);
      doubleArray2[3] = (-1878.6128950425916);
      doubleArray2[4] = (-1532.0);
      doubleArray2[5] = (-1878.6128950425916);
      doubleArray2[6] = 2664.3882310677;
      EigenDecompositionImpl eigenDecompositionImpl0 = new EigenDecompositionImpl(doubleArray1, doubleArray2, (-1532.0));
  }
}
