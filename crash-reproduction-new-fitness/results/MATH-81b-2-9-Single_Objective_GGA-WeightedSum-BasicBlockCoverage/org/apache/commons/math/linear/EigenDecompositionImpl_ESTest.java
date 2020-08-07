/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 11:46:22 UTC 2020
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
      double[] doubleArray0 = new double[8];
      doubleArray0[1] = (-1.577308848500529);
      doubleArray0[2] = (-1.577308848500529);
      doubleArray0[3] = 1513.172;
      doubleArray0[4] = (-417.2);
      doubleArray0[6] = (-1.577308848500529);
      doubleArray0[7] = (-755.078067624695);
      double[] doubleArray1 = new double[7];
      doubleArray1[1] = (-755.078067624695);
      doubleArray1[2] = (-1.577308848500529);
      doubleArray1[3] = (-1.577308848500529);
      doubleArray1[4] = (-192.939);
      doubleArray1[5] = (-755.078067624695);
      doubleArray1[6] = (-1.577308848500529);
      EigenDecompositionImpl eigenDecompositionImpl0 = new EigenDecompositionImpl(doubleArray0, doubleArray1, (-417.2));
  }
}
