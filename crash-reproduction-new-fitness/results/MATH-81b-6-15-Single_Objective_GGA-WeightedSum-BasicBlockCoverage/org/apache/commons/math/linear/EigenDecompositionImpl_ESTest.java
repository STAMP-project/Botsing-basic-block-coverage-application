/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 11:49:26 UTC 2020
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
      doubleArray0[0] = (-1184.138566313823);
      doubleArray0[1] = (-2500.72146736849);
      doubleArray0[2] = (-2500.72146736849);
      doubleArray0[3] = (-2500.72146736849);
      doubleArray0[4] = (-2500.72146736849);
      double double0 = (-1181.049920703656);
      double double1 = 201256.69214263672;
      double[] doubleArray1 = new double[4];
      doubleArray1[0] = 2420.945;
      doubleArray1[1] = 1076.73471001457;
      doubleArray1[2] = 2420.945;
      doubleArray1[1] = 201256.69214263672;
      EigenDecompositionImpl eigenDecompositionImpl0 = new EigenDecompositionImpl(doubleArray0, doubleArray1, (-2500.72146736849));
  }
}
