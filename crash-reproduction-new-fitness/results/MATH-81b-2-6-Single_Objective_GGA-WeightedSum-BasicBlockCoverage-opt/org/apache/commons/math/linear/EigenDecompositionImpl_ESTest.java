/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 14:47:50 UTC 2021
 */

package org.apache.commons.math.linear;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.linear.ArrayRealVector;
import org.apache.commons.math.linear.EigenDecompositionImpl;
import org.apache.commons.math.linear.RealVector;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class EigenDecompositionImpl_ESTest extends EigenDecompositionImpl_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      ArrayRealVector arrayRealVector0 = new ArrayRealVector();
      arrayRealVector0.mapAcos();
      double[] doubleArray0 = new double[5];
      arrayRealVector0.getDistance((RealVector) arrayRealVector0);
      arrayRealVector0.mapMultiply(1.0);
      arrayRealVector0.append(215.25);
      doubleArray0[1] = 611.0;
      double[] doubleArray1 = new double[4];
      doubleArray1[0] = 1.0;
      doubleArray1[1] = (double) (-1911);
      doubleArray1[2] = (double) (-1911);
      doubleArray1[3] = 1420.12070047703;
      EigenDecompositionImpl eigenDecompositionImpl0 = new EigenDecompositionImpl(doubleArray0, doubleArray1, (-3976.6813985745175));
  }
}
