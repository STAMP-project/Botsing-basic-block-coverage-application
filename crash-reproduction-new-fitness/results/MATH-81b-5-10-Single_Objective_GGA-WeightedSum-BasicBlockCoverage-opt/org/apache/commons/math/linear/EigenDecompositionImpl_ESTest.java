/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 14:49:36 UTC 2021
 */

package org.apache.commons.math.linear;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.linear.Array2DRowRealMatrix;
import org.apache.commons.math.linear.DefaultRealMatrixPreservingVisitor;
import org.apache.commons.math.linear.EigenDecompositionImpl;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class EigenDecompositionImpl_ESTest extends EigenDecompositionImpl_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Array2DRowRealMatrix array2DRowRealMatrix0 = new Array2DRowRealMatrix();
      DefaultRealMatrixPreservingVisitor defaultRealMatrixPreservingVisitor0 = new DefaultRealMatrixPreservingVisitor();
      DefaultRealMatrixPreservingVisitor defaultRealMatrixPreservingVisitor1 = new DefaultRealMatrixPreservingVisitor();
      DefaultRealMatrixPreservingVisitor defaultRealMatrixPreservingVisitor2 = new DefaultRealMatrixPreservingVisitor();
      DefaultRealMatrixPreservingVisitor defaultRealMatrixPreservingVisitor3 = new DefaultRealMatrixPreservingVisitor();
      DefaultRealMatrixPreservingVisitor defaultRealMatrixPreservingVisitor4 = new DefaultRealMatrixPreservingVisitor();
      DefaultRealMatrixPreservingVisitor defaultRealMatrixPreservingVisitor5 = new DefaultRealMatrixPreservingVisitor();
      DefaultRealMatrixPreservingVisitor defaultRealMatrixPreservingVisitor6 = new DefaultRealMatrixPreservingVisitor();
      DefaultRealMatrixPreservingVisitor defaultRealMatrixPreservingVisitor7 = new DefaultRealMatrixPreservingVisitor();
      DefaultRealMatrixPreservingVisitor defaultRealMatrixPreservingVisitor8 = new DefaultRealMatrixPreservingVisitor();
      DefaultRealMatrixPreservingVisitor defaultRealMatrixPreservingVisitor9 = new DefaultRealMatrixPreservingVisitor();
      DefaultRealMatrixPreservingVisitor defaultRealMatrixPreservingVisitor10 = new DefaultRealMatrixPreservingVisitor();
      DefaultRealMatrixPreservingVisitor defaultRealMatrixPreservingVisitor11 = new DefaultRealMatrixPreservingVisitor();
      DefaultRealMatrixPreservingVisitor defaultRealMatrixPreservingVisitor12 = new DefaultRealMatrixPreservingVisitor();
      double[] doubleArray0 = new double[5];
      doubleArray0[0] = 936.13195911575;
      doubleArray0[1] = (-1273.0);
      doubleArray0[2] = 1572.7985054;
      doubleArray0[3] = 501.5067166159049;
      doubleArray0[4] = 1572.7985054;
      double[] doubleArray1 = new double[4];
      doubleArray1[0] = 0.9999999999999998;
      doubleArray1[1] = (-1273.0);
      doubleArray1[2] = (-1273.0);
      doubleArray1[3] = (-1273.0);
      EigenDecompositionImpl eigenDecompositionImpl0 = new EigenDecompositionImpl(doubleArray0, doubleArray1, (-1273.0));
  }
}
