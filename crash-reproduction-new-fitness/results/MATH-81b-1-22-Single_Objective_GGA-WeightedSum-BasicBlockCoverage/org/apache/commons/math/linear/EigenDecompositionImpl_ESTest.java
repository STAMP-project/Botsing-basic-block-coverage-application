/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 16:11:17 UTC 2020
 */

package org.apache.commons.math.linear;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.linear.Array2DRowRealMatrix;
import org.apache.commons.math.linear.DefaultRealMatrixChangingVisitor;
import org.apache.commons.math.linear.EigenDecompositionImpl;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class EigenDecompositionImpl_ESTest extends EigenDecompositionImpl_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Array2DRowRealMatrix array2DRowRealMatrix0 = new Array2DRowRealMatrix();
      DefaultRealMatrixChangingVisitor defaultRealMatrixChangingVisitor0 = new DefaultRealMatrixChangingVisitor();
      DefaultRealMatrixChangingVisitor defaultRealMatrixChangingVisitor1 = new DefaultRealMatrixChangingVisitor();
      DefaultRealMatrixChangingVisitor defaultRealMatrixChangingVisitor2 = new DefaultRealMatrixChangingVisitor();
      DefaultRealMatrixChangingVisitor defaultRealMatrixChangingVisitor3 = new DefaultRealMatrixChangingVisitor();
      DefaultRealMatrixChangingVisitor defaultRealMatrixChangingVisitor4 = new DefaultRealMatrixChangingVisitor();
      DefaultRealMatrixChangingVisitor defaultRealMatrixChangingVisitor5 = new DefaultRealMatrixChangingVisitor();
      DefaultRealMatrixChangingVisitor defaultRealMatrixChangingVisitor6 = new DefaultRealMatrixChangingVisitor();
      double[] doubleArray0 = new double[7];
      doubleArray0[0] = 127.848018242414;
      doubleArray0[1] = 2328.0395439613;
      doubleArray0[2] = 127.848018242414;
      doubleArray0[4] = 127.848018242414;
      doubleArray0[5] = (-2973.672232495683);
      doubleArray0[6] = 127.848018242414;
      double[] doubleArray1 = new double[6];
      doubleArray1[0] = (-2973.672232495683);
      doubleArray0[4] = (-2973.672232495683);
      doubleArray1[2] = 127.848018242414;
      doubleArray1[3] = 2328.0395439613;
      doubleArray1[4] = (-2973.672232495683);
      EigenDecompositionImpl eigenDecompositionImpl0 = new EigenDecompositionImpl(doubleArray0, doubleArray1, (-2973.672232495683));
      double[] doubleArray2 = eigenDecompositionImpl0.getImagEigenvalues();
      EigenDecompositionImpl eigenDecompositionImpl1 = new EigenDecompositionImpl(doubleArray2, doubleArray1, 0.5);
  }
}
