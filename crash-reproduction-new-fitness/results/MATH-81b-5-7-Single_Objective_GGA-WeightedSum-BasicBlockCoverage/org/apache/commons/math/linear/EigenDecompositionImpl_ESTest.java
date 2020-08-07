/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 11:45:44 UTC 2020
 */

package org.apache.commons.math.linear;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.linear.Array2DRowRealMatrix;
import org.apache.commons.math.linear.DefaultRealMatrixChangingVisitor;
import org.apache.commons.math.linear.DefaultRealMatrixPreservingVisitor;
import org.apache.commons.math.linear.EigenDecompositionImpl;
import org.apache.commons.math.linear.OpenMapRealVector;
import org.apache.commons.math.linear.RealMatrixPreservingVisitor;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class EigenDecompositionImpl_ESTest extends EigenDecompositionImpl_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      DefaultRealMatrixChangingVisitor defaultRealMatrixChangingVisitor0 = new DefaultRealMatrixChangingVisitor();
      double[] doubleArray0 = new double[8];
      doubleArray0[0] = (-1201.4716212409405);
      doubleArray0[1] = (-1201.4716212409405);
      doubleArray0[2] = (-1201.4716212409405);
      double double0 = 344.0;
      Array2DRowRealMatrix array2DRowRealMatrix0 = new Array2DRowRealMatrix(doubleArray0);
      DefaultRealMatrixPreservingVisitor defaultRealMatrixPreservingVisitor0 = new DefaultRealMatrixPreservingVisitor();
      array2DRowRealMatrix0.walkInRowOrder((RealMatrixPreservingVisitor) defaultRealMatrixPreservingVisitor0);
      DefaultRealMatrixPreservingVisitor defaultRealMatrixPreservingVisitor1 = new DefaultRealMatrixPreservingVisitor();
      array2DRowRealMatrix0.walkInRowOrder((RealMatrixPreservingVisitor) defaultRealMatrixPreservingVisitor0);
      int int0 = 13;
      OpenMapRealVector openMapRealVector0 = new OpenMapRealVector((-2133805408), 54.0);
      double[] doubleArray1 = new double[7];
      doubleArray1[0] = (-1211.9301);
      doubleArray1[2] = 344.0;
      doubleArray1[3] = 0.0;
      doubleArray1[4] = (double) 13;
      doubleArray1[5] = 2851.52425367353;
      doubleArray1[6] = (-1211.9301);
      double double1 = 2231.4694072;
      EigenDecompositionImpl eigenDecompositionImpl0 = new EigenDecompositionImpl(doubleArray0, doubleArray1, 2231.4694072);
  }
}
