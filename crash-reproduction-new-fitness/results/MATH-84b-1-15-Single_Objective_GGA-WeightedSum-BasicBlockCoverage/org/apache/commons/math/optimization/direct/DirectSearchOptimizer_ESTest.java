/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 22:18:28 UTC 2020
 */

package org.apache.commons.math.optimization.direct;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.Comparator;
import org.apache.commons.math.analysis.MultivariateVectorialFunction;
import org.apache.commons.math.optimization.OptimizationException;
import org.apache.commons.math.optimization.RealPointValuePair;
import org.apache.commons.math.optimization.direct.MultiDirectional;
import org.apache.commons.math.optimization.direct.NelderMead;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class DirectSearchOptimizer_ESTest extends DirectSearchOptimizer_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      MultiDirectional multiDirectional0 = new MultiDirectional();
      NelderMead nelderMead0 = new NelderMead();
      RealPointValuePair[] realPointValuePairArray0 = new RealPointValuePair[23];
      double[] doubleArray0 = new double[9];
      doubleArray0[1] = (-1719.6909947087224);
      doubleArray0[6] = (-1719.6909947087224);
      RealPointValuePair realPointValuePair0 = new RealPointValuePair(doubleArray0, (-1719.6909947087224));
      realPointValuePairArray0[0] = realPointValuePair0;
      RealPointValuePair realPointValuePair1 = new RealPointValuePair(doubleArray0, 0.0, true);
      realPointValuePairArray0[1] = realPointValuePair1;
      realPointValuePairArray0[2] = realPointValuePair1;
      MultivariateVectorialFunction multivariateVectorialFunction0 = mock(MultivariateVectorialFunction.class, new ViolatedAssumptionAnswer());
      RealPointValuePair realPointValuePair2 = new RealPointValuePair(doubleArray0, (-1719.6909947087224), true);
      realPointValuePairArray0[5] = realPointValuePair2;
      RealPointValuePair realPointValuePair3 = new RealPointValuePair(doubleArray0, 0.0, true);
      realPointValuePairArray0[6] = realPointValuePair3;
      RealPointValuePair realPointValuePair4 = new RealPointValuePair(doubleArray0, 0.0, true);
      realPointValuePairArray0[7] = realPointValuePair4;
      nelderMead0.simplex = realPointValuePairArray0;
      nelderMead0.setMaxIterations((-37));
      double[] doubleArray1 = new double[0];
      double[] doubleArray2 = new double[4];
      doubleArray2[0] = (-1719.6909947087224);
      doubleArray2[1] = (-4779.79497605);
      doubleArray2[2] = (-4779.79497605);
      doubleArray2[3] = (-2114.757063);
      multiDirectional0.setStartConfiguration(doubleArray2);
      try { 
        nelderMead0.iterateSimplex((Comparator<RealPointValuePair>) null);
        fail("Expecting exception: OptimizationException");
      
      } catch(OptimizationException e) {
         //
         // org.apache.commons.math.MaxIterationsExceededException: Maximal number of iterations (-37) exceeded
         //
         verifyException("org.apache.commons.math.optimization.direct.DirectSearchOptimizer", e);
      }
  }
}
