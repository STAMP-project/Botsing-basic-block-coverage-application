/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 23:00:56 UTC 2020
 */

package org.apache.commons.math.optimization.direct;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.Comparator;
import org.apache.commons.math.analysis.MultivariateRealFunction;
import org.apache.commons.math.optimization.GoalType;
import org.apache.commons.math.optimization.OptimizationException;
import org.apache.commons.math.optimization.RealPointValuePair;
import org.apache.commons.math.optimization.direct.MultiDirectional;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class DirectSearchOptimizer_ESTest extends DirectSearchOptimizer_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      MultivariateRealFunction multivariateRealFunction0 = mock(MultivariateRealFunction.class, new ViolatedAssumptionAnswer());
      doReturn(0.0, 0.0, 0.0, 0.0, 0.0).when(multivariateRealFunction0).value(any(double[].class));
      double[] doubleArray0 = new double[20];
      doubleArray0[7] = 0.0;
      MultiDirectional multiDirectional0 = new MultiDirectional();
      double[] doubleArray1 = new double[5];
      doubleArray1[0] = 283.4059448431692;
      doubleArray1[1] = (-1.3012317860855869);
      RealPointValuePair[] realPointValuePairArray0 = new RealPointValuePair[1];
      RealPointValuePair realPointValuePair0 = new RealPointValuePair(doubleArray0, (-2.0));
      realPointValuePairArray0[0] = realPointValuePair0;
      multiDirectional0.simplex = realPointValuePairArray0;
      multiDirectional0.incrementIterationsCounter();
      Comparator<RealPointValuePair> comparator0 = (Comparator<RealPointValuePair>) mock(Comparator.class, new ViolatedAssumptionAnswer());
      multiDirectional0.replaceWorstPoint((RealPointValuePair) null, comparator0);
      doubleArray1[2] = 283.4059448431692;
      multiDirectional0.setMaxIterations(1);
      GoalType goalType0 = GoalType.MINIMIZE;
      try { 
        multiDirectional0.optimize(multivariateRealFunction0, goalType0, doubleArray0);
        fail("Expecting exception: OptimizationException");
      
      } catch(OptimizationException e) {
         //
         // org.apache.commons.math.MaxIterationsExceededException: Maximal number of iterations (1) exceeded
         //
         verifyException("org.apache.commons.math.optimization.direct.DirectSearchOptimizer", e);
      }
  }
}
