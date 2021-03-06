/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 22:58:39 UTC 2020
 */

package org.apache.commons.math.optimization.direct;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
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
      double[] doubleArray0 = new double[5];
      doubleArray0[0] = 706.5620974376961;
      doubleArray0[1] = 732.5664357;
      doubleArray0[2] = 706.5620974376961;
      doubleArray0[3] = 0.5;
      doubleArray0[4] = 0.0;
      MultiDirectional multiDirectional0 = new MultiDirectional(0.0235, (-1630.1639258207));
      multiDirectional0.setMaxIterations((-640));
      RealPointValuePair[] realPointValuePairArray0 = new RealPointValuePair[5];
      RealPointValuePair realPointValuePair0 = new RealPointValuePair(doubleArray0, 0.679);
      realPointValuePairArray0[0] = realPointValuePair0;
      RealPointValuePair realPointValuePair1 = new RealPointValuePair(doubleArray0, 732.5664357, false);
      realPointValuePairArray0[1] = realPointValuePair1;
      GoalType goalType0 = GoalType.MINIMIZE;
      try { 
        multiDirectional0.optimize(multivariateRealFunction0, goalType0, doubleArray0);
        fail("Expecting exception: OptimizationException");
      
      } catch(OptimizationException e) {
         //
         // org.apache.commons.math.MaxIterationsExceededException: Maximal number of iterations (-640) exceeded
         //
         verifyException("org.apache.commons.math.optimization.direct.DirectSearchOptimizer", e);
      }
  }
}
