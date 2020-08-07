/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 22:17:51 UTC 2020
 */

package org.apache.commons.math.optimization.direct;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.MultivariateRealFunction;
import org.apache.commons.math.analysis.MultivariateVectorialFunction;
import org.apache.commons.math.optimization.GoalType;
import org.apache.commons.math.optimization.LeastSquaresConverter;
import org.apache.commons.math.optimization.OptimizationException;
import org.apache.commons.math.optimization.SimpleRealPointChecker;
import org.apache.commons.math.optimization.direct.MultiDirectional;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class DirectSearchOptimizer_ESTest extends DirectSearchOptimizer_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      MultiDirectional multiDirectional0 = new MultiDirectional();
      MultivariateRealFunction multivariateRealFunction0 = mock(MultivariateRealFunction.class, new ViolatedAssumptionAnswer());
      doReturn(0.0, 0.0, 0.0, 0.0, 0.0).when(multivariateRealFunction0).value(any(double[].class));
      SimpleRealPointChecker simpleRealPointChecker0 = new SimpleRealPointChecker(0.7615941559557649, (-1138.4783714));
      multiDirectional0.setConvergenceChecker(simpleRealPointChecker0);
      SimpleRealPointChecker simpleRealPointChecker1 = new SimpleRealPointChecker(423.63, (-1138.4783714));
      multiDirectional0.setMaxIterations(2);
      MultivariateVectorialFunction multivariateVectorialFunction0 = mock(MultivariateVectorialFunction.class, new ViolatedAssumptionAnswer());
      double[] doubleArray0 = new double[9];
      doubleArray0[0] = (-1138.4783714);
      doubleArray0[1] = 1041.9761501646917;
      doubleArray0[2] = (-1138.4783714);
      doubleArray0[3] = 1041.9761501646917;
      doubleArray0[4] = 0.495;
      doubleArray0[5] = (-1.0);
      doubleArray0[6] = (-1.0);
      doubleArray0[7] = 1041.9761501646917;
      doubleArray0[8] = 0.7615941559557649;
      LeastSquaresConverter leastSquaresConverter0 = new LeastSquaresConverter(multivariateVectorialFunction0, doubleArray0);
      GoalType goalType0 = GoalType.MINIMIZE;
      try { 
        multiDirectional0.optimize(multivariateRealFunction0, goalType0, doubleArray0);
        fail("Expecting exception: OptimizationException");
      
      } catch(OptimizationException e) {
         //
         // org.apache.commons.math.MaxIterationsExceededException: Maximal number of iterations (2) exceeded
         //
         verifyException("org.apache.commons.math.optimization.direct.DirectSearchOptimizer", e);
      }
  }
}
