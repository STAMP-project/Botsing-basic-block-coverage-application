/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 02:44:53 UTC 2020
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
import org.apache.commons.math.optimization.SimpleScalarValueChecker;
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
      MultivariateRealFunction multivariateRealFunction0 = mock(MultivariateRealFunction.class, new ViolatedAssumptionAnswer());
      doReturn(0.0, 0.0, 0.0, 0.0, 0.0).when(multivariateRealFunction0).value(any(double[].class));
      multiDirectional0.setMaxIterations((-1));
      NelderMead nelderMead0 = new NelderMead();
      GoalType goalType0 = GoalType.MINIMIZE;
      SimpleScalarValueChecker simpleScalarValueChecker0 = new SimpleScalarValueChecker((-1429.221072328), 0.29);
      double[] doubleArray0 = new double[4];
      doubleArray0[0] = 0.29;
      doubleArray0[1] = 0.29;
      doubleArray0[2] = (double) (-1);
      doubleArray0[3] = 0.29;
      Comparator<RealPointValuePair> comparator0 = (Comparator<RealPointValuePair>) mock(Comparator.class, new ViolatedAssumptionAnswer());
      double[][] doubleArray1 = new double[6][3];
      doubleArray1[0] = doubleArray0;
      doubleArray1[1] = doubleArray0;
      doubleArray1[2] = doubleArray0;
      RealPointValuePair[] realPointValuePairArray0 = new RealPointValuePair[1];
      try { 
        multiDirectional0.optimize(multivariateRealFunction0, goalType0, doubleArray0);
        fail("Expecting exception: OptimizationException");
      
      } catch(OptimizationException e) {
         //
         // org.apache.commons.math.MaxIterationsExceededException: Maximal number of iterations (-1) exceeded
         //
         verifyException("org.apache.commons.math.optimization.direct.DirectSearchOptimizer", e);
      }
  }
}
