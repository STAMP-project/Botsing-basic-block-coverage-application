/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 21:26:11 UTC 2020
 */

package org.apache.commons.math.optimization.direct;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.SumSincFunction;
import org.apache.commons.math.optimization.GoalType;
import org.apache.commons.math.optimization.direct.BOBYQAOptimizer;
import org.apache.commons.math.random.Well44497a;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseAbstractMultivariateOptimizer_ESTest extends BaseAbstractMultivariateOptimizer_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      double double0 = 180.96812428;
      int int0 = BOBYQAOptimizer.MINIMUM_PROBLEM_DIMENSION;
      int int1 = 9;
      int int2 = 2303;
      double double1 = BOBYQAOptimizer.DEFAULT_STOPPING_RADIUS;
      Well44497a well44497a0 = new Well44497a(2);
      BOBYQAOptimizer bOBYQAOptimizer0 = new BOBYQAOptimizer(13, 2303, 1.0E-8);
      SumSincFunction sumSincFunction0 = new SumSincFunction(180.96812428);
      bOBYQAOptimizer0.getMaxEvaluations();
      sumSincFunction0.gradient();
      sumSincFunction0.partialDerivative(2);
      double[] doubleArray0 = new double[4];
      doubleArray0[0] = 1.0E-8;
      GoalType goalType0 = GoalType.MAXIMIZE;
      // Undeclared exception!
      bOBYQAOptimizer0.optimize(2303, sumSincFunction0, goalType0, doubleArray0);
  }
}
