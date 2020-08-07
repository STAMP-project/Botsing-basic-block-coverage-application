/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 22:08:56 UTC 2020
 */

package org.apache.commons.math.optimization.direct;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.SumSincFunction;
import org.apache.commons.math.optimization.GoalType;
import org.apache.commons.math.optimization.direct.BOBYQAOptimizer;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseAbstractMultivariateSimpleBoundsOptimizer_ESTest extends BaseAbstractMultivariateSimpleBoundsOptimizer_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      BOBYQAOptimizer bOBYQAOptimizer0 = new BOBYQAOptimizer(16);
      SumSincFunction sumSincFunction0 = new SumSincFunction((-1614.95469865));
      sumSincFunction0.partialDerivative((-125));
      GoalType goalType0 = GoalType.MINIMIZE;
      int int0 = (-809);
      int int1 = 3246;
      double[] doubleArray0 = new double[6];
      doubleArray0[0] = (double) 16;
      doubleArray0[2] = (-1620.0);
      doubleArray0[3] = (-0.48337120983330184);
      GoalType goalType1 = GoalType.MAXIMIZE;
      // Undeclared exception!
      bOBYQAOptimizer0.optimize(16, sumSincFunction0, goalType1, doubleArray0);
  }
}
