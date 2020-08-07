/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:55:36 UTC 2020
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
      GoalType goalType0 = GoalType.MAXIMIZE;
      GoalType goalType1 = GoalType.MAXIMIZE;
      BOBYQAOptimizer bOBYQAOptimizer0 = new BOBYQAOptimizer((-1219));
      SumSincFunction sumSincFunction0 = new SumSincFunction((-2146289272));
      BOBYQAOptimizer bOBYQAOptimizer1 = new BOBYQAOptimizer(5, (-139.741393), 3793.0);
      SumSincFunction sumSincFunction1 = new SumSincFunction((-712.897414516335));
      BOBYQAOptimizer bOBYQAOptimizer2 = new BOBYQAOptimizer(21, 3793.0, (-712.897414516335));
      int int0 = 20;
      double[] doubleArray0 = new double[7];
      doubleArray0[0] = 1.0;
      doubleArray0[1] = (double) 1278;
      doubleArray0[2] = 53.36184306423;
      doubleArray0[3] = (double) 1278;
      doubleArray0[4] = (-1.0);
      doubleArray0[5] = (double) (-2146289272);
      // Undeclared exception!
      bOBYQAOptimizer2.optimize(1278, sumSincFunction0, goalType1, doubleArray0);
  }
}
