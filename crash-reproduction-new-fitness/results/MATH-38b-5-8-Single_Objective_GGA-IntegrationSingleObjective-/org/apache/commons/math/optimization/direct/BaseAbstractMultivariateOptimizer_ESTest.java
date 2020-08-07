/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 22:13:02 UTC 2020
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
public class BaseAbstractMultivariateOptimizer_ESTest extends BaseAbstractMultivariateOptimizer_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      double double0 = BOBYQAOptimizer.DEFAULT_STOPPING_RADIUS;
      int int0 = 336;
      double[][] doubleArray0 = new double[3][6];
      doubleArray0[0] = doubleArray0[0];
      BOBYQAOptimizer bOBYQAOptimizer0 = new BOBYQAOptimizer(14);
      bOBYQAOptimizer0.getEvaluations();
      SumSincFunction sumSincFunction0 = new SumSincFunction(336);
      sumSincFunction0.value(doubleArray0[0]);
      GoalType goalType0 = GoalType.MAXIMIZE;
      // Undeclared exception!
      bOBYQAOptimizer0.optimize(534, sumSincFunction0, goalType0, doubleArray0[0]);
  }
}
