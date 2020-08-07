/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 22:12:27 UTC 2020
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
      SumSincFunction sumSincFunction0 = new SumSincFunction(8.0);
      double[] doubleArray0 = new double[3];
      doubleArray0[0] = (double) (-36);
      doubleArray0[1] = 8.0;
      doubleArray0[2] = (double) (-36);
      sumSincFunction0.value(doubleArray0);
      sumSincFunction0.gradient();
      double[] doubleArray1 = new double[1];
      sumSincFunction0.value(doubleArray1);
      double double0 = BOBYQAOptimizer.DEFAULT_STOPPING_RADIUS;
      sumSincFunction0.value(doubleArray1);
      BOBYQAOptimizer bOBYQAOptimizer0 = new BOBYQAOptimizer(36, 1074.0, 8.0);
      GoalType goalType0 = GoalType.MINIMIZE;
      double[] doubleArray2 = new double[17];
      doubleArray2[0] = (double) 6;
      doubleArray2[1] = (double) 6;
      doubleArray2[2] = (double) 36;
      doubleArray2[3] = (double) 1074;
      sumSincFunction0.partialDerivative(36);
      doubleArray2[4] = (-3064.089115226);
      doubleArray2[6] = (double) 54;
      sumSincFunction0.partialDerivative(20);
      doubleArray2[7] = (double) 54;
      // Undeclared exception!
      bOBYQAOptimizer0.optimize(54, sumSincFunction0, goalType0, doubleArray2);
  }
}
