/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 22:10:52 UTC 2020
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
      int int0 = 20;
      BOBYQAOptimizer bOBYQAOptimizer0 = new BOBYQAOptimizer(20, 0.0, 20);
      int int1 = 57;
      double double0 = 1973.9822563532377;
      SumSincFunction sumSincFunction0 = new SumSincFunction(1973.9822563532377);
      GoalType goalType0 = GoalType.MINIMIZE;
      double[] doubleArray0 = new double[5];
      doubleArray0[0] = (double) 57;
      doubleArray0[2] = (double) 57;
      doubleArray0[3] = (double) 57;
      doubleArray0[3] = (double) 57;
      // Undeclared exception!
      bOBYQAOptimizer0.optimize(57, sumSincFunction0, goalType0, doubleArray0);
  }
}
