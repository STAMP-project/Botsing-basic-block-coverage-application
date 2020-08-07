/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:54:04 UTC 2020
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
      double[] doubleArray0 = new double[20];
      BOBYQAOptimizer bOBYQAOptimizer0 = new BOBYQAOptimizer(53);
      SumSincFunction sumSincFunction0 = new SumSincFunction(3.0931312092963026);
      BOBYQAOptimizer bOBYQAOptimizer1 = new BOBYQAOptimizer(39);
      SumSincFunction sumSincFunction1 = new SumSincFunction(1531.8476992755982);
      GoalType goalType0 = GoalType.MINIMIZE;
      int int0 = 1265;
      GoalType goalType1 = GoalType.MAXIMIZE;
      // Undeclared exception!
      bOBYQAOptimizer0.optimize(1265, sumSincFunction0, goalType1, doubleArray0);
  }
}
