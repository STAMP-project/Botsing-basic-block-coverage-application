/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 21:00:09 UTC 2021
 */

package org.apache.commons.math.optimization.direct;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.SumSincFunction;
import org.apache.commons.math.optimization.GoalType;
import org.apache.commons.math.optimization.direct.BOBYQAOptimizer;
import org.apache.commons.math.optimization.direct.CMAESOptimizer;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseAbstractMultivariateOptimizer_ESTest extends BaseAbstractMultivariateOptimizer_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      double[] doubleArray0 = new double[10];
      int int0 = 53;
      SumSincFunction sumSincFunction0 = new SumSincFunction(53);
      int int1 = CMAESOptimizer.DEFAULT_CHECKFEASABLECOUNT;
      int int2 = 57;
      SumSincFunction sumSincFunction1 = new SumSincFunction(0);
      BOBYQAOptimizer bOBYQAOptimizer0 = new BOBYQAOptimizer(57);
      GoalType goalType0 = GoalType.MINIMIZE;
      // Undeclared exception!
      bOBYQAOptimizer0.optimize(53, sumSincFunction1, goalType0, doubleArray0);
  }
}
