/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 02:28:11 UTC 2020
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
      int int0 = 16;
      BOBYQAOptimizer bOBYQAOptimizer0 = new BOBYQAOptimizer(16);
      double double0 = 2.0;
      SumSincFunction sumSincFunction0 = new SumSincFunction(2.0);
      GoalType goalType0 = GoalType.MINIMIZE;
      double[] doubleArray0 = new double[7];
      doubleArray0[0] = (double) 16;
      doubleArray0[1] = (double) 16;
      doubleArray0[2] = 2.0;
      // Undeclared exception!
      bOBYQAOptimizer0.optimize(16, sumSincFunction0, goalType0, doubleArray0);
  }
}
