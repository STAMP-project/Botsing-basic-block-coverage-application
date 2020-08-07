/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:55:20 UTC 2020
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
public class BOBYQAOptimizer_ESTest extends BOBYQAOptimizer_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      double[] doubleArray0 = new double[8];
      BOBYQAOptimizer bOBYQAOptimizer0 = new BOBYQAOptimizer(13);
      SumSincFunction sumSincFunction0 = new SumSincFunction(0.0);
      sumSincFunction0.partialDerivative((-1796));
      double double0 = BOBYQAOptimizer.DEFAULT_STOPPING_RADIUS;
      SumSincFunction sumSincFunction1 = new SumSincFunction(0.0);
      sumSincFunction0.value(doubleArray0);
      GoalType goalType0 = GoalType.MINIMIZE;
      int int0 = 28;
      BOBYQAOptimizer bOBYQAOptimizer1 = new BOBYQAOptimizer(28);
      // Undeclared exception!
      bOBYQAOptimizer1.optimize(28, sumSincFunction0, goalType0, doubleArray0);
  }
}
