/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 02:34:32 UTC 2020
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
      BOBYQAOptimizer bOBYQAOptimizer0 = new BOBYQAOptimizer(27, 27, 27);
      SumSincFunction sumSincFunction0 = new SumSincFunction(27);
      SumSincFunction sumSincFunction1 = new SumSincFunction(27);
      sumSincFunction1.gradient();
      sumSincFunction1.partialDerivative((-786));
      SumSincFunction sumSincFunction2 = new SumSincFunction((-786));
      double[] doubleArray0 = new double[9];
      doubleArray0[0] = (double) 2025;
      doubleArray0[1] = (double) 2025;
      GoalType goalType0 = GoalType.MINIMIZE;
      // Undeclared exception!
      bOBYQAOptimizer0.optimize(27, sumSincFunction0, goalType0, doubleArray0);
  }
}
