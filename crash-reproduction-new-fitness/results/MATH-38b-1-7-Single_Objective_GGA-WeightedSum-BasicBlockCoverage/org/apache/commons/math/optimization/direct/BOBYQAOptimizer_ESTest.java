/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 21:30:22 UTC 2020
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
      BOBYQAOptimizer bOBYQAOptimizer0 = new BOBYQAOptimizer(15);
      SumSincFunction sumSincFunction0 = new SumSincFunction((-1901.4879450067342));
      GoalType goalType0 = GoalType.MINIMIZE;
      double[] doubleArray0 = new double[5];
      doubleArray0[0] = (double) 15;
      sumSincFunction0.partialDerivative(15);
      int int0 = BOBYQAOptimizer.MINIMUM_PROBLEM_DIMENSION;
      // Undeclared exception!
      bOBYQAOptimizer0.optimize(15, sumSincFunction0, goalType0, doubleArray0);
  }
}
