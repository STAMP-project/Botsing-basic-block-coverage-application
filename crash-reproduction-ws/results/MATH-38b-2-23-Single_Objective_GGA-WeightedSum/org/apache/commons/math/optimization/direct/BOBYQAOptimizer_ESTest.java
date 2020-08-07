/*
 * This file was automatically generated by EvoSuite
 * Fri Jan 17 22:34:30 UTC 2020
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
      BOBYQAOptimizer bOBYQAOptimizer0 = new BOBYQAOptimizer(21, 267.196, 21);
      SumSincFunction sumSincFunction0 = new SumSincFunction(1.0E-20);
      GoalType goalType0 = GoalType.MINIMIZE;
      double[] doubleArray0 = new double[7];
      // Undeclared exception!
      bOBYQAOptimizer0.optimize(21, sumSincFunction0, goalType0, doubleArray0);
  }
}
