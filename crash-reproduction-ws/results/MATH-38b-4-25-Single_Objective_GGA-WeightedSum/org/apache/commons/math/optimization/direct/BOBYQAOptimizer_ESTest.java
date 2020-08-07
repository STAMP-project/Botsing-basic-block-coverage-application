/*
 * This file was automatically generated by EvoSuite
 * Fri Jan 17 22:35:14 UTC 2020
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
      BOBYQAOptimizer bOBYQAOptimizer0 = new BOBYQAOptimizer(14);
      SumSincFunction sumSincFunction0 = new SumSincFunction(14);
      GoalType goalType0 = GoalType.MAXIMIZE;
      double[] doubleArray0 = new double[6];
      // Undeclared exception!
      bOBYQAOptimizer0.optimize(45, sumSincFunction0, goalType0, doubleArray0);
  }
}
