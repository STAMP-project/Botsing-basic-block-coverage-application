/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:58:58 UTC 2020
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
      BOBYQAOptimizer bOBYQAOptimizer0 = new BOBYQAOptimizer(10, 0.0, 10);
      int int0 = (-1491);
      SumSincFunction sumSincFunction0 = new SumSincFunction(10);
      GoalType goalType0 = GoalType.MINIMIZE;
      double[] doubleArray0 = new double[3];
      doubleArray0[0] = 0.0;
      doubleArray0[1] = (-1636.9);
      int int1 = 190;
      GoalType goalType1 = GoalType.MAXIMIZE;
      // Undeclared exception!
      bOBYQAOptimizer0.optimize(190, sumSincFunction0, goalType1, doubleArray0);
  }
}
