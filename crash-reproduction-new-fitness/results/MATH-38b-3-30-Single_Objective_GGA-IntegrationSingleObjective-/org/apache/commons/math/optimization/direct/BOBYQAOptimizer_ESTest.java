/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 02:40:50 UTC 2020
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
      SumSincFunction sumSincFunction0 = new SumSincFunction(5);
      sumSincFunction0.partialDerivative((-20));
      BOBYQAOptimizer bOBYQAOptimizer0 = new BOBYQAOptimizer(19);
      bOBYQAOptimizer0.getEvaluations();
      GoalType goalType0 = GoalType.MINIMIZE;
      double[] doubleArray0 = new double[8];
      doubleArray0[0] = (double) (-20);
      doubleArray0[1] = 0.0;
      doubleArray0[2] = (double) 0;
      doubleArray0[3] = 0.0;
      doubleArray0[4] = (double) 5;
      doubleArray0[5] = (double) (-20);
      doubleArray0[6] = (double) 0;
      doubleArray0[7] = (double) 19;
      // Undeclared exception!
      bOBYQAOptimizer0.optimize(19, sumSincFunction0, goalType0, doubleArray0);
  }
}
