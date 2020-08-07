/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 02:33:44 UTC 2020
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
      int int0 = (-1981);
      BOBYQAOptimizer bOBYQAOptimizer0 = new BOBYQAOptimizer(13);
      double double0 = (-3920.3882879411);
      SumSincFunction sumSincFunction0 = new SumSincFunction((-1981));
      GoalType goalType0 = GoalType.MAXIMIZE;
      double[] doubleArray0 = new double[4];
      doubleArray0[0] = (-2087.49335);
      doubleArray0[1] = (double) 13;
      doubleArray0[2] = (-3920.3882879411);
      doubleArray0[3] = Double.POSITIVE_INFINITY;
      // Undeclared exception!
      bOBYQAOptimizer0.optimize(13, sumSincFunction0, goalType0, doubleArray0);
  }
}
