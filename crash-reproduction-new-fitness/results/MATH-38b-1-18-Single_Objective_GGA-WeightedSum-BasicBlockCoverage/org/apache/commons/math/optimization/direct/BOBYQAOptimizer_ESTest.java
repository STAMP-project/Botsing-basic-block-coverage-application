/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:54:41 UTC 2020
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
      int int0 = 48;
      BOBYQAOptimizer bOBYQAOptimizer0 = new BOBYQAOptimizer(48);
      SumSincFunction sumSincFunction0 = new SumSincFunction((-384.94246628030885));
      int int1 = (-1698);
      sumSincFunction0.gradient();
      sumSincFunction0.partialDerivative((-1698));
      GoalType goalType0 = GoalType.MAXIMIZE;
      double[] doubleArray0 = new double[9];
      doubleArray0[0] = (-384.94246628030885);
      doubleArray0[1] = (double) 48;
      sumSincFunction0.value(doubleArray0);
      doubleArray0[2] = (double) 48;
      // Undeclared exception!
      bOBYQAOptimizer0.optimize(48, sumSincFunction0, goalType0, doubleArray0);
  }
}
