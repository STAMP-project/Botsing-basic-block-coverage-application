/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 02:32:14 UTC 2020
 */

package org.apache.commons.math.optimization.direct;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.MultivariateFunction;
import org.apache.commons.math.optimization.GoalType;
import org.apache.commons.math.optimization.direct.BOBYQAOptimizer;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BOBYQAOptimizer_ESTest extends BOBYQAOptimizer_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      BOBYQAOptimizer bOBYQAOptimizer0 = new BOBYQAOptimizer(17, 17, (-1.0));
      int int0 = 0;
      MultivariateFunction multivariateFunction0 = mock(MultivariateFunction.class, new ViolatedAssumptionAnswer());
      doReturn(0.0, 0.0, 0.0, 0.0, 0.0).when(multivariateFunction0).value(any(double[].class));
      GoalType goalType0 = GoalType.MAXIMIZE;
      double[] doubleArray0 = new double[8];
      doubleArray0[0] = (double) 0;
      doubleArray0[1] = (double) 17;
      doubleArray0[2] = (-1.0);
      doubleArray0[4] = (double) 17;
      double[] doubleArray1 = new double[6];
      doubleArray1[0] = (-1.0);
      doubleArray1[1] = (double) 17;
      doubleArray1[2] = (-1.0);
      doubleArray1[3] = (double) 0;
      doubleArray1[4] = (double) 17;
      doubleArray1[5] = (-4285.0);
      // Undeclared exception!
      bOBYQAOptimizer0.optimize(17, multivariateFunction0, goalType0, doubleArray1);
  }
}
