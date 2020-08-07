/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 02:32:07 UTC 2020
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
public class BaseAbstractMultivariateSimpleBoundsOptimizer_ESTest extends BaseAbstractMultivariateSimpleBoundsOptimizer_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      MultivariateFunction multivariateFunction0 = mock(MultivariateFunction.class, new ViolatedAssumptionAnswer());
      doReturn(0.0, 0.0, 0.0, 0.0, 0.0).when(multivariateFunction0).value(any(double[].class));
      BOBYQAOptimizer bOBYQAOptimizer0 = new BOBYQAOptimizer(17, 17, 17);
      GoalType goalType0 = GoalType.MAXIMIZE;
      double[] doubleArray0 = new double[6];
      doubleArray0[0] = (double) 1674;
      doubleArray0[1] = (-342.77953346505893);
      doubleArray0[2] = (double) 17;
      doubleArray0[3] = (double) 17;
      doubleArray0[4] = (double) 3;
      doubleArray0[5] = (double) 3;
      // Undeclared exception!
      bOBYQAOptimizer0.optimize(1674, multivariateFunction0, goalType0, doubleArray0, (double[]) null, (double[]) null);
  }
}
