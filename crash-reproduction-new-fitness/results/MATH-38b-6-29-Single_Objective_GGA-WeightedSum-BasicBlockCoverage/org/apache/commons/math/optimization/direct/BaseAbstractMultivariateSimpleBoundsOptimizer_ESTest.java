/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:56:54 UTC 2020
 */

package org.apache.commons.math.optimization.direct;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.SumSincFunction;
import org.apache.commons.math.optimization.ConvergenceChecker;
import org.apache.commons.math.optimization.GoalType;
import org.apache.commons.math.optimization.RealPointValuePair;
import org.apache.commons.math.optimization.direct.BOBYQAOptimizer;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseAbstractMultivariateSimpleBoundsOptimizer_ESTest extends BaseAbstractMultivariateSimpleBoundsOptimizer_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      ConvergenceChecker<RealPointValuePair> convergenceChecker0 = (ConvergenceChecker<RealPointValuePair>) mock(ConvergenceChecker.class, new ViolatedAssumptionAnswer());
      int int0 = 2391;
      BOBYQAOptimizer bOBYQAOptimizer0 = new BOBYQAOptimizer(2391, 2391, 2391);
      double double0 = 1.0E-8;
      int int1 = 1478;
      BOBYQAOptimizer bOBYQAOptimizer1 = new BOBYQAOptimizer(1478, 1478, 1.0E-8);
      double double1 = (-1.0);
      SumSincFunction sumSincFunction0 = new SumSincFunction(692.0);
      BOBYQAOptimizer bOBYQAOptimizer2 = new BOBYQAOptimizer(20);
      GoalType goalType0 = GoalType.MAXIMIZE;
      int int2 = 1505;
      double[] doubleArray0 = new double[6];
      double double2 = 0.0;
      SumSincFunction sumSincFunction1 = new SumSincFunction(0.0);
      GoalType goalType1 = GoalType.MAXIMIZE;
      // Undeclared exception!
      bOBYQAOptimizer2.optimize(1505, sumSincFunction1, goalType1, doubleArray0);
  }
}
