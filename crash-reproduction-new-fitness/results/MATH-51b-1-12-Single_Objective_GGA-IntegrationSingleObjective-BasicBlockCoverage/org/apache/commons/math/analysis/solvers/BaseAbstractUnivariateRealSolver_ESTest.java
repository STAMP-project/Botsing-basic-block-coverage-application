/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 22:21:28 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.QuinticFunction;
import org.apache.commons.math.analysis.solvers.BracketingNthOrderBrentSolver;
import org.apache.commons.math.analysis.solvers.SecantSolver;
import org.apache.commons.math.exception.TooManyEvaluationsException;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseAbstractUnivariateRealSolver_ESTest extends BaseAbstractUnivariateRealSolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      SecantSolver secantSolver0 = new SecantSolver();
      double double0 = 16.853273115080533;
      BracketingNthOrderBrentSolver bracketingNthOrderBrentSolver0 = new BracketingNthOrderBrentSolver(154, (-16.621488525525173), 16.853273115080533, 154);
      double double1 = Double.NEGATIVE_INFINITY;
      bracketingNthOrderBrentSolver0.isSequence(154, 154, Double.NEGATIVE_INFINITY);
      double double2 = 1399.84;
      QuinticFunction quinticFunction0 = new QuinticFunction();
      quinticFunction0.derivative();
      bracketingNthOrderBrentSolver0.getEvaluations();
      double[] doubleArray0 = new double[7];
      doubleArray0[0] = 0.0;
      doubleArray0[1] = 1399.84;
      doubleArray0[2] = (double) 0;
      doubleArray0[4] = Double.NEGATIVE_INFINITY;
      doubleArray0[5] = 0.0;
      try { 
        bracketingNthOrderBrentSolver0.computeObjectiveValue(1399.84);
        fail("Expecting exception: TooManyEvaluationsException");
      
      } catch(TooManyEvaluationsException e) {
         //
         // illegal state: maximal count (0) exceeded: evaluations
         //
         verifyException("org.apache.commons.math.analysis.solvers.BaseAbstractUnivariateRealSolver", e);
      }
  }
}
