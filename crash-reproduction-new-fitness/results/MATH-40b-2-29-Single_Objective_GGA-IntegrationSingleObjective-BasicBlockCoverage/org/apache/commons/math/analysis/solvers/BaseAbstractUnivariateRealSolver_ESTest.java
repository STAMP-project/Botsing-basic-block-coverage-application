/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 02:36:06 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.solvers.BaseSecantSolver;
import org.apache.commons.math.analysis.solvers.RiddersSolver;
import org.apache.commons.math.analysis.solvers.SecantSolver;
import org.apache.commons.math.exception.TooManyEvaluationsException;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseAbstractUnivariateRealSolver_ESTest extends BaseAbstractUnivariateRealSolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      double double0 = BaseSecantSolver.DEFAULT_ABSOLUTE_ACCURACY;
      RiddersSolver riddersSolver0 = new RiddersSolver();
      SecantSolver secantSolver0 = new SecantSolver(0.0, (-550.56));
      try { 
        secantSolver0.computeObjectiveValue((-3667.4645));
        fail("Expecting exception: TooManyEvaluationsException");
      
      } catch(TooManyEvaluationsException e) {
         //
         // illegal state: maximal count (0) exceeded: evaluations
         //
         verifyException("org.apache.commons.math.analysis.solvers.BaseAbstractUnivariateRealSolver", e);
      }
  }
}
