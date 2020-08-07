/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 02:07:51 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.Expm1Function;
import org.apache.commons.math.analysis.solvers.BaseSecantSolver;
import org.apache.commons.math.analysis.solvers.MullerSolver;
import org.apache.commons.math.analysis.solvers.SecantSolver;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseAbstractUnivariateRealSolver_ESTest extends BaseAbstractUnivariateRealSolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      MullerSolver mullerSolver0 = new MullerSolver();
      Expm1Function expm1Function0 = new Expm1Function();
      double double0 = SecantSolver.DEFAULT_ABSOLUTE_ACCURACY;
      SecantSolver secantSolver0 = new SecantSolver(0);
      secantSolver0.getEvaluations();
      double double1 = BaseSecantSolver.DEFAULT_ABSOLUTE_ACCURACY;
      secantSolver0.getMaxEvaluations();
      secantSolver0.getEvaluations();
      // Undeclared exception!
      secantSolver0.doSolve();
  }
}
