/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:58:35 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.solvers.BaseSecantSolver;
import org.apache.commons.math.analysis.solvers.BisectionSolver;
import org.apache.commons.math.analysis.solvers.NewtonSolver;
import org.apache.commons.math.analysis.solvers.SecantSolver;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseAbstractUnivariateRealSolver_ESTest extends BaseAbstractUnivariateRealSolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      double double0 = (-12.799393208592997);
      double double1 = (-1091.4692098969535);
      NewtonSolver newtonSolver0 = new NewtonSolver();
      SecantSolver secantSolver0 = new SecantSolver((-1091.4692098969535));
      double double2 = BaseSecantSolver.DEFAULT_ABSOLUTE_ACCURACY;
      BisectionSolver bisectionSolver0 = new BisectionSolver((-12.799393208592997), (-1091.4692098969535));
      // Undeclared exception!
      bisectionSolver0.incrementEvaluationCount();
  }
}
