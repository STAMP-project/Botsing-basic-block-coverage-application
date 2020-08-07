/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 22:17:02 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.QuinticFunction;
import org.apache.commons.math.analysis.UnivariateFunction;
import org.apache.commons.math.analysis.solvers.BisectionSolver;
import org.apache.commons.math.analysis.solvers.NewtonSolver;
import org.apache.commons.math.analysis.solvers.RiddersSolver;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseAbstractUnivariateRealSolver_ESTest extends BaseAbstractUnivariateRealSolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      NewtonSolver newtonSolver0 = new NewtonSolver();
      double double0 = 0.45971803989819093;
      BisectionSolver bisectionSolver0 = new BisectionSolver(0.45971803989819093);
      int int0 = 0;
      QuinticFunction quinticFunction0 = new QuinticFunction();
      double double1 = (-2.0);
      double double2 = 0.5;
      RiddersSolver riddersSolver0 = new RiddersSolver();
      int int1 = 555;
      bisectionSolver0.isSequence(0, 0.5, 2763.7190063);
      int int2 = 0;
      QuinticFunction quinticFunction1 = new QuinticFunction();
      quinticFunction1.derivative();
      double double3 = (-46.782);
      // Undeclared exception!
      riddersSolver0.solve(0, (UnivariateFunction) quinticFunction1, (-46.782), (double) 0, (-46.782));
  }
}
