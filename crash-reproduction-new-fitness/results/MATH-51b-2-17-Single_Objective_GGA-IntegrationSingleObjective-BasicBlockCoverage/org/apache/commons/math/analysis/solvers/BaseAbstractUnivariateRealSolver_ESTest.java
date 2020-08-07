/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 02:41:50 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.Expm1Function;
import org.apache.commons.math.analysis.QuinticFunction;
import org.apache.commons.math.analysis.solvers.NewtonSolver;
import org.apache.commons.math.analysis.solvers.PegasusSolver;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseAbstractUnivariateRealSolver_ESTest extends BaseAbstractUnivariateRealSolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      QuinticFunction quinticFunction0 = new QuinticFunction();
      quinticFunction0.derivative();
      quinticFunction0.derivative();
      quinticFunction0.derivative();
      quinticFunction0.derivative();
      quinticFunction0.derivative();
      Expm1Function expm1Function0 = new Expm1Function();
      quinticFunction0.derivative();
      quinticFunction0.derivative();
      quinticFunction0.derivative();
      quinticFunction0.derivative();
      quinticFunction0.derivative();
      quinticFunction0.derivative();
      QuinticFunction quinticFunction1 = new QuinticFunction();
      quinticFunction0.derivative();
      Expm1Function expm1Function1 = new Expm1Function();
      double double0 = 0.0;
      PegasusSolver pegasusSolver0 = new PegasusSolver();
      NewtonSolver newtonSolver0 = new NewtonSolver(0.5);
      // Undeclared exception!
      newtonSolver0.doSolve();
  }
}
