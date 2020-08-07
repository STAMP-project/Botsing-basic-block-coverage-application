/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 22:11:19 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.DifferentiableUnivariateFunction;
import org.apache.commons.math.analysis.Expm1Function;
import org.apache.commons.math.analysis.solvers.NewtonSolver;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseAbstractUnivariateRealSolver_ESTest extends BaseAbstractUnivariateRealSolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      double double0 = 1.5;
      NewtonSolver newtonSolver0 = new NewtonSolver();
      Expm1Function expm1Function0 = new Expm1Function();
      // Undeclared exception!
      newtonSolver0.solve((-1129), (DifferentiableUnivariateFunction) expm1Function0, 1.5, (-502.6517871));
  }
}
