/*
 * This file was automatically generated by EvoSuite
 * Fri Jan 17 22:42:46 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.DifferentiableUnivariateRealFunction;
import org.apache.commons.math.analysis.Expm1Function;
import org.apache.commons.math.analysis.solvers.NewtonSolver;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseAbstractUnivariateRealSolver_ESTest extends BaseAbstractUnivariateRealSolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      NewtonSolver newtonSolver0 = new NewtonSolver();
      Expm1Function expm1Function0 = new Expm1Function();
      // Undeclared exception!
      newtonSolver0.solve((-1961), (DifferentiableUnivariateRealFunction) expm1Function0, 1146.494203134603, 1146.494203134603);
  }
}
