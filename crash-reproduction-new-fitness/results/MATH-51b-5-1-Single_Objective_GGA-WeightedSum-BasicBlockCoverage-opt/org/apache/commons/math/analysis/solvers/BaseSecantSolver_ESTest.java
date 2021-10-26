/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 21:01:14 UTC 2021
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.Expm1Function;
import org.apache.commons.math.analysis.UnivariateRealFunction;
import org.apache.commons.math.analysis.solvers.AllowedSolution;
import org.apache.commons.math.analysis.solvers.RegulaFalsiSolver;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseSecantSolver_ESTest extends BaseSecantSolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      RegulaFalsiSolver regulaFalsiSolver0 = new RegulaFalsiSolver(1921.93212557179);
      Expm1Function expm1Function0 = new Expm1Function();
      expm1Function0.derivative();
      double double0 = 1368.87442004;
      AllowedSolution allowedSolution0 = AllowedSolution.ABOVE_SIDE;
      double double1 = (-0.009212472244026994);
      // Undeclared exception!
      regulaFalsiSolver0.solve(12, (UnivariateRealFunction) expm1Function0, (-0.009212472244026994), (double) 1562, allowedSolution0);
  }
}
