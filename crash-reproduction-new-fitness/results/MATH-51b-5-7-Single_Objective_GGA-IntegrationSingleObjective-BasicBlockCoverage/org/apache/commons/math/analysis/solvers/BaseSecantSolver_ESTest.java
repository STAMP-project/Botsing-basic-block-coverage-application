/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 22:20:21 UTC 2020
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
      Expm1Function expm1Function0 = new Expm1Function();
      AllowedSolution allowedSolution0 = AllowedSolution.LEFT_SIDE;
      RegulaFalsiSolver regulaFalsiSolver0 = new RegulaFalsiSolver(197.8114, 4, (-6370.690846673739));
      // Undeclared exception!
      regulaFalsiSolver0.solve(1497, (UnivariateRealFunction) expm1Function0, (-1101.0795457076206), (double) 1497, 0.0, allowedSolution0);
  }
}
