/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 22:20:41 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.UnivariateRealFunction;
import org.apache.commons.math.analysis.XMinus5Function;
import org.apache.commons.math.analysis.solvers.AllowedSolution;
import org.apache.commons.math.analysis.solvers.PegasusSolver;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseSecantSolver_ESTest extends BaseSecantSolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      PegasusSolver pegasusSolver0 = new PegasusSolver();
      XMinus5Function xMinus5Function0 = new XMinus5Function();
      AllowedSolution allowedSolution0 = AllowedSolution.RIGHT_SIDE;
      pegasusSolver0.solve(2075, (UnivariateRealFunction) xMinus5Function0, (-4450.630619), (double) 2075, (-4450.630619), allowedSolution0);
      pegasusSolver0.getMax();
      pegasusSolver0.doSolve();
      AllowedSolution allowedSolution1 = AllowedSolution.ANY_SIDE;
      // Undeclared exception!
      pegasusSolver0.solve(2, (UnivariateRealFunction) xMinus5Function0, (-2.2250738585072014E-308), 2887.3888, allowedSolution1);
  }
}
