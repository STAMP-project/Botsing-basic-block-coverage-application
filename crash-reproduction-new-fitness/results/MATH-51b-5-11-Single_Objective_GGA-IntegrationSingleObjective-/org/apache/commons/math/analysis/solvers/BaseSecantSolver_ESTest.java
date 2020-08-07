/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 22:25:45 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.SinFunction;
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
      AllowedSolution allowedSolution0 = AllowedSolution.RIGHT_SIDE;
      SinFunction sinFunction0 = new SinFunction();
      sinFunction0.derivative();
      RegulaFalsiSolver regulaFalsiSolver0 = new RegulaFalsiSolver();
      sinFunction0.derivative();
      // Undeclared exception!
      regulaFalsiSolver0.solve(1668, (UnivariateRealFunction) sinFunction0, 1.8909537397323457, 10.553937388, 1.0E-14, allowedSolution0);
  }
}
