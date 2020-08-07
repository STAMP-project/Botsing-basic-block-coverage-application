/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 22:26:35 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.SinFunction;
import org.apache.commons.math.analysis.UnivariateRealFunction;
import org.apache.commons.math.analysis.solvers.AllowedSolution;
import org.apache.commons.math.analysis.solvers.IllinoisSolver;
import org.apache.commons.math.analysis.solvers.RegulaFalsiSolver;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseSecantSolver_ESTest extends BaseSecantSolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      IllinoisSolver illinoisSolver0 = new IllinoisSolver((-960.9690433));
      SinFunction sinFunction0 = new SinFunction();
      RegulaFalsiSolver regulaFalsiSolver0 = new RegulaFalsiSolver((-960.9690433));
      AllowedSolution allowedSolution0 = AllowedSolution.LEFT_SIDE;
      // Undeclared exception!
      regulaFalsiSolver0.solve(2284, (UnivariateRealFunction) sinFunction0, 2527.14, 3562.358741, 294.091687431385, allowedSolution0);
  }
}
