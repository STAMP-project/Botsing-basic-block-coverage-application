/*
 * This file was automatically generated by EvoSuite
 * Fri Jan 17 22:47:01 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.SinFunction;
import org.apache.commons.math.analysis.UnivariateRealFunction;
import org.apache.commons.math.analysis.solvers.AllowedSolution;
import org.apache.commons.math.analysis.solvers.IllinoisSolver;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseSecantSolver_ESTest extends BaseSecantSolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      IllinoisSolver illinoisSolver0 = new IllinoisSolver();
      SinFunction sinFunction0 = new SinFunction();
      AllowedSolution allowedSolution0 = AllowedSolution.BELOW_SIDE;
      UnivariateRealFunction univariateRealFunction0 = sinFunction0.derivative();
      // Undeclared exception!
      illinoisSolver0.solve(5, univariateRealFunction0, (-454.653814637781), 1.0E-6, (double) 5, allowedSolution0);
  }
}
