/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 21:05:37 UTC 2021
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
      AllowedSolution allowedSolution0 = AllowedSolution.ABOVE_SIDE;
      IllinoisSolver illinoisSolver0 = new IllinoisSolver(3.9736429850260626E-8, (-4.999999999999716), 453.19372);
      SinFunction sinFunction0 = new SinFunction();
      // Undeclared exception!
      illinoisSolver0.solve(2, (UnivariateRealFunction) sinFunction0, (-0.028259040530708636), 453.19372, 3.9736429850260626E-8, allowedSolution0);
  }
}
