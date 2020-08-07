/*
 * This file was automatically generated by EvoSuite
 * Fri Jan 17 22:44:05 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.Expm1Function;
import org.apache.commons.math.analysis.MonitoredFunction;
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
      AllowedSolution allowedSolution0 = AllowedSolution.RIGHT_SIDE;
      IllinoisSolver illinoisSolver0 = new IllinoisSolver(1.0E-6);
      Expm1Function expm1Function0 = new Expm1Function();
      MonitoredFunction monitoredFunction0 = new MonitoredFunction(expm1Function0);
      // Undeclared exception!
      illinoisSolver0.solve(210, (UnivariateRealFunction) monitoredFunction0, (-3891.1860887), 1546.9643, allowedSolution0);
  }
}
