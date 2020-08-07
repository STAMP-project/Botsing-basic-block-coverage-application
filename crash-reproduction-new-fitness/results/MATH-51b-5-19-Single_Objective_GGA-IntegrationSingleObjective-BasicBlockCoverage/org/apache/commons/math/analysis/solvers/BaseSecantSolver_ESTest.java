/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 02:42:37 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.MonitoredFunction;
import org.apache.commons.math.analysis.QuinticFunction;
import org.apache.commons.math.analysis.UnivariateRealFunction;
import org.apache.commons.math.analysis.solvers.AllowedSolution;
import org.apache.commons.math.analysis.solvers.BaseSecantSolver;
import org.apache.commons.math.analysis.solvers.PegasusSolver;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseSecantSolver_ESTest extends BaseSecantSolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      int int0 = 3;
      QuinticFunction quinticFunction0 = new QuinticFunction();
      UnivariateRealFunction univariateRealFunction0 = quinticFunction0.derivative();
      MonitoredFunction monitoredFunction0 = new MonitoredFunction(univariateRealFunction0);
      quinticFunction0.derivative();
      quinticFunction0.derivative();
      MonitoredFunction monitoredFunction1 = new MonitoredFunction(univariateRealFunction0);
      MonitoredFunction monitoredFunction2 = new MonitoredFunction(quinticFunction0);
      int int1 = (-443387707);
      BaseSecantSolver.Method.values();
      PegasusSolver pegasusSolver0 = new PegasusSolver((-131596), 0.0);
      BaseSecantSolver.Method.values();
      BaseSecantSolver.Method.values();
      AllowedSolution allowedSolution0 = AllowedSolution.RIGHT_SIDE;
      // Undeclared exception!
      pegasusSolver0.solve(3, (UnivariateRealFunction) quinticFunction0, (-3458.074407), (double) 307, allowedSolution0);
  }
}
