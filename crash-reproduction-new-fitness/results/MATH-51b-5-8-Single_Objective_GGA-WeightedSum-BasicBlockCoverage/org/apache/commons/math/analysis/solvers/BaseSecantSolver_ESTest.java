/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 21:40:32 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.MonitoredFunction;
import org.apache.commons.math.analysis.QuinticFunction;
import org.apache.commons.math.analysis.UnivariateRealFunction;
import org.apache.commons.math.analysis.solvers.BaseSecantSolver;
import org.apache.commons.math.analysis.solvers.RegulaFalsiSolver;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseSecantSolver_ESTest extends BaseSecantSolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      QuinticFunction quinticFunction0 = new QuinticFunction();
      MonitoredFunction monitoredFunction0 = new MonitoredFunction(quinticFunction0);
      int int0 = (-1699);
      QuinticFunction quinticFunction1 = new QuinticFunction();
      BaseSecantSolver.Method.values();
      RegulaFalsiSolver regulaFalsiSolver0 = new RegulaFalsiSolver();
      quinticFunction1.derivative();
      double double0 = 1071.8622057;
      // Undeclared exception!
      regulaFalsiSolver0.solve(187, (UnivariateRealFunction) quinticFunction1, (double) (-121), 1071.8622057);
  }
}
