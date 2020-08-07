/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 02:42:25 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.MonitoredFunction;
import org.apache.commons.math.analysis.SincFunction;
import org.apache.commons.math.analysis.UnivariateRealFunction;
import org.apache.commons.math.analysis.solvers.AllowedSolution;
import org.apache.commons.math.analysis.solvers.BaseSecantSolver;
import org.apache.commons.math.analysis.solvers.RegulaFalsiSolver;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseSecantSolver_ESTest extends BaseSecantSolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      double double0 = 0.0;
      SincFunction sincFunction0 = new SincFunction();
      sincFunction0.derivative();
      MonitoredFunction monitoredFunction0 = new MonitoredFunction(sincFunction0);
      monitoredFunction0.value(0.0);
      double double1 = 0.0;
      BaseSecantSolver.Method.values();
      BaseSecantSolver.Method.values();
      double double2 = (-99.29618146968);
      RegulaFalsiSolver regulaFalsiSolver0 = new RegulaFalsiSolver((-99.29618146968), (-99.29618146968));
      AllowedSolution allowedSolution0 = AllowedSolution.BELOW_SIDE;
      SincFunction sincFunction1 = new SincFunction();
      // Undeclared exception!
      regulaFalsiSolver0.solve(2, (UnivariateRealFunction) sincFunction1, (-642.0), (double) (-330), (-3066.80601476), allowedSolution0);
  }
}
