/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 22:22:15 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.MonitoredFunction;
import org.apache.commons.math.analysis.SincFunction;
import org.apache.commons.math.analysis.UnivariateRealFunction;
import org.apache.commons.math.analysis.solvers.BaseSecantSolver;
import org.apache.commons.math.analysis.solvers.IllinoisSolver;
import org.apache.commons.math.analysis.solvers.RegulaFalsiSolver;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseSecantSolver_ESTest extends BaseSecantSolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      BaseSecantSolver.Method.values();
      SincFunction sincFunction0 = new SincFunction();
      SincFunction sincFunction1 = new SincFunction();
      double double0 = BaseSecantSolver.DEFAULT_ABSOLUTE_ACCURACY;
      RegulaFalsiSolver regulaFalsiSolver0 = new RegulaFalsiSolver(1.0E-6, 0.0, 331.75123728);
      regulaFalsiSolver0.isSequence((-4339.0), (-4339.0), (-2227.277093));
      IllinoisSolver illinoisSolver0 = new IllinoisSolver((-2464.753707156));
      SincFunction sincFunction2 = new SincFunction();
      MonitoredFunction monitoredFunction0 = new MonitoredFunction(sincFunction0);
      sincFunction1.derivative();
      // Undeclared exception!
      illinoisSolver0.solve(5, (UnivariateRealFunction) sincFunction0, (-2874.47), 1272.655225533817, 1.0E-6);
  }
}
