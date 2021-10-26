/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 21:26:06 UTC 2021
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.Expm1Function;
import org.apache.commons.math.analysis.MonitoredFunction;
import org.apache.commons.math.analysis.UnivariateRealFunction;
import org.apache.commons.math.analysis.solvers.BrentSolver;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BrentSolver_ESTest extends BrentSolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      BrentSolver brentSolver0 = new BrentSolver();
      Expm1Function expm1Function0 = new Expm1Function();
      BrentSolver brentSolver1 = new BrentSolver(expm1Function0);
      MonitoredFunction monitoredFunction0 = new MonitoredFunction(expm1Function0);
      BrentSolver brentSolver2 = new BrentSolver();
      // Undeclared exception!
      brentSolver2.solve((UnivariateRealFunction) expm1Function0, (-3786.19), (-857.1));
  }
}
