/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 12:41:18 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.UnivariateRealFunction;
import org.apache.commons.math.analysis.solvers.BisectionSolver;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BisectionSolver_ESTest extends BisectionSolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      BisectionSolver bisectionSolver0 = new BisectionSolver();
      UnivariateRealFunction univariateRealFunction0 = null;
      double double0 = (-1941.932731);
      double double1 = 494.19275818817;
      // Undeclared exception!
      bisectionSolver0.solve((UnivariateRealFunction) null, (-1941.932731), 494.19275818817, 494.19275818817);
  }
}
