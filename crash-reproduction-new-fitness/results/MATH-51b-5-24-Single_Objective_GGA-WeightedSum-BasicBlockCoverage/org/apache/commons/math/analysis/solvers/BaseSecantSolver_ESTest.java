/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 02:06:49 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.QuinticFunction;
import org.apache.commons.math.analysis.UnivariateRealFunction;
import org.apache.commons.math.analysis.solvers.PegasusSolver;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseSecantSolver_ESTest extends BaseSecantSolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      PegasusSolver pegasusSolver0 = new PegasusSolver();
      int int0 = 4;
      QuinticFunction quinticFunction0 = new QuinticFunction();
      double double0 = (-1614.865332);
      // Undeclared exception!
      pegasusSolver0.solve(4, (UnivariateRealFunction) quinticFunction0, (-1614.865332), (double) 4);
  }
}
