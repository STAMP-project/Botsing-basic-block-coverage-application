/*
 * This file was automatically generated by EvoSuite
 * Fri Jan 17 22:47:59 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.Expm1Function;
import org.apache.commons.math.analysis.UnivariateRealFunction;
import org.apache.commons.math.analysis.solvers.PegasusSolver;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseSecantSolver_ESTest extends BaseSecantSolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      PegasusSolver pegasusSolver0 = new PegasusSolver((-1700.32662065), (-1186.347407), 1.0E-6);
      Expm1Function expm1Function0 = new Expm1Function();
      // Undeclared exception!
      pegasusSolver0.solve(10, (UnivariateRealFunction) expm1Function0, (-1700.32662065), 60.65, Double.NEGATIVE_INFINITY);
  }
}
