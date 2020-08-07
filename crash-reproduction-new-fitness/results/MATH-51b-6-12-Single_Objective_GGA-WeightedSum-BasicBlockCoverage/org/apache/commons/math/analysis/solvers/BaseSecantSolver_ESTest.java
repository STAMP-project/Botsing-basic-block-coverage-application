/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 21:41:13 UTC 2020
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
      PegasusSolver pegasusSolver0 = new PegasusSolver((-91.071179820071));
      int int0 = 2;
      Expm1Function expm1Function0 = new Expm1Function();
      double double0 = 1480.92108;
      // Undeclared exception!
      pegasusSolver0.solve(2, (UnivariateRealFunction) expm1Function0, (-91.071179820071), 1480.92108);
  }
}
