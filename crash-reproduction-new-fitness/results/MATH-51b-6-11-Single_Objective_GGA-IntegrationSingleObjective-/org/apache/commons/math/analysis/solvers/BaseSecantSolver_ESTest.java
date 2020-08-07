/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 22:25:47 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.Expm1Function;
import org.apache.commons.math.analysis.UnivariateRealFunction;
import org.apache.commons.math.analysis.solvers.IllinoisSolver;
import org.apache.commons.math.analysis.solvers.PegasusSolver;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseSecantSolver_ESTest extends BaseSecantSolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      IllinoisSolver illinoisSolver0 = new IllinoisSolver();
      PegasusSolver pegasusSolver0 = new PegasusSolver(1919.22253291);
      Expm1Function expm1Function0 = new Expm1Function();
      // Undeclared exception!
      pegasusSolver0.solve(2, (UnivariateRealFunction) expm1Function0, (-1762.7542693443), (double) 2, 6.38905609893065);
  }
}
