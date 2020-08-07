/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 21:41:06 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.SincFunction;
import org.apache.commons.math.analysis.UnivariateRealFunction;
import org.apache.commons.math.analysis.solvers.PegasusSolver;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseSecantSolver_ESTest extends BaseSecantSolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      PegasusSolver pegasusSolver0 = new PegasusSolver((-3903.306), 3.676336746138986E-8);
      pegasusSolver0.getMin();
      SincFunction sincFunction0 = new SincFunction();
      sincFunction0.derivative();
      UnivariateRealFunction univariateRealFunction0 = sincFunction0.derivative();
      double double0 = (-4462.372262995);
      // Undeclared exception!
      pegasusSolver0.solve(3, univariateRealFunction0, (-4462.372262995), 948.0416557, (-2110.0));
  }
}
