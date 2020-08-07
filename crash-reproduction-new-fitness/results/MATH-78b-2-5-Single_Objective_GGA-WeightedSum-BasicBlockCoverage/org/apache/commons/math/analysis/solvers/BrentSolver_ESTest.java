/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 22:14:18 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.SinFunction;
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
      brentSolver0.getRelativeAccuracy();
      brentSolver0.resetMaximalIterationCount();
      brentSolver0.setAbsoluteAccuracy(1.0E-14);
      brentSolver0.getAbsoluteAccuracy();
      brentSolver0.setResult(1.9607233326216973, 2092);
      brentSolver0.resetAbsoluteAccuracy();
      brentSolver0.getFunctionValueAccuracy();
      brentSolver0.getAbsoluteAccuracy();
      brentSolver0.getFunctionValueAccuracy();
      brentSolver0.setResult(1.9607233326216973, (-2060.059347993), 2092);
      SinFunction sinFunction0 = new SinFunction();
      UnivariateRealFunction univariateRealFunction0 = sinFunction0.derivative();
      // Undeclared exception!
      brentSolver0.solve(univariateRealFunction0, (-1.0), 0.0);
  }
}
