/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 12:40:16 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.SinFunction;
import org.apache.commons.math.analysis.UnivariateRealFunction;
import org.apache.commons.math.analysis.solvers.BisectionSolver;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BisectionSolver_ESTest extends BisectionSolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      double double0 = 0.0;
      SinFunction sinFunction0 = new SinFunction();
      sinFunction0.derivative();
      double double1 = 0.0;
      UnivariateRealFunction univariateRealFunction0 = mock(UnivariateRealFunction.class, new ViolatedAssumptionAnswer());
      sinFunction0.derivative();
      sinFunction0.derivative();
      double double2 = (-1506.303494);
      UnivariateRealFunction univariateRealFunction1 = mock(UnivariateRealFunction.class, new ViolatedAssumptionAnswer());
      BisectionSolver bisectionSolver0 = new BisectionSolver();
      double double3 = (-1039.367930639);
      double double4 = bisectionSolver0.defaultFunctionValueAccuracy;
      // Undeclared exception!
      bisectionSolver0.solve((-1.0), 0.03125, 536.9858149);
  }
}
