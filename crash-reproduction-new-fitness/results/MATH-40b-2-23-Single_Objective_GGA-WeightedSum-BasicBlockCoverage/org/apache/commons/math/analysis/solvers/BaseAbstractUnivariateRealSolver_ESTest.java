/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:58:39 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.DifferentiableUnivariateFunction;
import org.apache.commons.math.analysis.Expm1Function;
import org.apache.commons.math.analysis.QuinticFunction;
import org.apache.commons.math.analysis.SinFunction;
import org.apache.commons.math.analysis.SincFunction;
import org.apache.commons.math.analysis.solvers.NewtonSolver;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseAbstractUnivariateRealSolver_ESTest extends BaseAbstractUnivariateRealSolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      SincFunction sincFunction0 = new SincFunction();
      double double0 = 0.95;
      QuinticFunction quinticFunction0 = new QuinticFunction();
      Expm1Function expm1Function0 = new Expm1Function();
      SincFunction sincFunction1 = new SincFunction();
      SinFunction sinFunction0 = new SinFunction();
      NewtonSolver newtonSolver0 = new NewtonSolver(0.0);
      // Undeclared exception!
      newtonSolver0.solve(0, (DifferentiableUnivariateFunction) expm1Function0, (-2017.2641390891217));
  }
}
