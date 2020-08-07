/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 21:32:59 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.Expm1Function;
import org.apache.commons.math.analysis.SinFunction;
import org.apache.commons.math.analysis.UnivariateFunction;
import org.apache.commons.math.analysis.solvers.BrentSolver;
import org.apache.commons.math.analysis.solvers.NewtonSolver;
import org.apache.commons.math.analysis.solvers.SecantSolver;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseAbstractUnivariateRealSolver_ESTest extends BaseAbstractUnivariateRealSolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Expm1Function expm1Function0 = new Expm1Function();
      double double0 = (-1.743220115059699);
      SecantSolver secantSolver0 = new SecantSolver((-1.743220115059699));
      NewtonSolver newtonSolver0 = new NewtonSolver();
      SinFunction sinFunction0 = new SinFunction();
      double double1 = (-27.857125118805524);
      double double2 = 0.0;
      BrentSolver brentSolver0 = new BrentSolver(4, 0.0, (-2712.4166605231417));
      // Undeclared exception!
      brentSolver0.solve((-19), (UnivariateFunction) expm1Function0, (-27.857125118805524), 1260.11539);
  }
}
