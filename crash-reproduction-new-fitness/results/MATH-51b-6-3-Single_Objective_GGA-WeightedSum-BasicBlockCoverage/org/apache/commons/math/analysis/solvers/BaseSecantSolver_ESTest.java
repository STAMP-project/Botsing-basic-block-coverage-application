/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 21:39:25 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.SinFunction;
import org.apache.commons.math.analysis.SincFunction;
import org.apache.commons.math.analysis.UnivariateRealFunction;
import org.apache.commons.math.analysis.solvers.BaseSecantSolver;
import org.apache.commons.math.analysis.solvers.RegulaFalsiSolver;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseSecantSolver_ESTest extends BaseSecantSolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      RegulaFalsiSolver regulaFalsiSolver0 = new RegulaFalsiSolver();
      regulaFalsiSolver0.getMaxEvaluations();
      double double0 = BaseSecantSolver.DEFAULT_ABSOLUTE_ACCURACY;
      SincFunction sincFunction0 = new SincFunction();
      sincFunction0.derivative();
      BaseSecantSolver.Method.values();
      SinFunction sinFunction0 = new SinFunction();
      regulaFalsiSolver0.verifyInterval((-3152.75), (-244.29390424093));
      UnivariateRealFunction univariateRealFunction0 = sinFunction0.derivative();
      // Undeclared exception!
      regulaFalsiSolver0.solve(1487, univariateRealFunction0, (-3951.4344), (double) (-1354), (-5120.0858193));
  }
}
