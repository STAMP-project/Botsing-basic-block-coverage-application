/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 22:21:30 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.MonitoredFunction;
import org.apache.commons.math.analysis.QuinticFunction;
import org.apache.commons.math.analysis.SinFunction;
import org.apache.commons.math.analysis.SincFunction;
import org.apache.commons.math.analysis.UnivariateRealFunction;
import org.apache.commons.math.analysis.solvers.IllinoisSolver;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseSecantSolver_ESTest extends BaseSecantSolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      SinFunction sinFunction0 = new SinFunction();
      sinFunction0.derivative();
      sinFunction0.derivative();
      UnivariateRealFunction univariateRealFunction0 = sinFunction0.derivative();
      sinFunction0.derivative();
      MonitoredFunction monitoredFunction0 = new MonitoredFunction(sinFunction0);
      sinFunction0.derivative();
      sinFunction0.derivative();
      sinFunction0.derivative();
      sinFunction0.derivative();
      SincFunction sincFunction0 = new SincFunction();
      sinFunction0.derivative();
      sinFunction0.derivative();
      sinFunction0.derivative();
      sinFunction0.derivative();
      sinFunction0.derivative();
      sinFunction0.derivative();
      QuinticFunction quinticFunction0 = new QuinticFunction();
      UnivariateRealFunction univariateRealFunction1 = quinticFunction0.derivative();
      MonitoredFunction monitoredFunction1 = new MonitoredFunction(univariateRealFunction1);
      IllinoisSolver illinoisSolver0 = new IllinoisSolver();
      // Undeclared exception!
      illinoisSolver0.solve(2, univariateRealFunction0, (-1.0), (double) 2, 0.0);
  }
}
