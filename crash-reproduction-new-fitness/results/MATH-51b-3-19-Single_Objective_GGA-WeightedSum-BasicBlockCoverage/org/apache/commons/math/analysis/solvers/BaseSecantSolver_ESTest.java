/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 02:05:48 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.SinFunction;
import org.apache.commons.math.analysis.SincFunction;
import org.apache.commons.math.analysis.UnivariateRealFunction;
import org.apache.commons.math.analysis.solvers.IllinoisSolver;
import org.apache.commons.math.analysis.solvers.RegulaFalsiSolver;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseSecantSolver_ESTest extends BaseSecantSolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      RegulaFalsiSolver regulaFalsiSolver0 = new RegulaFalsiSolver(0.0);
      SinFunction sinFunction0 = new SinFunction();
      regulaFalsiSolver0.setup(1140, sinFunction0, 0.0, (-2370.97129769), 1140);
      regulaFalsiSolver0.getRelativeAccuracy();
      IllinoisSolver illinoisSolver0 = new IllinoisSolver();
      illinoisSolver0.solve(1140, (UnivariateRealFunction) sinFunction0, (-612.52965632), 1.997844754509471E-9, (-3588.180066458651));
      SincFunction sincFunction0 = new SincFunction();
      sincFunction0.derivative();
      // Undeclared exception!
      regulaFalsiSolver0.solve(1140, (UnivariateRealFunction) sinFunction0, (-1.0), 1666.575712);
  }
}
