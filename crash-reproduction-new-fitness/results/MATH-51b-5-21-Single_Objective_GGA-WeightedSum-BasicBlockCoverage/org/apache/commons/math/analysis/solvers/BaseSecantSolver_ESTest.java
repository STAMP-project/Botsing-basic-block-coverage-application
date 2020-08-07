/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 02:06:11 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.SinFunction;
import org.apache.commons.math.analysis.UnivariateRealFunction;
import org.apache.commons.math.analysis.solvers.AllowedSolution;
import org.apache.commons.math.analysis.solvers.BaseSecantSolver;
import org.apache.commons.math.analysis.solvers.RegulaFalsiSolver;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseSecantSolver_ESTest extends BaseSecantSolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      BaseSecantSolver.Method baseSecantSolver_Method0 = BaseSecantSolver.Method.PEGASUS;
      SinFunction sinFunction0 = new SinFunction();
      sinFunction0.derivative();
      AllowedSolution allowedSolution0 = AllowedSolution.ANY_SIDE;
      RegulaFalsiSolver regulaFalsiSolver0 = new RegulaFalsiSolver((-6033.824223485824), (-2294.73573759), (-6033.824223485824));
      regulaFalsiSolver0.solve(64, (UnivariateRealFunction) sinFunction0, 0.0, 6.911503837900322E-12, allowedSolution0);
      AllowedSolution allowedSolution1 = AllowedSolution.ABOVE_SIDE;
      // Undeclared exception!
      regulaFalsiSolver0.solve(64, (UnivariateRealFunction) sinFunction0, 2.776389480507507E-10, 4446.7, (-1462.43077125), allowedSolution1);
  }
}
