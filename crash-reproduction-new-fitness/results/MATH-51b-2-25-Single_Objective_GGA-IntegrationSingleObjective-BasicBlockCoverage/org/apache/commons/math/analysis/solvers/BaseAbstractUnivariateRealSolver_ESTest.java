/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 02:43:59 UTC 2020
 */

package org.apache.commons.math.analysis.solvers;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.DifferentiableUnivariateRealFunction;
import org.apache.commons.math.analysis.polynomials.PolynomialFunction;
import org.apache.commons.math.analysis.solvers.BaseSecantSolver;
import org.apache.commons.math.analysis.solvers.NewtonSolver;
import org.apache.commons.math.analysis.solvers.PegasusSolver;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseAbstractUnivariateRealSolver_ESTest extends BaseAbstractUnivariateRealSolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      PegasusSolver pegasusSolver0 = new PegasusSolver(20.327578817169947, 20.327578817169947, 20.327578817169947);
      double[] doubleArray0 = new double[4];
      PolynomialFunction polynomialFunction0 = new PolynomialFunction(doubleArray0);
      PolynomialFunction polynomialFunction1 = new PolynomialFunction(doubleArray0);
      polynomialFunction0.subtract(polynomialFunction1);
      PolynomialFunction polynomialFunction2 = new PolynomialFunction(doubleArray0);
      polynomialFunction0.subtract(polynomialFunction2);
      polynomialFunction0.polynomialDerivative();
      polynomialFunction0.derivative();
      NewtonSolver newtonSolver0 = new NewtonSolver();
      double double0 = BaseSecantSolver.DEFAULT_ABSOLUTE_ACCURACY;
      // Undeclared exception!
      newtonSolver0.solve(0, (DifferentiableUnivariateRealFunction) polynomialFunction1, 1.0E-6, 1.0E-6);
  }
}
