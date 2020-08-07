/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 02:58:08 UTC 2020
 */

package org.apache.commons.math.analysis.function;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.analysis.function.Gaussian;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class Gaussian$Parametric_ESTest extends Gaussian$Parametric_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Gaussian.Parametric gaussian_Parametric0 = new Gaussian.Parametric();
      double[] doubleArray0 = new double[16];
      doubleArray0[0] = 0.0;
      double[] doubleArray1 = new double[3];
      doubleArray1[0] = (-3526.039);
      doubleArray1[1] = 0.0;
      doubleArray1[2] = 1520.5;
      gaussian_Parametric0.value((-3526.039), doubleArray1);
      double[] doubleArray2 = gaussian_Parametric0.gradient(1520.5, doubleArray1);
      // Undeclared exception!
      gaussian_Parametric0.value(1520.5, doubleArray2);
  }
}
