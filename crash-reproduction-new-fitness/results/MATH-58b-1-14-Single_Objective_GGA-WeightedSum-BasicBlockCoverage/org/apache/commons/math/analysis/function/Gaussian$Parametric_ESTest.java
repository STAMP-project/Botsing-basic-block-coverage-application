/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 21:59:14 UTC 2020
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
      double[] doubleArray0 = new double[3];
      doubleArray0[0] = 0.036;
      doubleArray0[1] = 0.036;
      doubleArray0[2] = 0.036;
      gaussian_Parametric0.value(0.036, doubleArray0);
      double[] doubleArray1 = new double[9];
      doubleArray1[0] = 0.041666666666621166;
      doubleArray1[1] = 0.036;
      doubleArray1[2] = 0.036;
      double double0 = 2.0;
      doubleArray1[3] = 2.0;
      doubleArray1[4] = 0.036;
      doubleArray1[5] = 0.041666666666621166;
      doubleArray1[6] = 0.036;
      double[] doubleArray2 = new double[3];
      doubleArray2[0] = 2.0;
      doubleArray2[1] = 2202.4278683210614;
      doubleArray2[2] = (-207.605281549);
      // Undeclared exception!
      gaussian_Parametric0.value((-3014.463176268), doubleArray2);
  }
}
