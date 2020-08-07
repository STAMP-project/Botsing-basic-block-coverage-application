/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 21:08:47 UTC 2020
 */

package org.apache.commons.math.special;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.MaxIterationsExceededException;
import org.apache.commons.math.special.Gamma;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class Gamma_ESTest extends Gamma_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Gamma.regularizedGammaP((-991.99), (-1.0), (-991.99), (-1878));
      Gamma.regularizedGammaP(Double.NaN, 0.0);
      double double0 = (-2515.97863173212);
      Gamma.regularizedGammaP((-2515.97863173212), (-1.0), Double.NaN, (-1878));
      double double1 = 739.82096;
      int int0 = (-2826);
      try { 
        Gamma.regularizedGammaP(739.82096, 739.82096, (-2515.97863173212), (-2826));
        fail("Expecting exception: MaxIterationsExceededException");
      
      } catch(MaxIterationsExceededException e) {
         //
         // Maximal number of iterations (-2,826) exceeded
         //
         verifyException("org.apache.commons.math.special.Gamma", e);
      }
  }
}
