/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 21:46:43 UTC 2020
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
      double double0 = (-962.62992737955);
      int int0 = 1;
      Gamma.regularizedGammaQ((-962.62992737955), (-962.62992737955), 0.0, 1);
      double double1 = 399.7806051024635;
      Gamma.regularizedGammaP(0.0, 859.648382, 399.7806051024635, 1);
      Gamma.logGamma(1);
      double double2 = 1.7976931348623157E308;
      try { 
        Gamma.regularizedGammaP(1.7976931348623157E308, 1.7976931348623157E308, (-1538.0), 1);
        fail("Expecting exception: MaxIterationsExceededException");
      
      } catch(MaxIterationsExceededException e) {
         //
         // Maximal number of iterations (1) exceeded
         //
         verifyException("org.apache.commons.math.special.Gamma", e);
      }
  }
}
