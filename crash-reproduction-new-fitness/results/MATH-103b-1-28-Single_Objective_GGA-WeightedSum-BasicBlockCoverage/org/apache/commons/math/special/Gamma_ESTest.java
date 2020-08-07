/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:34:57 UTC 2020
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
      int int0 = 1;
      Gamma.regularizedGammaP(2586.99687988365, 2586.99687988365, 4430.15961902566, 1);
      double double0 = 0.0;
      Gamma.regularizedGammaP((double) 1, 0.0);
      try { 
        Gamma.regularizedGammaP(0.007843287303051467, (double) 1, 0.0, 1);
        fail("Expecting exception: MaxIterationsExceededException");
      
      } catch(MaxIterationsExceededException e) {
         //
         // Maximal number of iterations (1) exceeded
         //
         verifyException("org.apache.commons.math.special.Gamma", e);
      }
  }
}
