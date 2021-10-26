/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 20:43:03 UTC 2021
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
      double double0 = 1.0;
      Gamma.regularizedGammaP((-3378.361073761247), 1.0);
      double double1 = 1.0;
      int int0 = (-112);
      try { 
        Gamma.regularizedGammaP(1.0, 1.0, 1.0, (-112));
        fail("Expecting exception: MaxIterationsExceededException");
      
      } catch(MaxIterationsExceededException e) {
         //
         // Maximal number of iterations (-112) exceeded
         //
         verifyException("org.apache.commons.math.special.Gamma", e);
      }
  }
}
