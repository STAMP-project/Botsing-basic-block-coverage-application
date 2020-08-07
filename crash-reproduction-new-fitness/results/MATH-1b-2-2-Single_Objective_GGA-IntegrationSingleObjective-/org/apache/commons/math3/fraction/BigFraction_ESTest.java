/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 21:50:05 UTC 2020
 */

package org.apache.commons.math3.fraction;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math3.fraction.BigFraction;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BigFraction_ESTest extends BigFraction_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      BigFraction bigFraction0 = BigFraction.TWO;
      bigFraction0.abs();
      int int0 = 1670;
      int int1 = 1216;
      BigFraction bigFraction1 = new BigFraction(1670, 1216);
      BigFraction bigFraction2 = new BigFraction((double) 1670, 1670);
  }
}
