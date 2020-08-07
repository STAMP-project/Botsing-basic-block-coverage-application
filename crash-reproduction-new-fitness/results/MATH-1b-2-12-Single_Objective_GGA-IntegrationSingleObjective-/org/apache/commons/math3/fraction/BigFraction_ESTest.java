/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 21:50:44 UTC 2020
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
      BigFraction bigFraction0 = BigFraction.ONE_HALF;
      bigFraction0.longValue();
      double double0 = 5.9863754;
      int int0 = (-3345);
      BigFraction bigFraction1 = new BigFraction(5.9863754, (-3345));
      bigFraction1.getDenominatorAsLong();
      bigFraction0.doubleValue();
      bigFraction0.equals(bigFraction1);
      BigFraction bigFraction2 = BigFraction.TWO_FIFTHS;
      BigFraction bigFraction3 = new BigFraction(1L, 0.0, (-3345));
  }
}
