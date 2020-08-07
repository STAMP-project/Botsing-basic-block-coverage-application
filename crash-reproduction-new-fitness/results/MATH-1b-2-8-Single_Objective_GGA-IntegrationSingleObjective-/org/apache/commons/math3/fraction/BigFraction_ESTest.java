/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 21:50:34 UTC 2020
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
      BigFraction bigFraction0 = BigFraction.MINUS_ONE;
      BigFraction bigFraction1 = bigFraction0.abs();
      bigFraction1.floatValue();
      BigFraction bigFraction2 = bigFraction0.pow(13);
      int int0 = (-56);
      BigFraction bigFraction3 = new BigFraction(13, 102.5778359846962, (-56));
      bigFraction2.subtract((-56));
      bigFraction0.getNumeratorAsLong();
      bigFraction3.subtract(13);
      BigFraction bigFraction4 = bigFraction3.reciprocal();
      bigFraction4.getDenominator();
      BigFraction bigFraction5 = new BigFraction(0.0, 15);
  }
}
