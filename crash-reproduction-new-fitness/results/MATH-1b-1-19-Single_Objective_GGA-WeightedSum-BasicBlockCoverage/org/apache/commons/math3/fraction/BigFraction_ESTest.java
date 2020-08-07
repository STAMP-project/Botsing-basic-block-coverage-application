/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:36:21 UTC 2020
 */

package org.apache.commons.math3.fraction;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.math.BigInteger;
import org.apache.commons.math3.fraction.BigFraction;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BigFraction_ESTest extends BigFraction_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      BigFraction bigFraction0 = BigFraction.MINUS_ONE;
      BigInteger bigInteger0 = BigInteger.TEN;
      BigFraction bigFraction1 = new BigFraction(bigInteger0, bigInteger0);
      bigFraction1.reciprocal();
      BigFraction bigFraction2 = new BigFraction(2851.5, 2471);
      int int0 = (-725);
      BigFraction bigFraction3 = new BigFraction((-725), (-725));
      bigFraction2.getField();
      int int1 = 1764;
      BigFraction bigFraction4 = new BigFraction((double) 2471, 1764);
  }
}
