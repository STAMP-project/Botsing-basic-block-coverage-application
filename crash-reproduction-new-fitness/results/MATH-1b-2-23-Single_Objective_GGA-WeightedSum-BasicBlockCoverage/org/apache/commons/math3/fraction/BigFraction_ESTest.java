/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:36:33 UTC 2020
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
      BigFraction bigFraction0 = new BigFraction((-2.3401958604540354E-7), 408);
      bigFraction0.intValue();
      BigFraction bigFraction1 = bigFraction0.negate();
      BigFraction bigFraction2 = bigFraction1.subtract((long) 408);
      BigInteger bigInteger0 = bigFraction2.getNumerator();
      BigFraction bigFraction3 = new BigFraction(bigInteger0, bigInteger0);
      BigFraction bigFraction4 = bigFraction0.subtract(bigFraction1);
      BigFraction bigFraction5 = bigFraction2.add(0);
      bigFraction5.pow(1129.8477389985);
      bigFraction2.equals(bigInteger0);
      bigFraction4.equals(bigFraction3);
      bigFraction0.hashCode();
      BigFraction bigFraction6 = new BigFraction(1224.9, 0);
      BigFraction bigFraction7 = new BigFraction(408);
      bigFraction6.byteValue();
      int int0 = 1;
      BigFraction bigFraction8 = new BigFraction((double) 408, 1);
  }
}
