/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:36:42 UTC 2020
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
      BigFraction bigFraction0 = new BigFraction((-783.067309), 1410);
      bigFraction0.floatValue();
      BigFraction bigFraction1 = bigFraction0.subtract(485);
      bigFraction1.divide(1044);
      BigFraction bigFraction2 = bigFraction1.add(bigFraction0);
      bigFraction0.getDenominatorAsLong();
      BigFraction bigFraction3 = bigFraction2.subtract(bigFraction1);
      BigFraction bigFraction4 = bigFraction3.add(1044);
      BigInteger bigInteger0 = BigInteger.ONE;
      BigFraction bigFraction5 = bigFraction4.divide(bigInteger0);
      BigFraction bigFraction6 = bigFraction2.multiply(826);
      bigFraction5.toString();
      bigFraction6.equals(bigFraction3);
      BigFraction bigFraction7 = new BigFraction(1.7211089134216309, 1044);
      bigFraction6.floatValue();
      bigFraction5.getField();
      bigFraction2.intValue();
      BigFraction bigFraction8 = new BigFraction(bigInteger0, bigInteger0);
      bigFraction8.getNumerator();
      bigFraction6.abs();
      bigFraction1.multiply(0L);
      bigFraction5.pow((long) 1044);
      BigFraction bigFraction9 = new BigFraction(0.0, 2249);
  }
}
