/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 21:10:17 UTC 2020
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
      int int0 = (-5886);
      BigFraction bigFraction0 = new BigFraction((-1689.6535078989066), (-5886));
      BigInteger bigInteger0 = BigInteger.TEN;
      BigFraction bigFraction1 = bigFraction0.subtract(bigInteger0);
      bigFraction1.abs();
      bigFraction1.doubleValue();
      BigFraction bigFraction2 = new BigFraction((-1700.0), (-2140));
  }
}
