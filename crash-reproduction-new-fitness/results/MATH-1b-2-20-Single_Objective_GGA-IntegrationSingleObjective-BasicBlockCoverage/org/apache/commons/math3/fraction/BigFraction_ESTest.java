/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 02:10:07 UTC 2020
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
      BigFraction bigFraction0 = BigFraction.TWO_THIRDS;
      BigInteger bigInteger0 = BigInteger.ONE;
      bigFraction0.pow(bigInteger0);
      BigFraction bigFraction1 = new BigFraction(1.6455042362213135, 266);
      bigFraction0.shortValue();
      bigFraction0.getDenominatorAsInt();
      bigFraction0.getDenominatorAsInt();
      bigFraction0.negate();
      BigFraction bigFraction2 = new BigFraction((double) 266, 3);
  }
}
