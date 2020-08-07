/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 21:10:34 UTC 2020
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
      BigFraction bigFraction0 = new BigFraction(1877.9, 82);
      bigFraction0.getNumerator();
      BigFraction bigFraction1 = new BigFraction(82, (-3531L));
      BigFraction bigFraction2 = bigFraction0.multiply(1);
      bigFraction2.toString();
      BigFraction bigFraction3 = new BigFraction(1877.9, 154);
      BigFraction bigFraction4 = new BigFraction((double) 82, 82);
  }
}
