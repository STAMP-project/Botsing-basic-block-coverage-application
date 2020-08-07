/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:36:50 UTC 2020
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
      bigFraction0.percentageValue();
      bigFraction0.negate();
      int int0 = (-1611);
      BigFraction bigFraction1 = new BigFraction(3299.6803209238, (-1611));
      bigFraction1.intValue();
      BigFraction bigFraction2 = new BigFraction((double) 3299, (-1611));
  }
}
