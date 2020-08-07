/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 03:35:04 UTC 2020
 */

package org.apache.commons.math.distribution;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.distribution.FDistributionImpl;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class FDistributionImpl_ESTest extends FDistributionImpl_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      double double0 = Double.POSITIVE_INFINITY;
      double double1 = 2.0;
      FDistributionImpl fDistributionImpl0 = new FDistributionImpl(Double.POSITIVE_INFINITY, 2.0);
      fDistributionImpl0.getDenominatorDegreesOfFreedom();
      fDistributionImpl0.cumulativeProbability(2.0);
      fDistributionImpl0.getDenominatorDegreesOfFreedom();
      // Undeclared exception!
      fDistributionImpl0.inverseCumulativeProbability(Double.NaN);
  }
}
