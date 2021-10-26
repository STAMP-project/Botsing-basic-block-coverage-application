/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 21:32:55 UTC 2021
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
      FDistributionImpl fDistributionImpl0 = new FDistributionImpl(1.0E-15, 1.0E-15);
      fDistributionImpl0.setNumeratorDegreesOfFreedom(Double.NaN);
      double double0 = 0.0;
      // Undeclared exception!
      fDistributionImpl0.inverseCumulativeProbability(Double.NaN);
  }
}
