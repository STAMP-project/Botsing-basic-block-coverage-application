/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 03:35:29 UTC 2020
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
      FDistributionImpl fDistributionImpl0 = new FDistributionImpl(1.0E-6, 1.0E-6);
      fDistributionImpl0.cumulativeProbability(1.0E-6);
      fDistributionImpl0.getNumeratorDegreesOfFreedom();
      fDistributionImpl0.getDomainUpperBound(0.4999965461344935);
      fDistributionImpl0.getInitialDomain((-4481.0));
      // Undeclared exception!
      fDistributionImpl0.inverseCumulativeProbability(1.0E-6);
  }
}
