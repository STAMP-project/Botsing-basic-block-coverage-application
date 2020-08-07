/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 03:31:04 UTC 2020
 */

package org.apache.commons.math.distribution;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.distribution.CauchyDistributionImpl;
import org.apache.commons.math.distribution.FDistributionImpl;
import org.apache.commons.math.distribution.NormalDistributionImpl;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class AbstractContinuousDistribution_ESTest extends AbstractContinuousDistribution_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      NormalDistributionImpl normalDistributionImpl0 = new NormalDistributionImpl();
      CauchyDistributionImpl cauchyDistributionImpl0 = new CauchyDistributionImpl(0.5, 0.5);
      double double0 = 0.5;
      normalDistributionImpl0.inverseCumulativeProbability(0.5);
      FDistributionImpl fDistributionImpl0 = new FDistributionImpl(4.9E-324, 0.5);
      // Undeclared exception!
      fDistributionImpl0.inverseCumulativeProbability(0.5);
  }
}
