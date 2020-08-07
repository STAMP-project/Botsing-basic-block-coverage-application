/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 23:07:12 UTC 2020
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
      CauchyDistributionImpl cauchyDistributionImpl0 = new CauchyDistributionImpl();
      normalDistributionImpl0.setMean(2366.127487096709);
      normalDistributionImpl0.setMean(2366.127487096709);
      cauchyDistributionImpl0.getInitialDomain(2366.127487096709);
      FDistributionImpl fDistributionImpl0 = new FDistributionImpl(0.5, 1.0);
      fDistributionImpl0.inverseCumulativeProbability(1.0);
      FDistributionImpl fDistributionImpl1 = new FDistributionImpl(0.5, 368.803925517852);
      fDistributionImpl1.setDenominatorDegreesOfFreedom(1403.0);
      // Undeclared exception!
      fDistributionImpl0.inverseCumulativeProbability(1.0E-8);
  }
}
