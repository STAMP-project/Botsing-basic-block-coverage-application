/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 22:29:29 UTC 2020
 */

package org.apache.commons.math.distribution;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.distribution.BetaDistributionImpl;
import org.apache.commons.math.distribution.NormalDistributionImpl;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class AbstractContinuousDistribution_ESTest extends AbstractContinuousDistribution_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      NormalDistributionImpl normalDistributionImpl0 = new NormalDistributionImpl();
      BetaDistributionImpl betaDistributionImpl0 = new BetaDistributionImpl((-0.53), (-0.53));
      betaDistributionImpl0.cumulativeProbability(1.0E-6, 1.0E-6);
      betaDistributionImpl0.cumulativeProbability(1.0E-6);
      betaDistributionImpl0.inverseCumulativeProbability(Double.NaN);
      betaDistributionImpl0.cumulativeProbability(Double.NaN, 2261.135663220516);
      Double double0 = new Double((-218.0));
      NormalDistributionImpl normalDistributionImpl1 = new NormalDistributionImpl(1.7976931348623157E308, 1.0E-8);
      // Undeclared exception!
      normalDistributionImpl1.inverseCumulativeProbability(Double.NaN);
  }
}
