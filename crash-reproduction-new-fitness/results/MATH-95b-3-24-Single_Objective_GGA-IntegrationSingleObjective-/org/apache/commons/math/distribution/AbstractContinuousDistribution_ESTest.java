/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 03:36:39 UTC 2020
 */

package org.apache.commons.math.distribution;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.math.distribution.NormalDistributionImpl;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class AbstractContinuousDistribution_ESTest extends AbstractContinuousDistribution_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      NormalDistributionImpl normalDistributionImpl0 = new NormalDistributionImpl();
      Double double0 = new Double((-1.0));
      Double double1 = new Double((-1.0));
      double double2 = 0.5;
      Double double3 = new Double((-1813.72915398));
      Double double4 = new Double((-1813.72915398));
      double double5 = 1.7976931348623157E308;
      normalDistributionImpl0.setMean(1.7976931348623157E308);
      // Undeclared exception!
      normalDistributionImpl0.inverseCumulativeProbability(0.5);
  }
}
