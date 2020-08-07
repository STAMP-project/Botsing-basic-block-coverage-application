/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 22:28:04 UTC 2020
 */

package org.apache.commons.math.distribution;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
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
      normalDistributionImpl0.cumulativeProbability(0.0);
      normalDistributionImpl0.inverseCumulativeProbability(0.5);
      Double double0 = new Double(0.0);
      double double1 = (-0.5969450062048659);
      Double double2 = new Double((-1.81));
      normalDistributionImpl0.cumulativeProbability(0.5);
      FDistributionImpl fDistributionImpl0 = new FDistributionImpl(373.9515181385624, 8.441822398385275E-5);
      // Undeclared exception!
      fDistributionImpl0.inverseCumulativeProbability(8.441822398385275E-5);
  }
}
