/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 22:26:47 UTC 2020
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
      double double0 = 0.5;
      FDistributionImpl fDistributionImpl0 = new FDistributionImpl(0.5, 0.5);
      fDistributionImpl0.getNumeratorDegreesOfFreedom();
      // Undeclared exception!
      fDistributionImpl0.inverseCumulativeProbability(0.5);
  }
}
