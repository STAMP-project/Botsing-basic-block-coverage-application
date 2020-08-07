/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 18:27:31 UTC 2020
 */

package org.joda.time;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.joda.time.Period;
import org.joda.time.PeriodType;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class PeriodType_ESTest extends PeriodType_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      PeriodType periodType0 = PeriodType.seconds();
      Period period0 = new Period(15778800000L, 15778800000L);
      int[] intArray0 = new int[9];
      intArray0[0] = 4;
      intArray0[1] = 4;
      intArray0[2] = 4;
      intArray0[3] = 1;
      intArray0[4] = 2201;
      intArray0[5] = 4;
      intArray0[6] = (-2305);
      intArray0[7] = 4;
      intArray0[8] = 4;
      // Undeclared exception!
      periodType0.setIndexedField(period0, 4, intArray0, 4);
  }
}
