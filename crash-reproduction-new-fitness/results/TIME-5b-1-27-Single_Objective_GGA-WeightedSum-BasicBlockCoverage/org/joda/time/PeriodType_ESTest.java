/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 16:46:28 UTC 2020
 */

package org.joda.time;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.joda.time.PeriodType;
import org.joda.time.ReadablePeriod;
import org.joda.time.TestInterval_Constructors;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class PeriodType_ESTest extends PeriodType_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      PeriodType.seconds();
      PeriodType.millis();
      PeriodType.time();
      PeriodType periodType0 = PeriodType.seconds();
      PeriodType periodType1 = PeriodType.hours();
      TestInterval_Constructors testInterval_Constructors0 = new TestInterval_Constructors(";Nvi[McGG");
      TestInterval_Constructors.MockInterval testInterval_Constructors_MockInterval0 = testInterval_Constructors0.new MockInterval();
      testInterval_Constructors_MockInterval0.toPeriod(periodType0);
      int[] intArray0 = new int[8];
      intArray0[0] = 0;
      intArray0[1] = 0;
      intArray0[2] = 0;
      intArray0[3] = 0;
      intArray0[4] = 0;
      intArray0[5] = 0;
      intArray0[6] = 0;
      intArray0[7] = 0;
      // Undeclared exception!
      periodType1.setIndexedField((ReadablePeriod) null, 0, intArray0, 0);
  }
}
