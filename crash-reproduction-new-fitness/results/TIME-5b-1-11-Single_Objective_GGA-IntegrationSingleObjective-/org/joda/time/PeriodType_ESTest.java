/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 13:18:01 UTC 2020
 */

package org.joda.time;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.joda.time.Period;
import org.joda.time.PeriodType;
import org.joda.time.TestDuration_Basics;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class PeriodType_ESTest extends PeriodType_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      PeriodType periodType0 = PeriodType.days();
      PeriodType.time();
      periodType0.withYearsRemoved();
      PeriodType periodType1 = PeriodType.days();
      PeriodType.years();
      PeriodType periodType2 = PeriodType.years();
      PeriodType periodType3 = periodType2.withHoursRemoved();
      PeriodType.standard();
      periodType1.size();
      PeriodType.yearWeekDayTime();
      TestDuration_Basics testDuration_Basics0 = new TestDuration_Basics("");
      TestDuration_Basics.MockDuration testDuration_Basics_MockDuration0 = testDuration_Basics0.new MockDuration(869L);
      Period period0 = testDuration_Basics_MockDuration0.toPeriod();
      int[] intArray0 = new int[5];
      intArray0[0] = 1;
      intArray0[1] = 5;
      intArray0[2] = 2628;
      intArray0[3] = 1;
      intArray0[4] = 1;
      // Undeclared exception!
      periodType3.setIndexedField(period0, 5, intArray0, 1);
  }
}
