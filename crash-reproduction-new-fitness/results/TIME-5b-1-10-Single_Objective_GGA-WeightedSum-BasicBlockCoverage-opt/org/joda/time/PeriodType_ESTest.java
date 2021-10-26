/*
 * This file was automatically generated by EvoSuite
 * Tue Oct 26 06:34:52 UTC 2021
 */

package org.joda.time;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.joda.time.DurationFieldType;
import org.joda.time.PeriodType;
import org.joda.time.ReadablePeriod;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class PeriodType_ESTest extends PeriodType_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      PeriodType periodType0 = PeriodType.millis();
      DurationFieldType durationFieldType0 = DurationFieldType.DAYS_TYPE;
      periodType0.indexOf(durationFieldType0);
      periodType0.getName();
      PeriodType.yearMonthDayTime();
      PeriodType.hours();
      PeriodType.hours();
      PeriodType periodType1 = PeriodType.yearDay();
      int[] intArray0 = new int[5];
      intArray0[0] = 1;
      intArray0[1] = (-1);
      intArray0[2] = 1;
      intArray0[3] = 1;
      intArray0[4] = (-1);
      // Undeclared exception!
      periodType1.setIndexedField((ReadablePeriod) null, 1, intArray0, 2145);
  }
}
