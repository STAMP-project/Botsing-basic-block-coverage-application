/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 18:24:38 UTC 2020
 */

package org.joda.time;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.joda.time.DateTime;
import org.joda.time.DateTimeZone;
import org.joda.time.Days;
import org.joda.time.MutableDateTime;
import org.joda.time.PeriodType;
import org.joda.time.ReadableInstant;
import org.joda.time.TestDateMidnight_Basics;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class PeriodType_ESTest extends PeriodType_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      PeriodType periodType0 = PeriodType.millis();
      periodType0.withSecondsRemoved();
      PeriodType periodType1 = PeriodType.hours();
      PeriodType periodType2 = PeriodType.time();
      PeriodType periodType3 = periodType2.withHoursRemoved();
      periodType3.equals(periodType1);
      PeriodType periodType4 = periodType3.withMinutesRemoved();
      PeriodType.yearWeekDay();
      periodType4.toString();
      periodType1.withWeeksRemoved();
      periodType0.toString();
      PeriodType.days();
      periodType4.getFieldType(0);
      TestDateMidnight_Basics testDateMidnight_Basics0 = new TestDateMidnight_Basics("PeriodType[Millis]");
      TestDateMidnight_Basics.MockInstant testDateMidnight_Basics_MockInstant0 = testDateMidnight_Basics0.new MockInstant();
      DateTime dateTime0 = testDateMidnight_Basics_MockInstant0.toDateTime();
      DateTimeZone dateTimeZone0 = DateTimeZone.getDefault();
      MutableDateTime mutableDateTime0 = dateTime0.toMutableDateTime(dateTimeZone0);
      Days days0 = Days.daysBetween((ReadableInstant) mutableDateTime0, (ReadableInstant) dateTime0);
      int[] intArray0 = new int[6];
      intArray0[0] = 0;
      intArray0[1] = 0;
      intArray0[2] = 46;
      intArray0[3] = 0;
      intArray0[4] = (-660);
      intArray0[5] = 0;
      // Undeclared exception!
      periodType2.setIndexedField(days0, 0, intArray0, 0);
  }
}
