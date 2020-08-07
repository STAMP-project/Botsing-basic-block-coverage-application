/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 14:20:54 BST 2020
 */

package org.joda.time;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.joda.time.Chronology;
import org.joda.time.DateTime;
import org.joda.time.DateTimeZone;
import org.joda.time.Duration;
import org.joda.time.Period;
import org.joda.time.PeriodType;
import org.joda.time.ReadableInstant;
import org.joda.time.TestDuration_Basics;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class Period_ESTest extends Period_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      DateTime dateTime0 = DateTime.now();
      DateTimeZone dateTimeZone0 = DateTimeZone.getDefault();
      DateTime dateTime1 = dateTime0.toDateTime(dateTimeZone0);
      Period period0 = Period.ZERO;
      PeriodType periodType0 = PeriodType.weeks();
      PeriodType periodType1 = periodType0.withYearsRemoved();
      dateTime1.getMinuteOfDay();
      PeriodType periodType2 = PeriodType.months();
      PeriodType.yearWeekDayTime();
      dateTime1.toDateTime(dateTimeZone0);
      Period period1 = Period.ZERO;
      PeriodType.weeks();
      periodType1.withMonthsRemoved();
      Object object0 = new Object();
      PeriodType.standard();
      Duration duration0 = Duration.ZERO;
      TestDuration_Basics.MockMutableDuration testDuration_Basics_MockMutableDuration0 = new TestDuration_Basics.MockMutableDuration(0L);
      testDuration_Basics_MockMutableDuration0.toDuration();
      Duration duration1 = duration0.negated();
      duration1.toDuration();
      Period period2 = testDuration_Basics_MockMutableDuration0.toPeriod(periodType2, (Chronology) null);
      duration1.toPeriodFrom((ReadableInstant) dateTime1);
      // Undeclared exception!
      period2.withYears((-1726));
  }
}
