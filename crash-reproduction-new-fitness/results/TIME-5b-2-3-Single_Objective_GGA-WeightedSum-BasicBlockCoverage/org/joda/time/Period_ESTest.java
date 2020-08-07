/*
 * This file was automatically generated by EvoSuite
 * Fri May 15 12:18:24 UTC 2020
 */

package org.joda.time;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.joda.time.DateTime;
import org.joda.time.Hours;
import org.joda.time.LocalDateTime;
import org.joda.time.Period;
import org.joda.time.PeriodType;
import org.joda.time.TestInterval_Basics;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class Period_ESTest extends Period_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Hours hours0 = Hours.MIN_VALUE;
      hours0.toStandardDuration();
      TestInterval_Basics testInterval_Basics0 = new TestInterval_Basics("N|a9");
      TestInterval_Basics.MockInterval testInterval_Basics_MockInterval0 = testInterval_Basics0.new MockInterval();
      DateTime dateTime0 = testInterval_Basics_MockInterval0.getStart();
      LocalDateTime localDateTime0 = dateTime0.toLocalDateTime();
      PeriodType periodType0 = PeriodType.millis();
      PeriodType periodType1 = periodType0.withWeeksRemoved();
      Period period0 = new Period(localDateTime0, localDateTime0, periodType1);
      Period period1 = new Period(0L);
      int int0 = 421;
      Period.months(421);
      period1.plus(hours0);
      // Undeclared exception!
      period0.withYears((-1945));
  }
}
