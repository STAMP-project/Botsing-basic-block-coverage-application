/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 16:45:51 UTC 2020
 */

package org.joda.time;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.joda.time.Duration;
import org.joda.time.Period;
import org.joda.time.PeriodType;
import org.joda.time.ReadableDuration;
import org.joda.time.ReadableInstant;
import org.joda.time.Years;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class Period_ESTest extends Period_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      ReadableInstant readableInstant0 = null;
      Duration duration0 = new Duration(0L);
      Duration duration1 = Duration.standardDays(3775L);
      Duration duration2 = duration0.plus((ReadableDuration) duration1);
      Period period0 = new Period((ReadableInstant) null, duration2);
      int int0 = 0;
      Period period1 = period0.plusDays(0);
      Period period2 = period1.plusWeeks((-2860));
      Period period3 = period2.minus(period0);
      Years years0 = Years.TWO;
      period3.minus(years0);
      Period period4 = Period.months((-2860));
      PeriodType periodType0 = PeriodType.dayTime();
      PeriodType periodType1 = periodType0.withMinutesRemoved();
      Period period5 = period3.normalizedStandard(periodType1);
      period5.normalizedStandard(periodType1);
      period0.getValues();
      // Undeclared exception!
      period4.normalizedStandard(periodType1);
  }
}
