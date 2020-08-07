/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 04:07:57 UTC 2020
 */

package org.joda.time.chrono;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.joda.time.DateTimeZone;
import org.joda.time.DurationFieldType;
import org.joda.time.Instant;
import org.joda.time.LocalDateTime;
import org.joda.time.LocalTime;
import org.joda.time.Period;
import org.joda.time.chrono.GJChronology;
import org.joda.time.chrono.IslamicChronology;
import org.joda.time.tz.FixedDateTimeZone;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseChronology_ESTest extends BaseChronology_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      FixedDateTimeZone fixedDateTimeZone0 = (FixedDateTimeZone)DateTimeZone.UTC;
      Instant instant0 = GJChronology.DEFAULT_CUTOVER;
      Instant instant1 = new Instant();
      IslamicChronology islamicChronology0 = IslamicChronology.getInstance();
      Period period0 = Period.ZERO;
      Instant instant2 = new Instant(1L);
      IslamicChronology islamicChronology1 = IslamicChronology.getInstanceUTC();
      LocalTime localTime0 = new LocalTime((DateTimeZone) fixedDateTimeZone0);
      LocalTime localTime1 = localTime0.minusMillis((-2953));
      islamicChronology1.set(localTime1, (-661L));
      Period period1 = Period.ZERO;
      LocalDateTime localDateTime0 = new LocalDateTime((DateTimeZone) fixedDateTimeZone0);
      LocalDateTime localDateTime1 = new LocalDateTime((-71522593L));
      LocalDateTime localDateTime2 = localDateTime0.plusWeeks((-1793));
      DurationFieldType durationFieldType0 = DurationFieldType.years();
      durationFieldType0.getField(islamicChronology0);
      int int0 = 280;
      localDateTime2.withFieldAdded(durationFieldType0, 280);
      instant1.isBefore((-6L));
      // Undeclared exception!
      islamicChronology1.set(localDateTime1, 1L);
  }
}
