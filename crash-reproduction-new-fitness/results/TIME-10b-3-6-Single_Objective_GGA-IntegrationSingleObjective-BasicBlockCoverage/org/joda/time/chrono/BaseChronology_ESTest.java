/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 00:20:35 UTC 2020
 */

package org.joda.time.chrono;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.joda.time.Chronology;
import org.joda.time.DateMidnight;
import org.joda.time.DateTime;
import org.joda.time.DateTimeZone;
import org.joda.time.Interval;
import org.joda.time.LocalDateTime;
import org.joda.time.Minutes;
import org.joda.time.chrono.BuddhistChronology;
import org.joda.time.chrono.IslamicChronology;
import org.joda.time.tz.UTCProvider;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseChronology_ESTest extends BaseChronology_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      BuddhistChronology buddhistChronology0 = BuddhistChronology.getInstance();
      DateMidnight dateMidnight0 = DateMidnight.now();
      DateMidnight dateMidnight1 = dateMidnight0.minusYears(12);
      LocalDateTime localDateTime0 = new LocalDateTime((long) 12);
      DateMidnight dateMidnight2 = DateMidnight.now();
      UTCProvider uTCProvider0 = new UTCProvider();
      UTCProvider uTCProvider1 = new UTCProvider();
      uTCProvider1.getZone("");
      dateMidnight1.withZoneRetainFields((DateTimeZone) null);
      dateMidnight0.monthOfYear();
      dateMidnight2.minusYears((-423));
      LocalDateTime localDateTime1 = new LocalDateTime((-2067L));
      Interval interval0 = dateMidnight2.toInterval();
      dateMidnight1.toDateTime((Chronology) buddhistChronology0);
      buddhistChronology0.set(localDateTime0, (-423));
      IslamicChronology.LeapYearPatternType islamicChronology_LeapYearPatternType0 = IslamicChronology.LEAP_YEAR_16_BASED;
      IslamicChronology islamicChronology0 = IslamicChronology.getInstance((DateTimeZone) null, islamicChronology_LeapYearPatternType0);
      DateMidnight dateMidnight3 = new DateMidnight((Chronology) islamicChronology0);
      DateTime dateTime0 = dateMidnight2.toDateTime((Chronology) islamicChronology0);
      Minutes minutes0 = Minutes.minutesIn(interval0);
      minutes0.multipliedBy(675);
      // Undeclared exception!
      dateTime0.withFields(localDateTime1);
  }
}
