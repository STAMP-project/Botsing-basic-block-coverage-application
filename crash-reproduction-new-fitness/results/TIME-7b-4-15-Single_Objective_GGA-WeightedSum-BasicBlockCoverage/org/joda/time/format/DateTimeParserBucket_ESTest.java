/*
 * This file was automatically generated by EvoSuite
 * Fri May 15 12:24:31 UTC 2020
 */

package org.joda.time.format;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.LinkedHashSet;
import java.util.Locale;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.joda.time.DateTimeFieldType;
import org.joda.time.DateTimeZone;
import org.joda.time.LocalTime;
import org.joda.time.ReadablePeriod;
import org.joda.time.chrono.BuddhistChronology;
import org.joda.time.format.DateTimeParserBucket;
import org.joda.time.tz.FixedDateTimeZone;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class DateTimeParserBucket_ESTest extends DateTimeParserBucket_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      FixedDateTimeZone fixedDateTimeZone0 = (FixedDateTimeZone)DateTimeZone.UTC;
      LocalTime localTime0 = new LocalTime();
      LocalTime localTime1 = LocalTime.now();
      Locale locale0 = Locale.TAIWAN;
      LinkedHashSet<Character> linkedHashSet0 = new LinkedHashSet<Character>();
      localTime1.getMillisOfDay();
      BuddhistChronology buddhistChronology0 = BuddhistChronology.getInstance();
      localTime1.withPeriodAdded((ReadablePeriod) null, 44667238);
      buddhistChronology0.set(localTime0, 28L);
      Integer.sum(28, (-72));
      DateTimeParserBucket dateTimeParserBucket0 = new DateTimeParserBucket(44667238, buddhistChronology0, locale0);
      DateTimeFieldType dateTimeFieldType0 = DateTimeFieldType.dayOfMonth();
      dateTimeParserBucket0.saveField(dateTimeFieldType0, 2934);
      // Undeclared exception!
      dateTimeParserBucket0.computeMillis(true, "");
  }
}
