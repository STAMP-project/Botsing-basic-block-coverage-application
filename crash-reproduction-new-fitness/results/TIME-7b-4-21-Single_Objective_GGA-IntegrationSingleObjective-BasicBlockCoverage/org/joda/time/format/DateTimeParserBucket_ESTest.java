/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 17:45:45 UTC 2020
 */

package org.joda.time.format;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.Locale;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.joda.time.Chronology;
import org.joda.time.DateTimeField;
import org.joda.time.DateTimeFieldType;
import org.joda.time.DateTimeZone;
import org.joda.time.Period;
import org.joda.time.chrono.BuddhistChronology;
import org.joda.time.field.MillisDurationField;
import org.joda.time.format.DateTimeParserBucket;
import org.joda.time.tz.UTCProvider;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class DateTimeParserBucket_ESTest extends DateTimeParserBucket_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      MillisDurationField millisDurationField0 = (MillisDurationField)MillisDurationField.INSTANCE;
      DateTimeParserBucket.compareReverse(millisDurationField0, millisDurationField0);
      DateTimeParserBucket dateTimeParserBucket0 = new DateTimeParserBucket(0, (Chronology) null, (Locale) null);
      UTCProvider uTCProvider0 = new UTCProvider();
      DateTimeZone dateTimeZone0 = dateTimeParserBucket0.getZone();
      DateTimeFieldType dateTimeFieldType0 = DateTimeFieldType.minuteOfDay();
      dateTimeFieldType0.getField((Chronology) null);
      dateTimeParserBucket0.saveField(dateTimeFieldType0, 0);
      Period.months(Integer.MIN_VALUE);
      DateTimeZone dateTimeZone1 = DateTimeZone.forOffsetHours(0);
      dateTimeParserBucket0.setOffset(64);
      dateTimeParserBucket0.setZone(dateTimeZone0);
      DateTimeZone dateTimeZone2 = DateTimeZone.getDefault();
      BuddhistChronology buddhistChronology0 = BuddhistChronology.getInstanceUTC();
      BuddhistChronology.getInstance(dateTimeZone1);
      buddhistChronology0.getZone();
      DateTimeField dateTimeField0 = buddhistChronology0.dayOfMonth();
      dateTimeParserBucket0.saveField(dateTimeField0, 0);
      DateTimeZone.forOffsetHoursMinutes(0, 0);
      Locale locale0 = Locale.KOREA;
      Integer integer0 = new Integer(Integer.MIN_VALUE);
      dateTimeParserBucket0.getOffset();
      dateTimeParserBucket0.setZone(dateTimeZone2);
      buddhistChronology0.year();
      // Undeclared exception!
      dateTimeParserBucket0.computeMillis();
  }
}
