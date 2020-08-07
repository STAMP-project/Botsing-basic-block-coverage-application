/*
 * This file was automatically generated by EvoSuite
 * Fri May 15 12:24:13 UTC 2020
 */

package org.joda.time.format;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.Locale;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.joda.time.Chronology;
import org.joda.time.DateTimeField;
import org.joda.time.DateTimeFieldType;
import org.joda.time.DateTimeZone;
import org.joda.time.DurationField;
import org.joda.time.chrono.BuddhistChronology;
import org.joda.time.field.DelegatedDateTimeField;
import org.joda.time.format.DateTimeParserBucket;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class DateTimeParserBucket_ESTest extends DateTimeParserBucket_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Chronology chronology0 = mock(Chronology.class, new ViolatedAssumptionAnswer());
      doReturn((DateTimeZone) null).when(chronology0).getZone();
      doReturn((Chronology) null).when(chronology0).withUTC();
      long long0 = (-52L);
      Locale locale0 = null;
      DateTimeParserBucket dateTimeParserBucket0 = new DateTimeParserBucket((-52L), chronology0, (Locale) null);
      dateTimeParserBucket0.getChronology();
      DateTimeParserBucket.SavedState dateTimeParserBucket_SavedState0 = dateTimeParserBucket0.new SavedState();
      DateTimeZone dateTimeZone0 = dateTimeParserBucket_SavedState0.iZone;
      BuddhistChronology buddhistChronology0 = BuddhistChronology.getInstance((DateTimeZone) null);
      DateTimeField dateTimeField0 = buddhistChronology0.dayOfWeek();
      DateTimeFieldType dateTimeFieldType0 = DateTimeFieldType.dayOfWeek();
      DelegatedDateTimeField delegatedDateTimeField0 = new DelegatedDateTimeField(dateTimeField0, dateTimeFieldType0);
      DurationField durationField0 = delegatedDateTimeField0.getDurationField();
      DateTimeParserBucket.compareReverse(durationField0, durationField0);
      DateTimeFieldType.minuteOfHour();
      dateTimeParserBucket0.saveField(dateTimeFieldType0, 23118750);
      // Undeclared exception!
      dateTimeParserBucket0.computeMillis(true, "9;9!K*KzZ\"gM;PdjA");
  }
}
