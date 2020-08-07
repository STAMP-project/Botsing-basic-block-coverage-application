/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 16:50:22 UTC 2020
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
import org.joda.time.DateTimeFieldType;
import org.joda.time.DateTimeZone;
import org.joda.time.DurationField;
import org.joda.time.chrono.BuddhistChronology;
import org.joda.time.format.DateTimeParserBucket;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class DateTimeParserBucket_ESTest extends DateTimeParserBucket_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      DurationField durationField0 = mock(DurationField.class, new ViolatedAssumptionAnswer());
      doReturn(false, false).when(durationField0).isSupported();
      DateTimeParserBucket.compareReverse(durationField0, durationField0);
      Chronology chronology0 = mock(Chronology.class, new ViolatedAssumptionAnswer());
      DurationField durationField1 = mock(DurationField.class, new ViolatedAssumptionAnswer());
      doReturn(false, false).when(durationField1).isSupported();
      DateTimeParserBucket.compareReverse(durationField1, durationField1);
      BuddhistChronology buddhistChronology0 = BuddhistChronology.getInstance();
      DurationField durationField2 = buddhistChronology0.years();
      DateTimeParserBucket.compareReverse(durationField2, durationField2);
      Locale locale0 = null;
      DateTimeParserBucket dateTimeParserBucket0 = new DateTimeParserBucket(1234L, buddhistChronology0, (Locale) null);
      DateTimeZone dateTimeZone0 = DateTimeZone.forOffsetHoursMinutes(0, (-1));
      BuddhistChronology buddhistChronology1 = BuddhistChronology.getInstance(dateTimeZone0);
      DateTimeZone dateTimeZone1 = buddhistChronology1.getZone();
      dateTimeParserBucket0.setZone(dateTimeZone1);
      DateTimeFieldType dateTimeFieldType0 = DateTimeFieldType.dayOfYear();
      dateTimeParserBucket0.saveField(dateTimeFieldType0, 0);
      // Undeclared exception!
      dateTimeParserBucket0.computeMillis(true);
  }
}
