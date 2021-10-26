/*
 * This file was automatically generated by EvoSuite
 * Tue Oct 26 06:35:35 UTC 2021
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
import org.joda.time.Period;
import org.joda.time.ReadablePeriod;
import org.joda.time.chrono.IslamicChronology;
import org.joda.time.format.DateTimeParserBucket;
import org.joda.time.tz.FixedDateTimeZone;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class DateTimeParserBucket_ESTest extends DateTimeParserBucket_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      DateTimeParserBucket.SavedField[] dateTimeParserBucket_SavedFieldArray0 = new DateTimeParserBucket.SavedField[15];
      Chronology chronology0 = mock(Chronology.class, new ViolatedAssumptionAnswer());
      doReturn((DateTimeZone) null).when(chronology0).getZone();
      doReturn((Chronology) null).when(chronology0).withUTC();
      DateTimeParserBucket dateTimeParserBucket0 = new DateTimeParserBucket((-64L), chronology0, (Locale) null, (Integer) null);
      IslamicChronology islamicChronology0 = IslamicChronology.getInstance();
      FixedDateTimeZone fixedDateTimeZone0 = (FixedDateTimeZone)DateTimeZone.UTC;
      Chronology chronology1 = islamicChronology0.withZone(fixedDateTimeZone0);
      DateTimeParserBucket dateTimeParserBucket1 = new DateTimeParserBucket((-64L), chronology1, (Locale) null, (Integer) null);
      DateTimeFieldType dateTimeFieldType0 = DateTimeFieldType.dayOfMonth();
      dateTimeParserBucket1.saveField(dateTimeFieldType0, (String) null, (Locale) null);
      DateTimeParserBucket dateTimeParserBucket2 = new DateTimeParserBucket(0L, islamicChronology0, (Locale) null);
      dateTimeParserBucket2.getPivotYear();
      Period period0 = new Period((Object) null);
      dateTimeParserBucket2.setOffset((Integer) null);
      int int0 = 3600000;
      islamicChronology0.add((ReadablePeriod) period0, 0L, 3600000);
      // Undeclared exception!
      dateTimeParserBucket1.computeMillis(true, "Fields invalid for add");
  }
}
