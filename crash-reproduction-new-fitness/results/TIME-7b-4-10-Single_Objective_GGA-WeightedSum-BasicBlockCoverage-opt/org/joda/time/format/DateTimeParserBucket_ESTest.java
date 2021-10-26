/*
 * This file was automatically generated by EvoSuite
 * Tue Oct 26 06:36:39 UTC 2021
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
import org.joda.time.DateTimeField;
import org.joda.time.DateTimeFieldType;
import org.joda.time.DurationField;
import org.joda.time.chrono.BuddhistChronology;
import org.joda.time.chrono.GregorianChronology;
import org.joda.time.field.OffsetDateTimeField;
import org.joda.time.format.DateTimeParserBucket;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class DateTimeParserBucket_ESTest extends DateTimeParserBucket_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      GregorianChronology gregorianChronology0 = GregorianChronology.getInstance();
      Integer integer0 = new Integer(2792);
      BuddhistChronology buddhistChronology0 = BuddhistChronology.getInstanceUTC();
      DateTimeField dateTimeField0 = buddhistChronology0.dayOfWeek();
      OffsetDateTimeField offsetDateTimeField0 = new OffsetDateTimeField(dateTimeField0, 2792);
      DateTimeFieldType dateTimeFieldType0 = mock(DateTimeFieldType.class, new ViolatedAssumptionAnswer());
      DurationField durationField0 = mock(DurationField.class, new ViolatedAssumptionAnswer());
      int int0 = new Integer(354);
      buddhistChronology0.getZone();
      DateTimeParserBucket.SavedField dateTimeParserBucket_SavedField0 = new DateTimeParserBucket.SavedField(dateTimeField0, 8);
      DateTimeParserBucket.SavedField dateTimeParserBucket_SavedField1 = new DateTimeParserBucket.SavedField(offsetDateTimeField0, 2792);
      dateTimeParserBucket_SavedField0.compareTo(dateTimeParserBucket_SavedField1);
      DateTimeParserBucket dateTimeParserBucket0 = new DateTimeParserBucket(0, gregorianChronology0, (Locale) null, integer0);
      dateTimeParserBucket0.getOffsetInteger();
      dateTimeParserBucket0.saveField(dateTimeField0, 1023);
      dateTimeParserBucket0.saveField(dateTimeField0, 2569);
      dateTimeParserBucket0.getPivotYear();
      // Undeclared exception!
      dateTimeParserBucket0.computeMillis(true, (String) null);
  }
}
