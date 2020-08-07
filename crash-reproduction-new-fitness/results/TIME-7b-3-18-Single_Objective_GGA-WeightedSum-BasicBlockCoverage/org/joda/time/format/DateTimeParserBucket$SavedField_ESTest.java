/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 16:47:32 UTC 2020
 */

package org.joda.time.format;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.Locale;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.joda.time.DateTimeField;
import org.joda.time.DateTimeFieldType;
import org.joda.time.chrono.BuddhistChronology;
import org.joda.time.field.OffsetDateTimeField;
import org.joda.time.format.DateTimeParserBucket;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class DateTimeParserBucket$SavedField_ESTest extends DateTimeParserBucket$SavedField_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      BuddhistChronology buddhistChronology0 = BuddhistChronology.getInstanceUTC();
      boolean boolean0 = false;
      DateTimeField dateTimeField0 = buddhistChronology0.weekOfWeekyear();
      DateTimeParserBucket.SavedField dateTimeParserBucket_SavedField0 = new DateTimeParserBucket.SavedField(dateTimeField0, 1);
      dateTimeParserBucket_SavedField0.set(1, false);
      BuddhistChronology.getInstanceUTC();
      DateTimeFieldType.weekyear();
      int int0 = (-2144);
      OffsetDateTimeField offsetDateTimeField0 = new OffsetDateTimeField(dateTimeField0, (-2144));
      Locale locale0 = Locale.FRENCH;
      DateTimeParserBucket.SavedField dateTimeParserBucket_SavedField1 = new DateTimeParserBucket.SavedField(offsetDateTimeField0, (String) null, locale0);
      Locale locale1 = Locale.KOREAN;
      DateTimeParserBucket.SavedField dateTimeParserBucket_SavedField2 = new DateTimeParserBucket.SavedField(dateTimeField0, (String) null, locale0);
      long long0 = (-1L);
      // Undeclared exception!
      dateTimeParserBucket_SavedField2.set((-1L), false);
  }
}
