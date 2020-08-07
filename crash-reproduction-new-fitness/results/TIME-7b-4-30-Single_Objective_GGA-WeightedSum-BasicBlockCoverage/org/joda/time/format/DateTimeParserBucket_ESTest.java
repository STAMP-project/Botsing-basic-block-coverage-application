/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 16:51:07 UTC 2020
 */

package org.joda.time.format;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.Locale;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.joda.time.DateTimeFieldType;
import org.joda.time.chrono.JulianChronology;
import org.joda.time.format.DateTimeParserBucket;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class DateTimeParserBucket_ESTest extends DateTimeParserBucket_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      DateTimeParserBucket.SavedField[] dateTimeParserBucket_SavedFieldArray0 = new DateTimeParserBucket.SavedField[3];
      DateTimeFieldType dateTimeFieldType0 = DateTimeFieldType.dayOfYear();
      Locale locale0 = Locale.GERMAN;
      JulianChronology julianChronology0 = JulianChronology.getInstanceUTC();
      Integer integer0 = new Integer(1220);
      DateTimeParserBucket dateTimeParserBucket0 = new DateTimeParserBucket((-1880L), julianChronology0, locale0, integer0);
      DateTimeParserBucket.SavedState dateTimeParserBucket_SavedState0 = dateTimeParserBucket0.new SavedState();
      Integer integer1 = dateTimeParserBucket_SavedState0.iOffset;
      dateTimeParserBucket0.saveField(dateTimeFieldType0, (String) null, locale0);
      // Undeclared exception!
      dateTimeParserBucket0.computeMillis();
  }
}
