/*
 * This file was automatically generated by EvoSuite
 * Sat Jan 18 06:36:14 UTC 2020
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
      JulianChronology julianChronology0 = JulianChronology.getInstance();
      Locale locale0 = new Locale("Ij!v");
      DateTimeParserBucket dateTimeParserBucket0 = new DateTimeParserBucket(698L, julianChronology0, locale0);
      DateTimeFieldType dateTimeFieldType0 = DateTimeFieldType.dayOfWeek();
      dateTimeParserBucket0.saveField(dateTimeFieldType0, 0);
      // Undeclared exception!
      dateTimeParserBucket0.computeMillis(true);
  }
}
