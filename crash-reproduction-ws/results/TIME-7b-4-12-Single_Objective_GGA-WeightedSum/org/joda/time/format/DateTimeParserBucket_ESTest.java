/*
 * This file was automatically generated by EvoSuite
 * Sat Jan 18 06:31:41 UTC 2020
 */

package org.joda.time.format;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.Locale;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.joda.time.DateTimeField;
import org.joda.time.chrono.BuddhistChronology;
import org.joda.time.format.DateTimeParserBucket;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class DateTimeParserBucket_ESTest extends DateTimeParserBucket_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      BuddhistChronology buddhistChronology0 = BuddhistChronology.getInstance();
      Locale locale0 = Locale.KOREA;
      DateTimeParserBucket dateTimeParserBucket0 = new DateTimeParserBucket(1517L, buddhistChronology0, locale0);
      DateTimeField dateTimeField0 = buddhistChronology0.dayOfWeek();
      dateTimeParserBucket0.saveField(dateTimeField0, (-832));
      // Undeclared exception!
      dateTimeParserBucket0.computeMillis(false, "");
  }
}
