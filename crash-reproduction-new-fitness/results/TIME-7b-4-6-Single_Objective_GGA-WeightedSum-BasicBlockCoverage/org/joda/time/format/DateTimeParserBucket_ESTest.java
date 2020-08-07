/*
 * This file was automatically generated by EvoSuite
 * Fri May 15 12:22:02 UTC 2020
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
import org.joda.time.DurationField;
import org.joda.time.chrono.BuddhistChronology;
import org.joda.time.format.DateTimeParserBucket;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class DateTimeParserBucket_ESTest extends DateTimeParserBucket_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      BuddhistChronology buddhistChronology0 = BuddhistChronology.getInstanceUTC();
      buddhistChronology0.getDateTimeMillis((-957L), 1, 1, 1, 160);
      DurationField durationField0 = buddhistChronology0.seconds();
      DateTimeParserBucket.compareReverse(durationField0, durationField0);
      Locale locale0 = Locale.JAPAN;
      Locale.forLanguageTag("RMj1BBkfH");
      DateTimeFieldType.era();
      Integer integer0 = new Integer(1914);
      DateTimeParserBucket dateTimeParserBucket0 = new DateTimeParserBucket(1, buddhistChronology0, locale0, integer0, 41);
      DateTimeField dateTimeField0 = buddhistChronology0.dayOfMonth();
      dateTimeParserBucket0.saveField(dateTimeField0, 3600);
      dateTimeParserBucket0.restoreState("GSpfLLCH$p^xgq6vN=g");
      dateTimeParserBucket0.setOffset(2);
      dateTimeParserBucket0.setOffset(41);
      Integer integer1 = new Integer(41);
      dateTimeParserBucket0.setOffset(integer1);
      // Undeclared exception!
      dateTimeParserBucket0.computeMillis(false, "");
  }
}
