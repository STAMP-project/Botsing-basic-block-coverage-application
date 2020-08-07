/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 16:50:01 UTC 2020
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
import org.joda.time.MonthDay;
import org.joda.time.Months;
import org.joda.time.chrono.BuddhistChronology;
import org.joda.time.format.DateTimeParserBucket;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class DateTimeParserBucket_ESTest extends DateTimeParserBucket_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      BuddhistChronology buddhistChronology0 = BuddhistChronology.getInstanceUTC();
      MonthDay monthDay0 = MonthDay.parse("");
      Months months0 = Months.months((-1408));
      months0.getFieldType();
      buddhistChronology0.set(monthDay0, (-1408));
      DateTimeField dateTimeField0 = buddhistChronology0.weekOfWeekyear();
      monthDay0.dayOfMonth();
      Locale locale0 = Locale.KOREAN;
      DateTimeFieldType.year();
      buddhistChronology0.set(monthDay0, 3022);
      DateTimeParserBucket dateTimeParserBucket0 = new DateTimeParserBucket(3022, (Chronology) null, (Locale) null);
      dateTimeParserBucket0.saveField(dateTimeField0, (-723));
      dateTimeParserBucket0.getLocale();
      DateTimeParserBucket.SavedState dateTimeParserBucket_SavedState0 = dateTimeParserBucket0.new SavedState();
      Integer integer0 = dateTimeParserBucket_SavedState0.iOffset;
      dateTimeParserBucket0.setPivotYear((Integer) null);
      // Undeclared exception!
      dateTimeParserBucket0.computeMillis(true, "YearDay");
  }
}
