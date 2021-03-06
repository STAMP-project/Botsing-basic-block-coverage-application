/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 00:18:12 UTC 2020
 */

package org.joda.time.chrono;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.joda.time.DateTime;
import org.joda.time.DateTimeZone;
import org.joda.time.LocalDate;
import org.joda.time.ReadableInstant;
import org.joda.time.chrono.GJChronology;
import org.joda.time.chrono.IslamicChronology;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseChronology_ESTest extends BaseChronology_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      DateTimeZone.forOffsetHours(448);
      DateTime dateTime0 = DateTime.now();
      DateTime dateTime1 = dateTime0.minusMonths(448);
      dateTime1.withWeekyear(448);
      DateTimeZone dateTimeZone0 = DateTimeZone.forOffsetHours(448);
      DateTime.now();
      dateTime1.minusMonths((-3159));
      dateTime1.withWeekyear(448);
      GJChronology gJChronology0 = GJChronology.getInstance(dateTimeZone0, (ReadableInstant) dateTime1);
      gJChronology0.millisOfSecond();
      LocalDate localDate0 = new LocalDate((long) (-3159));
      IslamicChronology islamicChronology0 = IslamicChronology.getInstance();
      // Undeclared exception!
      islamicChronology0.set(localDate0, 448);
  }
}
