/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 00:22:12 UTC 2020
 */

package org.joda.time.chrono;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.joda.time.DateTimeZone;
import org.joda.time.Days;
import org.joda.time.LocalDate;
import org.joda.time.chrono.IslamicChronology;
import org.joda.time.chrono.JulianChronology;
import org.joda.time.tz.FixedDateTimeZone;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseChronology_ESTest extends BaseChronology_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      DateTimeZone dateTimeZone0 = DateTimeZone.getDefault();
      LocalDate localDate0 = LocalDate.now();
      Days days0 = Days.FOUR;
      String string0 = " ";
      localDate0.toString(" ");
      DateTimeZone.forOffsetHours(173);
      int int0 = IslamicChronology.AH;
      FixedDateTimeZone fixedDateTimeZone0 = (FixedDateTimeZone)DateTimeZone.UTC;
      JulianChronology.getInstance(dateTimeZone0);
      Integer integer0 = new Integer(7);
      IslamicChronology islamicChronology0 = IslamicChronology.getInstanceUTC();
      islamicChronology0.withZone(dateTimeZone0);
      IslamicChronology islamicChronology1 = IslamicChronology.getInstance((DateTimeZone) fixedDateTimeZone0);
      islamicChronology1.getDayOfMonth((-7), (-250), 1);
      localDate0.toDateTimeAtStartOfDay();
      LocalDate localDate1 = new LocalDate((-2647L), dateTimeZone0);
      // Undeclared exception!
      islamicChronology0.set(localDate1, 173);
  }
}
