/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 22:17:51 UTC 2021
 */

package org.joda.time;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.GregorianCalendar;
import java.util.Locale;
import java.util.TimeZone;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.joda.time.Chronology;
import org.joda.time.Days;
import org.joda.time.MonthDay;
import org.joda.time.ReadablePartial;
import org.joda.time.TestInterval_Basics;
import org.joda.time.TestYearMonthDay_Basics;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class Days_ESTest extends Days_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      TimeZone timeZone0 = TimeZone.getDefault();
      Locale locale0 = Locale.KOREAN;
      GregorianCalendar gregorianCalendar0 = new GregorianCalendar(timeZone0, locale0);
      String string0 = "2002 10";
      TestYearMonthDay_Basics testYearMonthDay_Basics0 = new TestYearMonthDay_Basics("2002 10");
      TestYearMonthDay_Basics.MockInstant testYearMonthDay_Basics_MockInstant0 = testYearMonthDay_Basics0.new MockInstant();
      TestInterval_Basics testInterval_Basics0 = new TestInterval_Basics((String) null);
      TestInterval_Basics.MockInterval testInterval_Basics_MockInterval0 = testInterval_Basics0.new MockInterval();
      Chronology chronology0 = testInterval_Basics_MockInterval0.getChronology();
      MonthDay monthDay0 = new MonthDay(chronology0);
      int[] intArray0 = new int[6];
      intArray0[0] = 6;
      intArray0[1] = 723;
      intArray0[2] = (-32);
      intArray0[3] = 1721;
      intArray0[4] = 1617;
      int int0 = (-1252);
      intArray0[5] = (-1252);
      MonthDay monthDay1 = new MonthDay(monthDay0, intArray0);
      // Undeclared exception!
      Days.daysBetween((ReadablePartial) monthDay1, (ReadablePartial) monthDay0);
  }
}
