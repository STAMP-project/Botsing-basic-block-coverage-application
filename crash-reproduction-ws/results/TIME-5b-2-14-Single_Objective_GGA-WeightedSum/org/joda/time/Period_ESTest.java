/*
 * This file was automatically generated by EvoSuite
 * Sat Jan 18 06:25:31 UTC 2020
 */

package org.joda.time;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.joda.time.Chronology;
import org.joda.time.Duration;
import org.joda.time.Hours;
import org.joda.time.Period;
import org.joda.time.PeriodType;
import org.joda.time.TestLocalTime_Basics;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class Period_ESTest extends Period_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Duration duration0 = Duration.millis(0L);
      Hours hours0 = duration0.toStandardHours();
      PeriodType periodType0 = hours0.getPeriodType();
      TestLocalTime_Basics testLocalTime_Basics0 = new TestLocalTime_Basics("");
      TestLocalTime_Basics.MockInstant testLocalTime_Basics_MockInstant0 = testLocalTime_Basics0.new MockInstant();
      Chronology chronology0 = testLocalTime_Basics_MockInstant0.getChronology();
      Period period0 = duration0.toPeriod(periodType0, chronology0);
      // Undeclared exception!
      period0.withYears(1904);
  }
}
