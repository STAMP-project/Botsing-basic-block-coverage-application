/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 17:44:00 UTC 2020
 */

package org.joda.time;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.joda.time.Chronology;
import org.joda.time.Hours;
import org.joda.time.Period;
import org.joda.time.PeriodType;
import org.joda.time.TestDateTime_Basics;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class Period_ESTest extends Period_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Hours hours0 = Hours.FIVE;
      PeriodType periodType0 = hours0.getPeriodType();
      PeriodType periodType1 = periodType0.withDaysRemoved();
      TestDateTime_Basics testDateTime_Basics0 = mock(TestDateTime_Basics.class, new ViolatedAssumptionAnswer());
      TestDateTime_Basics.MockInstant testDateTime_Basics_MockInstant0 = testDateTime_Basics0.new MockInstant();
      Chronology chronology0 = testDateTime_Basics_MockInstant0.getChronology();
      Period period0 = new Period(2024L, 2024L, periodType1, chronology0);
      int int0 = (-70);
      period0.withHours((-70));
      int int1 = 0;
      // Undeclared exception!
      period0.withYears(0);
  }
}
