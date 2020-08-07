/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 18:23:42 UTC 2020
 */

package org.joda.time;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.joda.time.Period;
import org.joda.time.PeriodType;
import org.joda.time.Weeks;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class Period_ESTest extends Period_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Weeks weeks0 = Weeks.TWO;
      PeriodType periodType0 = weeks0.getPeriodType();
      PeriodType periodType1 = periodType0.withDaysRemoved();
      Period period0 = new Period(3600000L, periodType1);
      period0.toMutablePeriod();
      int int0 = 12;
      // Undeclared exception!
      period0.withYears(12);
  }
}
