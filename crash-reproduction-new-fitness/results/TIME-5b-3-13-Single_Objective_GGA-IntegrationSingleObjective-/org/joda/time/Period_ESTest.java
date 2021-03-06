/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 13:18:46 UTC 2020
 */

package org.joda.time;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.joda.time.Hours;
import org.joda.time.Period;
import org.joda.time.PeriodType;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class Period_ESTest extends Period_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Period period0 = Period.hours((-475));
      Period period1 = period0.withYears((-2144));
      Hours hours0 = Hours.MIN_VALUE;
      PeriodType periodType0 = hours0.getPeriodType();
      PeriodType periodType1 = periodType0.withHoursRemoved();
      // Undeclared exception!
      period1.normalizedStandard(periodType1);
  }
}
