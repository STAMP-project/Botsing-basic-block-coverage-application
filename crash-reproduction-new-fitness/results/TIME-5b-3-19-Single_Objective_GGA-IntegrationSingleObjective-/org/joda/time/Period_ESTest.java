/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 18:25:21 UTC 2020
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
      int int0 = 59;
      int int1 = 0;
      int int2 = 1;
      int int3 = 1900;
      Period period0 = new Period(59, 2006, (-1), 0, 1, (-2681), 1900, 59);
      period0.getYears();
      Period period1 = period0.multipliedBy(59);
      Weeks weeks0 = Weeks.TWO;
      PeriodType periodType0 = weeks0.getPeriodType();
      // Undeclared exception!
      period1.normalizedStandard(periodType0);
  }
}
