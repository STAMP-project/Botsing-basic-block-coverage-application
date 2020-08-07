/*
 * This file was automatically generated by EvoSuite
 * Sat Jan 18 06:23:00 UTC 2020
 */

package org.joda.time;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.joda.time.Period;
import org.joda.time.PeriodType;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class Period_ESTest extends Period_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Period period0 = new Period((-31), (-31), 0, 0, (-31), 0, 1943, 0);
      PeriodType periodType0 = PeriodType.weeks();
      // Undeclared exception!
      period0.normalizedStandard(periodType0);
  }
}
