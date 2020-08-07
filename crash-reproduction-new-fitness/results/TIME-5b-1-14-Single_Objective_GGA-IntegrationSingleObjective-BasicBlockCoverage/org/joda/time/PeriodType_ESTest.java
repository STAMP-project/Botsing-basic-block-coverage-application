/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 13:18:57 UTC 2020
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
public class PeriodType_ESTest extends PeriodType_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      PeriodType periodType0 = PeriodType.standard();
      PeriodType periodType1 = periodType0.withWeeksRemoved();
      PeriodType.time();
      PeriodType.yearDayTime();
      Period period0 = new Period(587L);
      Period period1 = period0.withWeeks(2);
      int[] intArray0 = new int[8];
      intArray0[0] = 2;
      intArray0[1] = 2;
      intArray0[2] = 2;
      intArray0[3] = 2;
      intArray0[4] = 2;
      intArray0[5] = 2;
      intArray0[6] = (-1500);
      intArray0[7] = 2;
      // Undeclared exception!
      periodType1.setIndexedField(period1, 2, intArray0, 0);
  }
}
