/*
 * This file was automatically generated by EvoSuite
 * Fri May 15 12:19:41 UTC 2020
 */

package org.joda.time;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.joda.time.Days;
import org.joda.time.PeriodType;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class PeriodType_ESTest extends PeriodType_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      PeriodType periodType0 = PeriodType.years();
      PeriodType.yearDayTime();
      periodType0.withSecondsRemoved();
      PeriodType periodType1 = PeriodType.years();
      PeriodType.hours();
      Days days0 = Days.FIVE;
      int[] intArray0 = new int[9];
      intArray0[0] = 0;
      intArray0[1] = 0;
      intArray0[2] = 0;
      intArray0[3] = 0;
      intArray0[4] = 0;
      intArray0[5] = 0;
      intArray0[6] = 0;
      intArray0[7] = 0;
      intArray0[8] = 0;
      periodType1.setIndexedField(days0, 0, intArray0, 0);
      periodType1.withHoursRemoved();
      // Undeclared exception!
      periodType0.setIndexedField(days0, 6, intArray0, 0);
  }
}
