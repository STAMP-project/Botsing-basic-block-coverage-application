/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 13:16:57 UTC 2020
 */

package org.joda.time;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.joda.time.Months;
import org.joda.time.PeriodType;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class PeriodType_ESTest extends PeriodType_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      PeriodType periodType0 = PeriodType.dayTime();
      PeriodType.yearDayTime();
      Months months0 = Months.EIGHT;
      int[] intArray0 = new int[9];
      intArray0[0] = 2;
      intArray0[1] = 2;
      intArray0[2] = 2;
      intArray0[3] = 2;
      intArray0[4] = 2;
      intArray0[5] = 2;
      intArray0[6] = 2070;
      intArray0[7] = 2;
      intArray0[8] = 2;
      // Undeclared exception!
      periodType0.setIndexedField(months0, 2, intArray0, (-1439));
  }
}
