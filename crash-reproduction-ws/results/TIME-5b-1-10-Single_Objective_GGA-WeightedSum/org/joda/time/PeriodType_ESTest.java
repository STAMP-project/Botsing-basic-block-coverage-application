/*
 * This file was automatically generated by EvoSuite
 * Sat Jan 18 06:24:42 UTC 2020
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
      PeriodType periodType0 = PeriodType.minutes();
      Period period0 = new Period(1485L, 1485L);
      int[] intArray0 = new int[8];
      // Undeclared exception!
      periodType0.setIndexedField(period0, 0, intArray0, 0);
  }
}
