/*
 * This file was automatically generated by EvoSuite
 * Sat Jan 18 06:24:49 UTC 2020
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
      Days days0 = Days.FOUR;
      PeriodType periodType0 = days0.getPeriodType();
      int[] intArray0 = new int[2];
      // Undeclared exception!
      periodType0.setIndexedField(days0, 0, intArray0, 1);
  }
}
