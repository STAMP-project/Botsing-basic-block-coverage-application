/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 13:18:32 UTC 2020
 */

package org.joda.time;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.joda.time.PeriodType;
import org.joda.time.ReadablePeriod;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class PeriodType_ESTest extends PeriodType_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      PeriodType periodType0 = PeriodType.millis();
      PeriodType.yearWeekDay();
      int[] intArray0 = new int[1];
      intArray0[0] = 0;
      // Undeclared exception!
      periodType0.setIndexedField((ReadablePeriod) null, 0, intArray0, (-5934));
  }
}
