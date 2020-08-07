/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 18:28:56 UTC 2020
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
      Months months0 = Months.EIGHT;
      PeriodType periodType0 = months0.getPeriodType();
      int[] intArray0 = new int[2];
      intArray0[0] = 0;
      intArray0[1] = 0;
      // Undeclared exception!
      periodType0.setIndexedField(months0, 0, intArray0, 0);
  }
}
