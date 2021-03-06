/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 04:41:27 UTC 2020
 */

package org.joda.time;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.joda.time.DateTimeZone;
import org.joda.time.Days;
import org.joda.time.MonthDay;
import org.joda.time.ReadablePartial;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class Days_ESTest extends Days_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      DateTimeZone dateTimeZone0 = mock(DateTimeZone.class, new ViolatedAssumptionAnswer());
      doReturn(0L, 0L).when(dateTimeZone0).convertUTCToLocal(anyLong());
      MonthDay monthDay0 = new MonthDay(dateTimeZone0);
      int[] intArray0 = new int[6];
      intArray0[0] = 4;
      intArray0[1] = 1767;
      intArray0[2] = 843;
      intArray0[3] = 908;
      intArray0[4] = 27;
      intArray0[5] = 2200;
      MonthDay monthDay1 = new MonthDay(monthDay0, intArray0);
      // Undeclared exception!
      Days.daysBetween((ReadablePartial) monthDay1, (ReadablePartial) monthDay1);
  }
}
