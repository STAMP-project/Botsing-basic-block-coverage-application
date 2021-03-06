/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:20:12 UTC 2020
 */

package org.apache.commons.lang3.time;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.text.ParseException;
import java.util.Calendar;
import java.util.Date;
import org.apache.commons.lang3.time.DateUtils;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class DateUtils_ESTest extends DateUtils_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Date date0 = mock(Date.class, new ViolatedAssumptionAnswer());
      doReturn(0L).when(date0).getTime();
      DateUtils.setMinutes(date0, 30);
      Calendar calendar0 = mock(Calendar.class, new ViolatedAssumptionAnswer());
      doReturn(0, 0, 0, 0, 0).when(calendar0).get(anyInt());
      DateUtils.isSameLocalTime(calendar0, calendar0);
      String[] stringArray0 = new String[0];
      try { 
        DateUtils.parseDate("k\"KphkL5UOLI6", stringArray0);
        fail("Expecting exception: ParseException");
      
      } catch(ParseException e) {
         //
         // Unable to parse the date: k\"KphkL5UOLI6
         //
         verifyException("org.apache.commons.lang3.time.DateUtils", e);
      }
  }
}
