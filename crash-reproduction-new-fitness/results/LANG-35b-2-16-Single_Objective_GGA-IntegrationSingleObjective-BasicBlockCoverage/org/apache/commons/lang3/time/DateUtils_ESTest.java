/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:53:25 UTC 2020
 */

package org.apache.commons.lang3.time;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.text.ParseException;
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
      doReturn(0L, 0L).when(date0).getTime();
      DateUtils.isSameInstant(date0, date0);
      String[] stringArray0 = new String[4];
      stringArray0[0] = "";
      stringArray0[1] = "";
      stringArray0[2] = "";
      stringArray0[3] = "";
      Date date1 = DateUtils.parseDate("", stringArray0);
      Date date2 = DateUtils.addSeconds(date1, 0);
      int int0 = 0;
      DateUtils.addMinutes(date2, 0);
      DateUtils.truncate(date1, 0);
      String string0 = "=g%nYg96;^N|.q%!%V|";
      try { 
        DateUtils.parseDate("=g%nYg96;^N|.q%!%V|", stringArray0);
        fail("Expecting exception: ParseException");
      
      } catch(ParseException e) {
         //
         // Unable to parse the date: =g%nYg96;^N|.q%!%V|
         //
         verifyException("org.apache.commons.lang3.time.DateUtils", e);
      }
  }
}
