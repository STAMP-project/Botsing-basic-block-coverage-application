/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:57:33 UTC 2020
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
      doReturn(0L).when(date0).getTime();
      int int0 = 1;
      DateUtils.addMinutes(date0, 1);
      String string0 = "w";
      String[] stringArray0 = new String[8];
      stringArray0[0] = "w";
      stringArray0[1] = "w";
      stringArray0[2] = "w";
      stringArray0[3] = "w";
      stringArray0[4] = "w";
      stringArray0[5] = "w";
      stringArray0[6] = "w";
      String string1 = "";
      stringArray0[7] = "";
      try { 
        DateUtils.parseDate("w", stringArray0);
        fail("Expecting exception: ParseException");
      
      } catch(ParseException e) {
         //
         // Unable to parse the date: w
         //
         verifyException("org.apache.commons.lang3.time.DateUtils", e);
      }
  }
}
