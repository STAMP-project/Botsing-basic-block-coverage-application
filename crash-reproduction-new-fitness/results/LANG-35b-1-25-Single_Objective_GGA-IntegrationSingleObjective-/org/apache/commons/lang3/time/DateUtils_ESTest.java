/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:58:13 UTC 2020
 */

package org.apache.commons.lang3.time;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.text.ParseException;
import java.util.Calendar;
import java.util.GregorianCalendar;
import org.apache.commons.lang3.time.DateUtils;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class DateUtils_ESTest extends DateUtils_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      DateUtils dateUtils0 = new DateUtils();
      int int0 = (-1);
      int int1 = (-833);
      GregorianCalendar gregorianCalendar0 = new GregorianCalendar((-1), (-1), (-833));
      DateUtils.isSameInstant((Calendar) gregorianCalendar0, (Calendar) gregorianCalendar0);
      String[] stringArray0 = new String[1];
      stringArray0[0] = "";
      try { 
        DateUtils.parseDate("Us}P9ApkQ_q*Me\"", stringArray0);
        fail("Expecting exception: ParseException");
      
      } catch(ParseException e) {
         //
         // Unable to parse the date: Us}P9ApkQ_q*Me\"
         //
         verifyException("org.apache.commons.lang3.time.DateUtils", e);
      }
  }
}
