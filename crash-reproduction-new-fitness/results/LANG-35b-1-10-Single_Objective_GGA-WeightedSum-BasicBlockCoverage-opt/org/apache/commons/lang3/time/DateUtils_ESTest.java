/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 20:35:32 UTC 2021
 */

package org.apache.commons.lang3.time;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.text.ParseException;
import org.apache.commons.lang3.time.DateUtils;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class DateUtils_ESTest extends DateUtils_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      String[] stringArray0 = new String[1];
      stringArray0[0] = "";
      DateUtils.parseDate("", stringArray0);
      try { 
        DateUtils.parseDate("]ZZ", stringArray0);
        fail("Expecting exception: ParseException");
      
      } catch(ParseException e) {
         //
         // Unable to parse the date: ]ZZ
         //
         verifyException("org.apache.commons.lang3.time.DateUtils", e);
      }
  }
}
