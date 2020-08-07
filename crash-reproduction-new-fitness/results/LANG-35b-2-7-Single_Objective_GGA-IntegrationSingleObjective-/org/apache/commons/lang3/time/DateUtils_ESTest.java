/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 21:33:47 UTC 2020
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
      String string0 = "WSMuEYqh=`b{`UAO9";
      String[] stringArray0 = new String[0];
      try { 
        DateUtils.parseDate("WSMuEYqh=`b{`UAO9", stringArray0);
        fail("Expecting exception: ParseException");
      
      } catch(ParseException e) {
         //
         // Unable to parse the date: WSMuEYqh=`b{`UAO9
         //
         verifyException("org.apache.commons.lang3.time.DateUtils", e);
      }
  }
}
