/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 00:54:51 UTC 2020
 */

package org.apache.commons.lang3;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.Locale;
import org.apache.commons.lang3.StringUtils;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class StringUtils_ESTest extends StringUtils_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Locale locale0 = Locale.PRC;
      StringUtils.lowerCase("", locale0);
      StringUtils.removeStart("", "6.0");
      Object[] objectArray0 = new Object[2];
      Object object0 = new Object();
      objectArray0[0] = object0;
      objectArray0[1] = (Object) "";
      StringUtils.join(objectArray0, '1', 0, 0);
      String string0 = "R7<@8lm2%/KWn6oEN8";
      StringUtils.replace((String) null, "", "R7<@8lm2%/KWn6oEN8");
      StringUtils stringUtils0 = new StringUtils();
      String string1 = "DcFb!o{1VI6o/@I";
      String[] stringArray0 = new String[8];
      stringArray0[0] = "";
      stringArray0[1] = "DcFb!o{1VI6o/@I";
      stringArray0[2] = "";
      stringArray0[3] = null;
      stringArray0[4] = "";
      stringArray0[5] = null;
      stringArray0[6] = "";
      stringArray0[7] = "R7<@8lm2%/KWn6oEN8";
      // Undeclared exception!
      StringUtils.replaceEach("DcFb!o{1VI6o/@I", stringArray0, stringArray0);
  }
}
