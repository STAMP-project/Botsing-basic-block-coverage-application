/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 21:09:49 UTC 2020
 */

package org.apache.commons.lang;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.Locale;
import org.apache.commons.lang.LocaleUtils;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class LocaleUtils_ESTest extends LocaleUtils_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Locale locale0 = Locale.KOREA;
      LocaleUtils.isAvailableLocale(locale0);
      LocaleUtils localeUtils0 = new LocaleUtils();
      String string0 = "TI)t~1F ecQj:)F;Xmp";
      // Undeclared exception!
      LocaleUtils.toLocale("TI)t~1F ecQj:)F;Xmp");
  }
}
