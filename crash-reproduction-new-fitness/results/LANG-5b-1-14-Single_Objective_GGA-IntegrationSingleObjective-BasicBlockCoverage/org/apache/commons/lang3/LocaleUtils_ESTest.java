/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 21:36:00 UTC 2020
 */

package org.apache.commons.lang3;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.Locale;
import org.apache.commons.lang3.LocaleUtils;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class LocaleUtils_ESTest extends LocaleUtils_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      String string0 = "";
      LocaleUtils.countriesByLanguage("");
      LocaleUtils.toLocale((String) null);
      LocaleUtils.localeLookupList((Locale) null, (Locale) null);
      LocaleUtils.isAvailableLocale((Locale) null);
      LocaleUtils.availableLocaleList();
      // Undeclared exception!
      LocaleUtils.toLocale("Q\"`!9R");
  }
}
