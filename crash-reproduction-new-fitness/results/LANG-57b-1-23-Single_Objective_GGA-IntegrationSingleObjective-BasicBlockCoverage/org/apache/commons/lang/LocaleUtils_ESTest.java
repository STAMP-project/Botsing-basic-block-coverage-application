/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:29:48 UTC 2020
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
      LocaleUtils.countriesByLanguage("{");
      Locale locale0 = Locale.GERMAN;
      LocaleUtils.isAvailableLocale(locale0);
      LocaleUtils.localeLookupList(locale0, locale0);
      // Undeclared exception!
      LocaleUtils.toLocale("org.apache.commons.lang.LocaleUtils");
  }
}
