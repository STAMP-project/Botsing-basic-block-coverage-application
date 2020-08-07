/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:53:09 UTC 2020
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
      Locale locale0 = Locale.KOREA;
      LocaleUtils.isAvailableLocale(locale0);
      LocaleUtils.availableLocaleSet();
      LocaleUtils.availableLocaleSet();
      String string0 = null;
      LocaleUtils.toLocale((String) null);
      LocaleUtils.countriesByLanguage((String) null);
      LocaleUtils localeUtils0 = new LocaleUtils();
      String string1 = "";
      LocaleUtils.languagesByCountry("");
      LocaleUtils.languagesByCountry("");
      LocaleUtils.availableLocaleList();
      LocaleUtils.localeLookupList(locale0);
      // Undeclared exception!
      LocaleUtils.toLocale("");
  }
}
