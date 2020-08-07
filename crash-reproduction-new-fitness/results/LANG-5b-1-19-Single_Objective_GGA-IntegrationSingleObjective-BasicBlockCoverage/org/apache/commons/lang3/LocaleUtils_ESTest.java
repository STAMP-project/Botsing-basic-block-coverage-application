/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:58:12 UTC 2020
 */

package org.apache.commons.lang3;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.lang3.LocaleUtils;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class LocaleUtils_ESTest extends LocaleUtils_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      LocaleUtils.countriesByLanguage((String) null);
      LocaleUtils localeUtils0 = new LocaleUtils();
      LocaleUtils.countriesByLanguage((String) null);
      LocaleUtils.availableLocaleSet();
      LocaleUtils.languagesByCountry((String) null);
      LocaleUtils localeUtils1 = new LocaleUtils();
      LocaleUtils.availableLocaleSet();
      LocaleUtils localeUtils2 = new LocaleUtils();
      LocaleUtils.availableLocaleSet();
      LocaleUtils.countriesByLanguage("");
      LocaleUtils.availableLocaleSet();
      LocaleUtils.availableLocaleSet();
      LocaleUtils.toLocale((String) null);
      // Undeclared exception!
      LocaleUtils.toLocale("l`");
  }
}
