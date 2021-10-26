/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 20:37:48 UTC 2021
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
      Locale locale0 = Locale.CHINA;
      LocaleUtils.isAvailableLocale(locale0);
      String string0 = "";
      LocaleUtils.languagesByCountry("");
      LocaleUtils localeUtils0 = new LocaleUtils();
      LocaleUtils.availableLocaleList();
      String string1 = "V}8;OjM-6*S[-I";
      LocaleUtils.countriesByLanguage("V}8;OjM-6*S[-I");
      LocaleUtils.countriesByLanguage("");
      LocaleUtils.countriesByLanguage("y5Nqbt,j5");
      String string2 = "TX363S1>?LXt";
      // Undeclared exception!
      LocaleUtils.toLocale("TX363S1>?LXt");
  }
}
