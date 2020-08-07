/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 21:37:54 UTC 2020
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
      Locale locale0 = Locale.ROOT;
      LocaleUtils.isAvailableLocale(locale0);
      LocaleUtils.availableLocaleSet();
      LocaleUtils.countriesByLanguage(")^O|&\"Q6Ef%");
      LocaleUtils.localeLookupList(locale0);
      String string0 = "S ";
      LocaleUtils.languagesByCountry("S ");
      LocaleUtils.localeLookupList(locale0);
      LocaleUtils.countriesByLanguage(")^O|&\"Q6Ef%");
      LocaleUtils.countriesByLanguage("S ");
      LocaleUtils.availableLocaleList();
      LocaleUtils.availableLocaleList();
      LocaleUtils localeUtils0 = new LocaleUtils();
      LocaleUtils localeUtils1 = new LocaleUtils();
      String string1 = "M@FA<%'k;auMg{3aS";
      // Undeclared exception!
      LocaleUtils.toLocale("M@FA<%'k;auMg{3aS");
  }
}
