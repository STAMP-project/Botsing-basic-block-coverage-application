/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 20:37:35 UTC 2021
 */

package org.apache.commons.lang;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.LinkedList;
import java.util.Locale;
import org.apache.commons.lang.LocaleUtils;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class LocaleUtils_ESTest extends LocaleUtils_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      LinkedList<Locale.LanguageRange> linkedList0 = new LinkedList<Locale.LanguageRange>();
      Locale locale0 = Locale.GERMAN;
      LocaleUtils.isAvailableLocale((Locale) null);
      LocaleUtils.isAvailableLocale((Locale) null);
      // Undeclared exception!
      LocaleUtils.toLocale("tc__l|tl11ajO");
  }
}
