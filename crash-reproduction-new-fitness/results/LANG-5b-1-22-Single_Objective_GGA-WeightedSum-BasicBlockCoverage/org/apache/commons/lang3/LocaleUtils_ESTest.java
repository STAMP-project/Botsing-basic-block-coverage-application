/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:24:26 UTC 2020
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
      Locale locale0 = Locale.ITALIAN;
      LocaleUtils.localeLookupList(locale0, locale0);
      String string0 = "-rDOeIAn_pG|(j?_%";
      // Undeclared exception!
      LocaleUtils.toLocale("-rDOeIAn_pG|(j?_%");
  }
}
