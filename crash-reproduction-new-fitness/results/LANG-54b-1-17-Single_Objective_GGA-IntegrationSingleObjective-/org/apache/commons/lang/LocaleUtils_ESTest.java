/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 02:01:57 UTC 2020
 */

package org.apache.commons.lang;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.HashMap;
import java.util.List;
import org.apache.commons.lang.LocaleUtils;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class LocaleUtils_ESTest extends LocaleUtils_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      HashMap<String, List<String>> hashMap0 = new HashMap<String, List<String>>();
      LocaleUtils.countriesByLanguage("fz_)L-U");
      LocaleUtils localeUtils0 = new LocaleUtils();
      // Undeclared exception!
      LocaleUtils.toLocale("fz_)L-U");
  }
}
