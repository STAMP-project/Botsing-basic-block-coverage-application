/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:58:37 UTC 2020
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
      LocaleUtils localeUtils0 = new LocaleUtils();
      LocaleUtils localeUtils1 = new LocaleUtils();
      LocaleUtils.toLocale((String) null);
      String string0 = "{[rK <|)";
      // Undeclared exception!
      LocaleUtils.toLocale("{[rK <|)");
  }
}
