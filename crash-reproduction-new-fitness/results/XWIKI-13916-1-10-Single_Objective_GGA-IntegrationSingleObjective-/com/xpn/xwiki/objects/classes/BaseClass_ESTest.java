/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 14:59:16 UTC 2020
 */

package com.xpn.xwiki.objects.classes;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import com.xpn.xwiki.objects.classes.BaseClass;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseClass_ESTest extends BaseClass_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      BaseClass baseClass0 = new BaseClass();
      baseClass0.setDefaultWeb("property");
      String string0 = "] for a document reference";
      int int0 = 0;
      baseClass0.addPasswordField("] for a document reference", "", 0);
      int int1 = 0;
      // Undeclared exception!
      baseClass0.addTextAreaField("] for a document reference", "D-%tK++S", 0, 0, "property", "D-%tK++S");
  }
}
