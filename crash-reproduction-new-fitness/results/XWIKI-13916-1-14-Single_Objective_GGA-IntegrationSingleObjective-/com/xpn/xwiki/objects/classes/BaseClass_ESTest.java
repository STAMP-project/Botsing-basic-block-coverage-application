/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 15:03:55 UTC 2020
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
      String string0 = "";
      int int0 = (-71);
      baseClass0.addUsersField("", "", (-71));
      String string1 = null;
      int int1 = 0;
      String string2 = "";
      // Undeclared exception!
      baseClass0.addTextAreaField("", (String) null, (-71), 0, "", (String) null);
  }
}
