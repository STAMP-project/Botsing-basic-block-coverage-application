/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 16:26:20 UTC 2021
 */

package com.xpn.xwiki.objects.classes;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import com.xpn.xwiki.objects.classes.BaseClass;
import com.xpn.xwiki.objects.classes.TextAreaClass;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseClass_ESTest extends BaseClass_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      BaseClass baseClass0 = new BaseClass();
      String string0 = "";
      baseClass0.addGroupsField("", "", (-1));
      baseClass0.getEnabledProperties();
      int int0 = 0;
      TextAreaClass.ContentType textAreaClass_ContentType0 = TextAreaClass.ContentType.PURE_TEXT;
      // Undeclared exception!
      baseClass0.addTextAreaField("", "", 0, 0, textAreaClass_ContentType0);
  }
}
