/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 19:19:26 UTC 2020
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
      int int0 = (-1923);
      baseClass0.addGroupsField("6K)u", "object already associated, but no entry was found", 76);
      TextAreaClass.ContentType textAreaClass_ContentType0 = TextAreaClass.ContentType.VELOCITY_CODE;
      // Undeclared exception!
      baseClass0.addTextAreaField("6K)u", "6K)u", (-1923), (-1923), textAreaClass_ContentType0);
  }
}
