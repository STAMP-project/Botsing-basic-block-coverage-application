/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 19:13:01 UTC 2020
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
      String string0 = "!vI|Tot.Y~H;tx";
      String string1 = "w,erOr";
      baseClass0.addStaticListField("!vI|Tot.Y~H;tx", "fromParent", 3600000, false, "fromParent", "w,erOr", "w,erOr");
      baseClass0.addDBTreeListField("fromParent", "fromParent", "!vI|Tot.Y~H;tx");
      baseClass0.setCustomClass("l01N5^zo&QA)");
      TextAreaClass.ContentType textAreaClass_ContentType0 = TextAreaClass.ContentType.VELOCITY_CODE;
      // Undeclared exception!
      baseClass0.addTextAreaField("!vI|Tot.Y~H;tx", "!vI|Tot.Y~H;tx", (-1028), (-1028), textAreaClass_ContentType0);
  }
}
