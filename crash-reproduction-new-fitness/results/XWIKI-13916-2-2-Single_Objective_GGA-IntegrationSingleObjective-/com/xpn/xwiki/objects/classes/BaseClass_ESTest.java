/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 14:50:00 UTC 2020
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
      baseClass0.get("customMapping");
      String string0 = "l!XOH";
      baseClass0.addDBTreeListField("l!XOH", "l!XOH", (-3710), false, false, "l!XOH");
      baseClass0.getOwnerDocument();
      TextAreaClass.EditorType textAreaClass_EditorType0 = TextAreaClass.EditorType.PURE_TEXT;
      // Undeclared exception!
      baseClass0.addTextAreaField("l!XOH", "tSo}42; BlOY", 3222, (-3710), textAreaClass_EditorType0);
  }
}
