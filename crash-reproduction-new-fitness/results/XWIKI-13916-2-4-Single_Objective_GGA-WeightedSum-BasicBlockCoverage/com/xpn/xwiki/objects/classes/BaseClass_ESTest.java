/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 14:26:50 UTC 2020
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
      baseClass0.addBooleanField("", "", "");
      int int0 = 571;
      TextAreaClass.EditorType textAreaClass_EditorType0 = TextAreaClass.EditorType.WYSIWYG;
      TextAreaClass.ContentType textAreaClass_ContentType0 = TextAreaClass.ContentType.WIKI_TEXT;
      // Undeclared exception!
      baseClass0.addTextAreaField("", "customClass", 571, (-1254), textAreaClass_EditorType0, textAreaClass_ContentType0);
  }
}
