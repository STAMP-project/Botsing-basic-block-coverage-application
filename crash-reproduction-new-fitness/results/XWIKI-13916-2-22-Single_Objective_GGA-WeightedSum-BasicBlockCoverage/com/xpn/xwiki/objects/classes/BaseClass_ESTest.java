/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 18:56:48 UTC 2020
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
      String string0 = null;
      baseClass0.addNumberField("w%JF:PB,", (String) null, 2563, "Uknown property type [{}]");
      baseClass0.addStaticListField((String) null, "Uknown property type [{}]", 2563, true, "");
      String string1 = null;
      int int0 = 0;
      baseClass0.addGroupsField("Uknown property type [{}]", (String) null, 0);
      baseClass0.hasInternalCustomMapping();
      TextAreaClass.EditorType textAreaClass_EditorType0 = TextAreaClass.EditorType.WYSIWYG;
      TextAreaClass.ContentType textAreaClass_ContentType0 = TextAreaClass.ContentType.WIKI_TEXT;
      // Undeclared exception!
      baseClass0.addTextAreaField("w%JF:PB,", "_?ZPUazY]#bu%,O0", 0, 0, textAreaClass_EditorType0, textAreaClass_ContentType0);
  }
}
