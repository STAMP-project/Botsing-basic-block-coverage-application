/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 14:30:27 UTC 2020
 */

package com.xpn.xwiki.objects.classes;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import com.xpn.xwiki.objects.classes.TextAreaClass;
import com.xpn.xwiki.objects.meta.StringMetaClass;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseClass_ESTest extends BaseClass_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      StringMetaClass stringMetaClass0 = new StringMetaClass();
      stringMetaClass0.addStaticListField("s%CuT", "", 0, true, "hS94-;=:szF%uIgXNK", "ax");
      TextAreaClass.EditorType textAreaClass_EditorType0 = TextAreaClass.EditorType.TEXT;
      TextAreaClass.ContentType textAreaClass_ContentType0 = TextAreaClass.ContentType.PURE_TEXT;
      // Undeclared exception!
      stringMetaClass0.addTextAreaField("s%CuT", "ax", 677, 25, textAreaClass_EditorType0, textAreaClass_ContentType0);
  }
}
