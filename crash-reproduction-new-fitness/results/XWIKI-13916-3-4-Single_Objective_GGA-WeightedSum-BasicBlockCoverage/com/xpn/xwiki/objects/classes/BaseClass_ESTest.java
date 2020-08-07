/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 14:26:50 UTC 2020
 */

package com.xpn.xwiki.objects.classes;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import com.xpn.xwiki.XWikiContext;
import com.xpn.xwiki.objects.classes.TextAreaClass;
import com.xpn.xwiki.objects.meta.TextAreaMetaClass;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseClass_ESTest extends BaseClass_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      TextAreaMetaClass textAreaMetaClass0 = new TextAreaMetaClass();
      XWikiContext xWikiContext0 = new XWikiContext();
      XWikiContext xWikiContext1 = xWikiContext0.clone();
      XWikiContext xWikiContext2 = xWikiContext1.clone();
      XWikiContext xWikiContext3 = xWikiContext2.clone();
      XWikiContext xWikiContext4 = xWikiContext3.clone();
      textAreaMetaClass0.getCustomMappingPropertyList(xWikiContext4);
      String string0 = "Class";
      textAreaMetaClass0.addDateField("v", "v", "Class");
      int int0 = 141;
      TextAreaClass.ContentType textAreaClass_ContentType0 = TextAreaClass.ContentType.PURE_TEXT;
      // Undeclared exception!
      textAreaMetaClass0.addTextAreaField("v", "", 141, 15, textAreaClass_ContentType0);
  }
}
