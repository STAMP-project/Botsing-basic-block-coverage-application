/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 19:16:35 UTC 2020
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
      baseClass0.toXML();
      baseClass0.addDateField("j{A_aa#xbF6M3Kc&", "", "j{A_aa#xbF6M3Kc&");
      baseClass0.toXML();
      baseClass0.addTextField("vhp(_P4D+<7QVQ0", "", 1549);
      String string0 = "IzpH]*5";
      baseClass0.getPrettyName();
      baseClass0.getXClassReference();
      baseClass0.setDefaultWeb("vhp(_P4D+<7QVQ0");
      TextAreaClass.ContentType textAreaClass_ContentType0 = TextAreaClass.ContentType.WIKI_TEXT;
      baseClass0.addTextAreaField((String) null, "sD;,CH@1", 2002, 2002, textAreaClass_ContentType0);
      baseClass0.getSetValue("IzpH]*5");
      String string1 = "P";
      baseClass0.addTemplateField("P", "IzpH]*5");
      baseClass0.setNameField((String) null);
      baseClass0.toXML(baseClass0);
      baseClass0.getPropertyList();
      // Undeclared exception!
      baseClass0.addTextAreaField("j{A_aa#xbF6M3Kc&", "", 1407, 1549, textAreaClass_ContentType0);
  }
}
