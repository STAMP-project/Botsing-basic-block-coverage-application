/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 16:04:32 UTC 2021
 */

package com.xpn.xwiki.objects.classes;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import com.xpn.xwiki.objects.BaseCollection;
import com.xpn.xwiki.objects.meta.PageMetaClass;
import com.xpn.xwiki.objects.meta.PropertyMetaClass;
import java.util.HashMap;
import java.util.Map;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseClass_ESTest extends BaseClass_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      PageMetaClass pageMetaClass0 = new PageMetaClass();
      HashMap<String, PropertyMetaClass> hashMap0 = new HashMap<String, PropertyMetaClass>();
      String string0 = "validationRegExp";
      hashMap0.put("validationRegExp", pageMetaClass0);
      // Undeclared exception!
      pageMetaClass0.fromMap((Map<String, ?>) hashMap0, (BaseCollection) null);
  }
}
