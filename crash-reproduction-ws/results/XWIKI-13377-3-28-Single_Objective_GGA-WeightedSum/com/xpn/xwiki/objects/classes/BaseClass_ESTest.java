/*
 * This file was automatically generated by EvoSuite
 * Mon Mar 30 20:37:48 UTC 2020
 */

package com.xpn.xwiki.objects.classes;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import com.xpn.xwiki.objects.BaseCollection;
import com.xpn.xwiki.objects.classes.BaseClass;
import com.xpn.xwiki.objects.meta.PasswordMetaClass;
import com.xpn.xwiki.objects.meta.StringMetaClass;
import java.util.Map;
import java.util.concurrent.ConcurrentSkipListMap;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseClass_ESTest extends BaseClass_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      BaseClass baseClass0 = new BaseClass();
      StringMetaClass stringMetaClass0 = new StringMetaClass();
      ConcurrentSkipListMap<String, PasswordMetaClass> concurrentSkipListMap0 = new ConcurrentSkipListMap<String, PasswordMetaClass>();
      PasswordMetaClass passwordMetaClass0 = new PasswordMetaClass();
      concurrentSkipListMap0.put("prettyName", passwordMetaClass0);
      // Undeclared exception!
      stringMetaClass0.fromMap((Map<String, ?>) concurrentSkipListMap0, (BaseCollection) baseClass0);
  }
}
