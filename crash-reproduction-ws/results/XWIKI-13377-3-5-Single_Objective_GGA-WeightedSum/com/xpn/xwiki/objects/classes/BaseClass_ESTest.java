/*
 * This file was automatically generated by EvoSuite
 * Mon Mar 30 19:38:35 UTC 2020
 */

package com.xpn.xwiki.objects.classes;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import com.xpn.xwiki.objects.BaseCollection;
import com.xpn.xwiki.objects.BaseObject;
import com.xpn.xwiki.objects.meta.PageMetaClass;
import java.util.Map;
import java.util.concurrent.ConcurrentSkipListMap;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseClass_ESTest extends BaseClass_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      PageMetaClass pageMetaClass0 = new PageMetaClass();
      BaseObject baseObject0 = new BaseObject();
      ConcurrentSkipListMap<String, Object> concurrentSkipListMap0 = new ConcurrentSkipListMap<String, Object>();
      BaseCollection baseCollection0 = pageMetaClass0.fromMap((Map<String, ?>) concurrentSkipListMap0, (BaseCollection) baseObject0);
      concurrentSkipListMap0.put("~89fMF", baseCollection0);
      pageMetaClass0.addTimezoneField("~89fMF", "~89fMF", 3);
      // Undeclared exception!
      pageMetaClass0.fromMap((Map<String, ?>) concurrentSkipListMap0, (BaseCollection) baseObject0);
  }
}
