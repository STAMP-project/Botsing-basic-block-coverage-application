/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 20:01:29 UTC 2021
 */

package com.xpn.xwiki.objects;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import com.xpn.xwiki.objects.DBStringListProperty;
import com.xpn.xwiki.objects.classes.BaseClass;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseProperty_ESTest extends BaseProperty_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      BaseClass baseClass0 = new BaseClass();
      DBStringListProperty dBStringListProperty0 = new DBStringListProperty();
      DBStringListProperty dBStringListProperty1 = new DBStringListProperty();
      DBStringListProperty dBStringListProperty2 = new DBStringListProperty();
      DBStringListProperty dBStringListProperty3 = new DBStringListProperty();
      dBStringListProperty1.setObject(baseClass0);
      // Undeclared exception!
      dBStringListProperty1.equals((Object) null);
  }
}
