/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 12:37:48 UTC 2020
 */

package org.apache.commons.lang3;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.lang3.ClassUtils;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class ClassUtils_ESTest extends ClassUtils_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      String string0 = "5.1";
      ClassUtils.getPackageCanonicalName("5.1");
      ClassUtils.getShortClassName((Object) null, (String) null);
      ClassUtils.getShortCanonicalName((Object) null, (String) null);
      Object[] objectArray0 = new Object[5];
      objectArray0[0] = (Object) null;
      objectArray0[1] = (Object) "5";
      objectArray0[2] = (Object) "5";
      objectArray0[3] = (Object) null;
      objectArray0[4] = (Object) null;
      // Undeclared exception!
      ClassUtils.toClass(objectArray0);
  }
}
