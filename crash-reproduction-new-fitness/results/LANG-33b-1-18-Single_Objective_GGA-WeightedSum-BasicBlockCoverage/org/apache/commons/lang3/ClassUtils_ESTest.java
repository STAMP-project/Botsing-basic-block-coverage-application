/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 17:02:56 UTC 2020
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
      Class<Character> class0 = Character.class;
      ClassUtils.getPackageName(class0);
      String string0 = null;
      ClassUtils.getShortCanonicalName((Object) "java.lang", (String) null);
      Object[] objectArray0 = new Object[1];
      objectArray0[0] = (Object) null;
      // Undeclared exception!
      ClassUtils.toClass(objectArray0);
  }
}
