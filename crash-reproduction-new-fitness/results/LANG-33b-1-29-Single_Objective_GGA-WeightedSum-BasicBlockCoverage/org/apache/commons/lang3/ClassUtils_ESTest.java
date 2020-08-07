/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 17:03:33 UTC 2020
 */

package org.apache.commons.lang3;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.List;
import org.apache.commons.lang3.ClassUtils;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class ClassUtils_ESTest extends ClassUtils_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      ClassLoader classLoader0 = null;
      boolean boolean0 = false;
      ClassUtils.getClass((ClassLoader) null, "double", false);
      String string0 = "user.country";
      ClassUtils.getPackageName("user.country");
      Class<String> class0 = String.class;
      List<Class<?>> list0 = ClassUtils.getAllInterfaces(class0);
      ClassUtils.getShortClassName(class0);
      Object[] objectArray0 = new Object[9];
      objectArray0[0] = (Object) "user.country";
      objectArray0[1] = (Object) list0;
      objectArray0[2] = (Object) class0;
      objectArray0[3] = (Object) "user";
      objectArray0[4] = (Object) "String";
      objectArray0[5] = (Object) "user.country";
      objectArray0[6] = objectArray0[5];
      objectArray0[7] = (Object) null;
      objectArray0[8] = (Object) list0;
      // Undeclared exception!
      ClassUtils.toClass(objectArray0);
  }
}
