/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 17:03:05 UTC 2020
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
      Class<Byte> class0 = Byte.class;
      List<Class<?>> list0 = ClassUtils.getAllSuperclasses(class0);
      ClassUtils.isAssignable(class0, class0, true);
      ClassUtils.getShortCanonicalName("java.awt.headless");
      Object[] objectArray0 = new Object[3];
      Object object0 = new Object();
      objectArray0[0] = object0;
      objectArray0[2] = (Object) list0;
      // Undeclared exception!
      ClassUtils.toClass(objectArray0);
  }
}
