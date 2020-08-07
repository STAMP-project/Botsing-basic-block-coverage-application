/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 17:03:28 UTC 2020
 */

package org.apache.commons.lang3;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.lang.reflect.Array;
import org.apache.commons.lang3.ClassUtils;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class ClassUtils_ESTest extends ClassUtils_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      ClassLoader classLoader0 = mock(ClassLoader.class, new ViolatedAssumptionAnswer());
      Class<String> class0 = String.class;
      ClassUtils.isInnerClass(class0);
      Class<Short>[] classArray0 = (Class<Short>[]) Array.newInstance(Class.class, 8);
      Class<Short> class1 = Short.class;
      classArray0[0] = class1;
      Class<Short> class2 = Short.class;
      classArray0[1] = class2;
      Class<Short> class3 = Short.class;
      classArray0[2] = class3;
      Class<Short> class4 = Short.class;
      classArray0[3] = class4;
      Class<Short> class5 = Short.class;
      classArray0[4] = class5;
      Class<Short> class6 = Short.class;
      classArray0[5] = class6;
      Class<Short> class7 = Short.class;
      classArray0[6] = class7;
      Class<Short> class8 = Short.class;
      classArray0[7] = class8;
      ClassUtils.isAssignable(classArray0, classArray0);
      Object[] objectArray0 = new Object[5];
      objectArray0[0] = (Object) classLoader0;
      objectArray0[2] = (Object) classLoader0;
      objectArray0[3] = (Object) classLoader0;
      objectArray0[4] = (Object) classLoader0;
      // Undeclared exception!
      ClassUtils.toClass(objectArray0);
  }
}
