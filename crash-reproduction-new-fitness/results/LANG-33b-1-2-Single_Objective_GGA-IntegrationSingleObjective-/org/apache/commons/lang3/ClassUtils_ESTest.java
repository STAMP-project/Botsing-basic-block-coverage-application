/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 12:38:57 UTC 2020
 */

package org.apache.commons.lang3;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.lang.reflect.Array;
import java.util.LinkedList;
import org.apache.commons.lang3.ClassUtils;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class ClassUtils_ESTest extends ClassUtils_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Class<Object>[] classArray0 = (Class<Object>[]) Array.newInstance(Class.class, 6);
      Class<Object> class0 = Object.class;
      classArray0[0] = class0;
      Class<Object> class1 = Object.class;
      classArray0[1] = class1;
      Class<Object> class2 = Object.class;
      classArray0[2] = class2;
      Class<Object> class3 = Object.class;
      classArray0[3] = class3;
      Class<Object> class4 = Object.class;
      classArray0[4] = class4;
      Class<Object> class5 = Object.class;
      classArray0[5] = class5;
      Class<?>[] classArray1 = ClassUtils.wrappersToPrimitives(classArray0);
      Byte byte0 = new Byte((byte)19);
      ClassUtils.getShortClassName((Object) byte0, (String) null);
      LinkedList<Class<Short>> linkedList0 = new LinkedList<Class<Short>>();
      // Undeclared exception!
      ClassUtils.toClass(classArray1);
  }
}
