/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 17:03:36 UTC 2020
 */

package org.apache.commons.lang3;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.lang.reflect.Array;
import java.util.List;
import org.apache.commons.lang3.ClassUtils;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class ClassUtils_ESTest extends ClassUtils_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      ClassUtils.convertClassNamesToClasses((List<String>) null);
      ClassUtils classUtils0 = new ClassUtils();
      Class<Object>[] classArray0 = (Class<Object>[]) Array.newInstance(Class.class, 1);
      Class<Object> class0 = Object.class;
      classArray0[0] = class0;
      ClassUtils.isAssignable(classArray0, classArray0);
      Class<?>[] classArray1 = ClassUtils.wrappersToPrimitives(classArray0);
      ClassUtils classUtils1 = new ClassUtils();
      ClassUtils.getShortCanonicalName("\"uonUW-L0PlBY=Bh\"TP");
      ClassUtils.getShortClassName(class0);
      ClassUtils.isAssignable(classArray0, classArray1, true);
      // Undeclared exception!
      ClassUtils.toClass(classArray1);
  }
}
