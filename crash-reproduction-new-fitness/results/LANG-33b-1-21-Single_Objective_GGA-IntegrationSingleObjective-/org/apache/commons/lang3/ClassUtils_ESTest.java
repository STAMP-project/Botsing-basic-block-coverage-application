/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 17:02:30 UTC 2020
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
      Class<String> class0 = String.class;
      ClassUtils.wrapperToPrimitive(class0);
      Object[] objectArray0 = new Object[7];
      objectArray0[0] = (Object) null;
      objectArray0[1] = (Object) class0;
      objectArray0[2] = (Object) null;
      objectArray0[3] = objectArray0[2];
      objectArray0[4] = objectArray0[3];
      objectArray0[5] = (Object) class0;
      objectArray0[6] = (Object) class0;
      // Undeclared exception!
      ClassUtils.toClass(objectArray0);
  }
}
