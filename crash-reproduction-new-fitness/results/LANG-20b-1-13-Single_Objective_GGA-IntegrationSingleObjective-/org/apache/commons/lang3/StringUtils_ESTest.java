/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 21:32:07 UTC 2020
 */

package org.apache.commons.lang3;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.Comparator;
import org.apache.commons.lang3.StringUtils;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class StringUtils_ESTest extends StringUtils_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Comparator<Object> comparator0 = (Comparator<Object>) mock(Comparator.class, new ViolatedAssumptionAnswer());
      doReturn((String) null).when(comparator0).toString();
      Object object0 = new Object();
      Object[] objectArray0 = new Object[8];
      objectArray0[0] = (Object) comparator0;
      objectArray0[1] = (Object) comparator0;
      objectArray0[2] = (Object) "java.text.Normalizer";
      objectArray0[3] = (Object) "9| \"DBhW9!V";
      objectArray0[4] = objectArray0[1];
      objectArray0[5] = (Object) "9| \"DBhW9!V";
      objectArray0[6] = (Object) "java.text.Normalizer";
      objectArray0[7] = (Object) "SQEj|^9A1l$_8N>?\"w";
      // Undeclared exception!
      StringUtils.join(objectArray0, "9| \"DBhW9!V", 0, Integer.MAX_VALUE);
  }
}
