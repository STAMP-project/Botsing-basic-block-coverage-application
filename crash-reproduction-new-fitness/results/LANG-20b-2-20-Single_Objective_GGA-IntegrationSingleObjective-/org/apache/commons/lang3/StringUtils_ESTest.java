/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:54:55 UTC 2020
 */

package org.apache.commons.lang3;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.Comparator;
import java.util.LinkedList;
import org.apache.commons.lang3.StringUtils;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class StringUtils_ESTest extends StringUtils_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      LinkedList<String> linkedList0 = new LinkedList<String>();
      Comparator<Object> comparator0 = (Comparator<Object>) mock(Comparator.class, new ViolatedAssumptionAnswer());
      doReturn((String) null).when(comparator0).toString();
      linkedList0.sort(comparator0);
      Object[] objectArray0 = new Object[5];
      objectArray0[0] = (Object) comparator0;
      linkedList0.add("8DO-(;ZX>6^_,u'=>");
      objectArray0[1] = (Object) null;
      objectArray0[2] = (Object) linkedList0;
      objectArray0[3] = (Object) linkedList0;
      objectArray0[4] = (Object) comparator0;
      // Undeclared exception!
      StringUtils.join(objectArray0, (String) null);
  }
}
