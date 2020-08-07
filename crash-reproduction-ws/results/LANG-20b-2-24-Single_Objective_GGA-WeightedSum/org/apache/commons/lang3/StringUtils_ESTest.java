/*
 * This file was automatically generated by EvoSuite
 * Tue Mar 31 09:56:19 UTC 2020
 */

package org.apache.commons.lang3;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.Comparator;
import java.util.Locale;
import org.apache.commons.lang3.StringUtils;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class StringUtils_ESTest extends StringUtils_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Comparator<Locale.LanguageRange> comparator0 = (Comparator<Locale.LanguageRange>) mock(Comparator.class, new ViolatedAssumptionAnswer());
      doReturn((String) null).when(comparator0).toString();
      Object[] objectArray0 = new Object[6];
      objectArray0[0] = (Object) comparator0;
      // Undeclared exception!
      StringUtils.join(objectArray0, (String) null);
  }
}
