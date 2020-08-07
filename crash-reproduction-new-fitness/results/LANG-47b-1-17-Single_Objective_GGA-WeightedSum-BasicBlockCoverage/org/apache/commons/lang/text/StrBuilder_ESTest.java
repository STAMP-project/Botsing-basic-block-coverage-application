/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 00:52:59 UTC 2020
 */

package org.apache.commons.lang.text;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.Collection;
import org.apache.commons.lang.text.StrBuilder;
import org.apache.commons.lang.text.StrMatcher;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class StrBuilder_ESTest extends StrBuilder_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      StrBuilder strBuilder0 = new StrBuilder(2208);
      strBuilder0.getNullText();
      StrMatcher strMatcher0 = mock(StrMatcher.class, new ViolatedAssumptionAnswer());
      strBuilder0.lastIndexOf(strMatcher0);
      strBuilder0.toStringBuffer();
      StrBuilder strBuilder1 = strBuilder0.appendAll((Collection) null);
      StrBuilder strBuilder2 = strBuilder1.reverse();
      StrBuilder strBuilder3 = new StrBuilder();
      // Undeclared exception!
      strBuilder2.appendFixedWidthPadLeft((Object) null, 2208, '$');
  }
}
