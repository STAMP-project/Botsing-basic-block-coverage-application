/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:28:04 UTC 2020
 */

package org.apache.commons.lang.text;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.lang.text.StrBuilder;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class StrBuilder_ESTest extends StrBuilder_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      StrBuilder strBuilder0 = new StrBuilder();
      StrBuilder strBuilder1 = new StrBuilder();
      Object[] objectArray0 = new Object[4];
      objectArray0[0] = (Object) strBuilder0;
      objectArray0[1] = (Object) "startIndex must be valid";
      objectArray0[2] = (Object) strBuilder1;
      objectArray0[3] = (Object) strBuilder0;
      StrBuilder strBuilder2 = strBuilder1.appendWithSeparators(objectArray0, "startIndex must be valid");
      strBuilder0.getNullText();
      StrBuilder strBuilder3 = new StrBuilder((-2463));
      // Undeclared exception!
      strBuilder2.appendFixedWidthPadLeft((Object) null, 112, '{');
  }
}
