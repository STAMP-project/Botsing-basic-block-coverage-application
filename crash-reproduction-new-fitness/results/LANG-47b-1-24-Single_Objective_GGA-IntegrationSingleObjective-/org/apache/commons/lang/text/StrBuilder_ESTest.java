/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 01:32:00 UTC 2020
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
      int int0 = 1771;
      StrBuilder strBuilder0 = new StrBuilder(1771);
      StrBuilder strBuilder1 = strBuilder0.appendln(false);
      strBuilder1.append("5.0");
      // Undeclared exception!
      strBuilder0.appendFixedWidthPadLeft((Object) null, 1771, '!');
  }
}
