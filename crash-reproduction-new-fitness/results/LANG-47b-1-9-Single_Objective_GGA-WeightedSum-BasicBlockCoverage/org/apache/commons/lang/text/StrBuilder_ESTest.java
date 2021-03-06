/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 20:26:28 UTC 2020
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
      char char0 = 'O';
      strBuilder0.contains('O');
      StrBuilder strBuilder1 = strBuilder0.deleteFirst("");
      strBuilder1.midString(0, 0);
      strBuilder1.toCharArray();
      char char1 = '\\';
      // Undeclared exception!
      strBuilder1.appendFixedWidthPadLeft((Object) null, 5, '\\');
  }
}
