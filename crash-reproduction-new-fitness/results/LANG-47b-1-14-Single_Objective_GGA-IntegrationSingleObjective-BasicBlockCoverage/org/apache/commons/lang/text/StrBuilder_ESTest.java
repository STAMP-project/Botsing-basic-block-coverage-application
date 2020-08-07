/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 21:05:27 UTC 2020
 */

package org.apache.commons.lang.text;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.commons.lang.text.StrBuilder;
import org.apache.commons.lang.text.StrMatcher;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class StrBuilder_ESTest extends StrBuilder_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      StrBuilder strBuilder0 = new StrBuilder(0);
      strBuilder0.contains(';');
      StrBuilder strBuilder1 = strBuilder0.clear();
      strBuilder1.getNullText();
      int int0 = 0;
      StrBuilder strBuilder2 = strBuilder0.delete(0, 9);
      StrBuilder strBuilder3 = strBuilder2.appendln((long) 9);
      StrBuilder strBuilder4 = strBuilder0.replaceAll((StrMatcher) null, (String) null);
      strBuilder4.deleteAll('8');
      char char0 = 'y';
      // Undeclared exception!
      strBuilder3.appendFixedWidthPadLeft((Object) null, 2003, 'y');
  }
}
