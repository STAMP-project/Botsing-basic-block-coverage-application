/*
 * This file was automatically generated by EvoSuite
 * Fri Jan 17 21:25:58 UTC 2020
 */

package org.apache.commons.lang3.text.translate;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.io.StringWriter;
import java.nio.CharBuffer;
import org.apache.commons.lang3.text.translate.NumericEntityUnescaper;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class NumericEntityUnescaper_ESTest extends NumericEntityUnescaper_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      NumericEntityUnescaper numericEntityUnescaper0 = new NumericEntityUnescaper();
      StringWriter stringWriter0 = new StringWriter(4131);
      char[] charArray0 = new char[4];
      charArray0[2] = '&';
      charArray0[3] = '#';
      CharBuffer charBuffer0 = CharBuffer.wrap(charArray0);
      StringWriter stringWriter1 = stringWriter0.append((CharSequence) charBuffer0);
      StringWriter stringWriter2 = stringWriter1.append('#');
      StringBuffer stringBuffer0 = stringWriter2.getBuffer();
      // Undeclared exception!
      numericEntityUnescaper0.translate((CharSequence) stringBuffer0);
  }
}
