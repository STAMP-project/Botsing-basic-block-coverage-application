/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 20:45:17 UTC 2020
 */

package org.apache.commons.lang3.text.translate;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.io.StringWriter;
import java.nio.CharBuffer;
import org.apache.commons.lang3.text.translate.CharSequenceTranslator;
import org.apache.commons.lang3.text.translate.NumericEntityUnescaper;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class NumericEntityUnescaper_ESTest extends NumericEntityUnescaper_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      CharSequenceTranslator[] charSequenceTranslatorArray0 = new CharSequenceTranslator[0];
      StringWriter stringWriter0 = new StringWriter();
      char[] charArray0 = new char[7];
      charArray0[0] = '#';
      charArray0[1] = '&';
      charArray0[2] = '&';
      charArray0[3] = '#';
      charArray0[4] = '&';
      charArray0[5] = '&';
      stringWriter0.write(charArray0);
      stringWriter0.close();
      char[] charArray1 = new char[9];
      charArray1[1] = 'X';
      charArray1[2] = '&';
      stringWriter0.close();
      charArray1[3] = '#';
      charArray1[4] = 'X';
      charArray1[5] = '&';
      charArray1[0] = 'X';
      charArray1[7] = '#';
      stringWriter0.write("");
      CharBuffer.wrap(charArray1);
      NumericEntityUnescaper numericEntityUnescaper0 = new NumericEntityUnescaper();
      StringBuffer stringBuffer0 = stringWriter0.getBuffer();
      // Undeclared exception!
      numericEntityUnescaper0.translate((CharSequence) stringBuffer0);
  }
}
