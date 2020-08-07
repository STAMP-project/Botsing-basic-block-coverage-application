/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 21:20:02 UTC 2020
 */

package org.apache.commons.lang3.text.translate;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.io.StringWriter;
import java.io.Writer;
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
      NumericEntityUnescaper numericEntityUnescaper0 = new NumericEntityUnescaper();
      StringWriter stringWriter0 = new StringWriter();
      CharSequenceTranslator[] charSequenceTranslatorArray0 = new CharSequenceTranslator[12];
      charSequenceTranslatorArray0[0] = (CharSequenceTranslator) numericEntityUnescaper0;
      charSequenceTranslatorArray0[2] = (CharSequenceTranslator) numericEntityUnescaper0;
      charSequenceTranslatorArray0[3] = (CharSequenceTranslator) numericEntityUnescaper0;
      char[] charArray0 = new char[3];
      charArray0[0] = '&';
      charArray0[2] = '&';
      CharBuffer charBuffer0 = CharBuffer.wrap(charArray0);
      stringWriter0.write("&#");
      numericEntityUnescaper0.translate((CharSequence) charBuffer0, (Writer) stringWriter0);
      stringWriter0.flush();
      CharBuffer charBuffer1 = CharBuffer.wrap(charArray0);
      numericEntityUnescaper0.translate((CharSequence) charBuffer1, (Writer) stringWriter0);
      stringWriter0.flush();
      numericEntityUnescaper0.with(charSequenceTranslatorArray0);
      StringBuffer stringBuffer0 = stringWriter0.getBuffer();
      // Undeclared exception!
      numericEntityUnescaper0.translate((CharSequence) stringBuffer0);
  }
}
