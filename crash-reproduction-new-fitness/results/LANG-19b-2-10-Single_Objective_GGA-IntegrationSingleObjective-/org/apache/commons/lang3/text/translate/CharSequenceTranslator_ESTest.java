/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 21:28:31 UTC 2020
 */

package org.apache.commons.lang3.text.translate;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.io.StringWriter;
import java.io.Writer;
import java.nio.CharBuffer;
import org.apache.commons.lang3.text.translate.CharSequenceTranslator;
import org.apache.commons.lang3.text.translate.NumericEntityEscaper;
import org.apache.commons.lang3.text.translate.NumericEntityUnescaper;
import org.apache.commons.lang3.text.translate.UnicodeEscaper;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class CharSequenceTranslator_ESTest extends CharSequenceTranslator_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      CharSequenceTranslator[] charSequenceTranslatorArray0 = new CharSequenceTranslator[10];
      NumericEntityUnescaper numericEntityUnescaper0 = new NumericEntityUnescaper();
      StringWriter stringWriter0 = new StringWriter();
      StringBuffer stringBuffer0 = stringWriter0.getBuffer();
      CharBuffer.allocate(20);
      char[] charArray0 = new char[8];
      charArray0[0] = 'v';
      charArray0[1] = 'S';
      charArray0[2] = '3';
      charArray0[3] = 'T';
      charArray0[4] = 'C';
      charArray0[5] = 'D';
      charArray0[6] = '{';
      charArray0[7] = 'O';
      stringWriter0.write(charArray0);
      NumericEntityEscaper.above((-2706));
      UnicodeEscaper.below(20);
      NumericEntityUnescaper numericEntityUnescaper1 = new NumericEntityUnescaper();
      StringBuffer stringBuffer1 = stringWriter0.getBuffer();
      StringWriter stringWriter1 = stringWriter0.append((CharSequence) stringBuffer0);
      stringWriter1.write("&#");
      // Undeclared exception!
      numericEntityUnescaper1.translate((CharSequence) stringBuffer1, (Writer) stringWriter1);
  }
}
