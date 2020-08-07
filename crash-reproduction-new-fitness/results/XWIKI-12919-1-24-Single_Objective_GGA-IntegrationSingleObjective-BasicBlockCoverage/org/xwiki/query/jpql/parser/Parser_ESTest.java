/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 15:17:04 UTC 2020
 */

package org.xwiki.query.jpql.parser;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.io.PushbackReader;
import java.io.StringReader;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;
import org.xwiki.query.jpql.lexer.Lexer;
import org.xwiki.query.jpql.parser.Parser;
import org.xwiki.query.jpql.parser.ParserException;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class Parser_ESTest extends Parser_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Lexer lexer0 = mock(Lexer.class, new ViolatedAssumptionAnswer());
      Parser parser0 = new Parser(lexer0);
      StringReader stringReader0 = new StringReader("Not a child.");
      PushbackReader pushbackReader0 = new PushbackReader(stringReader0);
      PushbackReader pushbackReader1 = new PushbackReader(pushbackReader0, 10);
      Lexer lexer1 = new Lexer(pushbackReader1);
      Parser parser1 = new Parser(lexer1);
      try { 
        parser1.parse();
        fail("Expecting exception: ParserException");
      
      } catch(ParserException e) {
         //
         // [1,1] expecting: select
         //
         verifyException("org.xwiki.query.jpql.parser.Parser", e);
      }
  }
}
