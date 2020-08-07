/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 10:50:10 UTC 2020
 */

package org.xwiki.query.jpql.parser;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.io.PushbackReader;
import java.io.StringReader;
import java.util.ArrayList;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;
import org.xwiki.query.jpql.lexer.Lexer;
import org.xwiki.query.jpql.node.AConditionalPrimary;
import org.xwiki.query.jpql.node.ASimpleCondExpression;
import org.xwiki.query.jpql.parser.Parser;
import org.xwiki.query.jpql.parser.ParserException;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class Parser_ESTest extends Parser_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      ArrayList<AConditionalPrimary> arrayList0 = new ArrayList<AConditionalPrimary>();
      ArrayList<ASimpleCondExpression> arrayList1 = new ArrayList<ASimpleCondExpression>();
      StringReader stringReader0 = new StringReader("7eH5c<Rp?gH*0?(y}R");
      PushbackReader pushbackReader0 = new PushbackReader(stringReader0, 424);
      PushbackReader pushbackReader1 = new PushbackReader(pushbackReader0);
      Lexer lexer0 = new Lexer(pushbackReader1);
      char[] charArray0 = new char[0];
      pushbackReader0.read(charArray0);
      lexer0.next();
      char[] charArray1 = new char[6];
      Parser parser0 = new Parser(lexer0);
      try { 
        parser0.parse();
        fail("Expecting exception: ParserException");
      
      } catch(ParserException e) {
         //
         // [1,2] expecting: select
         //
         verifyException("org.xwiki.query.jpql.parser.Parser", e);
      }
  }
}
