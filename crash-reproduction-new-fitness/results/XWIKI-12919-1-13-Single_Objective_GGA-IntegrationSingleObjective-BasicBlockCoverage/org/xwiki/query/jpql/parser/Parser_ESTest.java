/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 10:53:06 UTC 2020
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
import org.xwiki.query.jpql.node.ASingleSelectList;
import org.xwiki.query.jpql.node.AStartSpec;
import org.xwiki.query.jpql.parser.Parser;
import org.xwiki.query.jpql.parser.ParserException;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class Parser_ESTest extends Parser_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      ArrayList<AStartSpec> arrayList0 = new ArrayList<AStartSpec>();
      ArrayList<ASingleSelectList> arrayList1 = new ArrayList<ASingleSelectList>();
      StringReader stringReader0 = new StringReader("b|+Xg?>7`");
      PushbackReader pushbackReader0 = new PushbackReader(stringReader0, 600);
      Lexer lexer0 = new Lexer(pushbackReader0);
      Parser parser0 = new Parser(lexer0);
      stringReader0.ready();
      Parser parser1 = new Parser((Lexer) null);
      try { 
        parser0.parse();
        fail("Expecting exception: ParserException");
      
      } catch(ParserException e) {
         //
         // [1,1] expecting: select
         //
         verifyException("org.xwiki.query.jpql.parser.Parser", e);
      }
  }
}
